{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.ODBC.DBConn
Description : Contains methods for running sql against databases
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Generic methods for running sql / comparing databases / printing and logging.
-}
module HSql.ODBC.DBConn (
  module HSql.ODBC.DBConn,
  module HSql.ODBC.GConn,
) where

import BaseUtils.Extra
import Control.Arrow
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Kind
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import qualified Data.List.NonEmpty.Extra as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord (Down (..), comparing)
import Data.Semigroup.Foldable (intercalateMap1)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import Data.Text.Lazy.Builder (fromText)
import Data.These
import Data.Time
import Data.UUID (UUID)
import Data.Vinyl
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import Data.Vinyl.TypeLevel as VT (RecAll)
import Data.Void
import qualified Database.HDBC as H
import qualified Database.HDBC.ODBC as H
import DocUtils.Condition
import DocUtils.Divvy
import DocUtils.Doc
import qualified DocUtils.Generics as GU
import Formatting (int, sformat, (%), (%.))
import qualified Formatting as F
import GHC.Generics (Generic)
import GHC.Stack
import GHC.TypeLits
import HSql.Core as Sql
import HSql.ODBC.GConn
import qualified Language.Haskell.TH as TH (Name)
import Logging
import qualified Prettyprinter as PP
import qualified Replace.Megaparsec as RM
import qualified Text.Megaparsec as Z
import Text.Shakespeare.Text
import qualified UnliftIO.Exception as U
import Utils.Error
import qualified Utils.TypeLevel as TP (FailUnless, Impl)

-- | wrapper for a hdbc connection
newtype HConn a = HConn H.Connection deriving newtype (H.IConnection)

-- | 'SqlPairRW' contains name of a table and the sql needed to create that table
type SqlPairRW db dbw a b = GConnWrite dbw => (Table dbw, Maybe (Sql db a b))

-- | creates a pair containing the table and sql
mkSqlPair ::
  GConnWrite dbw =>
  Table dbw ->
  Sql db a b ->
  SqlPairRW db dbw a b
mkSqlPair table sql = (table, Just sql)

-- | 'TVTable' distinguishes between tables and views
data TVTable = TVTable | TVView deriving stock (Show, Eq)

-- | predicate on 'TVTable'
isTable :: TVTable -> Bool
isTable TVTable = True
isTable TVView = False

-- | create table options
data TableCreate = DropTableCreate | TableCreate | SkipTableCreate deriving stock (Show, Eq)

instance ToText TableCreate where
  toText = fromText . T.pack . show

-- | checks to see that we are not trying to write to a readonly database
type RunSqlOk :: [Type] -> db -> Constraint
type RunSqlOk b db =
  TP.FailUnless
    (TP.Impl (Sql.WriteableRS b) (Sql.WriteableDB db))
    ( 'Text "Readonly database does not allow Upd")

-- | a list of constraints for that need to be fulfilled by runSql*
type TSql b a =
  ( VT.RecAll Sql.RState b Sql.SingleZ
  , V.ReifyConstraint Show V.Identity a
  , V.RecordToList a
  , V.RMap a
  , Sql.ValidateNested b
  )

-- | validates the dsl and checks if the dsl has the appropriate read/write access
type RunSqlOkT :: [Type] -> [Type] -> db -> Constraint
type RunSqlOkT a b db = (TSql b a, RunSqlOk b db)

-- | 'runSqlTime'' measures and returns the time taken to run the sql
runSqlTime ::
  forall b a c e m db ann.
  ( ML e m
  , GConn db
  , RunSqlOkT a b db
  , Sql.PGen b
  , Show c
  ) =>
  (Sql.SingleOuts b -> c) ->
  db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (PP.Doc ann)
runSqlTime fn db rs = fmap snd . runSqlTime' fn db rs

-- | 'runSqlTime' measures and returns the time taken to run the sql
runSqlTime' ::
  forall b a c e m db ann.
  ( ML e m
  , GConn db
  , RunSqlOkT a b db
  , Sql.PGen b
  , Show c
  ) =>
  (Sql.SingleOuts b -> c) ->
  db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Sql.SingleOuts b, PP.Doc ann)
runSqlTime' fn db rs sql = do
  ((org, c), difftm) <- timeCommandDiff (sDescription sql) $ do
    ret <- Sql.ext <$> runSqlUnsafe db rs sql
    return (ret, fn ret)
  $logInfo [st|runSqlTime' #{sDescription sql} difftm=#{difftm} c=#{psiT c}|]
  return (org, dumpInfo c difftm sql)

-- | pretty print time taken, sql descripion and a title
dumpInfo ::
  Show c =>
  c ->
  Text ->
  Sql db a b ->
  PP.Doc ann
dumpInfo c difftm sql =
  PP.pretty difftm
    PP.<+> PP.pretty (sDescription sql)
    PP.<+> PP.viaShow c

-- | 'runSqlITime' measures and returns the time taken to run the sql
runSqlITime ::
  forall b a c e m db ann.
  ( ML e m
  , RunSqlOkT a b db
  , Sql.PGen b
  , Show c
  ) =>
  (Sql.SingleOuts b -> c) ->
  HConn db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (PP.Doc ann)
runSqlITime fn db rs = fmap snd . runSqlITime' fn db rs

-- | 'runSqlITime'' measures and returns the time taken to run the sql
runSqlITime' ::
  forall b a c e m db ann.
  ( ML e m
  , RunSqlOkT a b db
  , Sql.PGen b
  , Show c
  ) =>
  (Sql.SingleOuts b -> c) ->
  HConn db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Sql.SingleOuts b, PP.Doc ann)
runSqlITime' fn db rs sql = do
  ((org, c), difftm) <- timeCommandDiff (sDescription sql) $ do
    ret <- Sql.ext <$> runSqlI db rs sql
    return (ret, fn ret)
  $logInfo [st|runSqlITime' #{sDescription sql} #{difftm} #{psiT c}|]
  return (org, dumpInfo c difftm sql)

-- | 'runSqlE' convenience method that runs 'Sql.ext'
runSqlE ::
  forall b a e m db.
  ( ML e m
  , GConn db
  , RunSqlOkT a b db
  , Sql.PGen b
  ) =>
  db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Sql.SingleOuts b)
runSqlE db rs sql =
  Sql.ext <$> runSqlUnsafe db rs sql

-- | 'runSqlE' convenience method that runs 'Sql.ext'
runSqlIE ::
  forall b a e m db.
  ( ML e m
  , RunSqlOkT a b db
  , Sql.PGen b
  ) =>
  HConn db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Sql.SingleOuts b)
runSqlIE db rs sql =
  Sql.ext <$> runSqlI db rs sql

-- | 'runSql' executes the sql for typed input and output
runSql ::
  forall b a e m db.
  ( ML e m
  , GConn db
  , RunSqlOkT a b db
  ) =>
  db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Rec Sql.RState b)
runSql = runSqlUnsafe

-- | 'runSql_' is the same as 'runSql' but throws away the output
runSql_ ::
  forall b a e m db.
  ( ML e m
  , GConn db
  , RunSqlOkT a b db
  ) =>
  db ->
  Rec V.Identity a ->
  Sql db a b ->
  m ()
runSql_ a b c = void $ runSqlUnsafe a b c

-- order is important for using @ b then a

-- | 'runSqlReadOnly' runs a typed query but explicitly requires the sql to be a non-update
runSqlReadOnly ::
  ( TSql b a
  , Sql.WriteableRS b ~ 'False
  , ML e m
  , GConn db
  ) =>
  db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Rec Sql.RState b)
runSqlReadOnly = runSqlUnsafe

-- | 'runSqlUnsafe' runs a typed query but doesnt check RunSqlOk
runSqlUnsafe ::
  ( TSql b a
  , ML e m
  , GConn db
  ) =>
  db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Rec Sql.RState b)
runSqlUnsafe db vals sql = do
  let hs = Sql.encodeVals (sEncoders sql) vals -- todo: has an extra encode!
  withDB (sSql sql <> "\n" <> psiT hs) db $ \conn -> runSqlI conn vals sql

-- | 'runSqlI' is like 'runSql' but reuses the connection from 'withDB'
runSqlI ::
  ( TSql b a
  , ML e m
  ) =>
  HConn db ->
  Rec V.Identity a ->
  Sql db a b ->
  m (Rec Sql.RState b)
runSqlI conn vals (Sql desc enc dec sql) = do
  let hs = Sql.encodeVals enc vals
  $logDebug [st|runSqlI: #{desc}|]
  rrs <- runSqlRawI conn hs sql
  case Sql.processRetCol dec rrs of
    Right z -> return z
    Left es -> do
      logSqlHandlerExceptions es
      U.throwIO $
        GBException
          [st|runSqlI #{desc}

{{BEGIN sqlhandler #{length es} errors}}
#{psiT es}
{{END sqlhandler #{length es} errors}}

{{BEGIN input values}}
#{psiT vals}
{{END input values}}

{{BEGIN sql}}
#{psiT sql}
{{END sql}}
|]

-- | log any sql errors to stderr
logSqlHandlerExceptions ::
  ML e m =>
  Sql.SE ->
  m ()
logSqlHandlerExceptions es = do
  let len = length es
  forM_ (N.zip (N.iterate (+ 1) (1 :: Int)) es) $ \(i, e) -> werrLS [st|#{i} of #{len}: #{Sql.seShortMessage e}|]

-- | 'runSqlRaw' runs an untyped query with metadata
runSqlRaw ::
  (ML e m, GConn db) =>
  db ->
  [SqlValue] ->
  Text ->
  m [Sql.ResultSet]
runSqlRaw db hs sql = withDB (sql <> "\n" <> psiT hs) db $ \conn -> runSqlRawI conn hs sql

-- | 'runSqlRawI' runs an untyped query with metadata using an existing connection using 'withDB'
runSqlRawI ::
  ML e m =>
  HConn db ->
  [SqlValue] ->
  Text ->
  m [Sql.ResultSet]
runSqlRawI conn hs sql = do
  $logDebug [st|runSqlRawI: encoded hs=#{psiT hs}|]
  runSqlImpl "runSqlRawI" conn hs (T.unpack sql)

-- | 'runSqlImpl' runs an untyped query where you pass in a callback to pull out the values you want
runSqlImpl ::
  (ML e m, H.IConnection b) =>
  String ->
  b ->
  [SqlValue] ->
  String ->
  m [Either Int ([H.SqlColDesc], [[SqlValue]])]
runSqlImpl desc conn ps sql = do
  $logDebug [st|runSqlImpl: running #{desc} sql=#{newline}#{sql}|]
  U.bracket (liftIO $ H.prepare conn sql) (liftIO . H.finish) $ \stmt -> do
    rc <- liftIO $ H.execute stmt ps
    let go _ Nothing = return []
        go !i (Just (Right meta)) = do
          $logDebug [st|runSqlImpl: Result set! i=#{i}|]
          rs <- liftIO $ H.fetchAllRows' stmt
          --                        $logDebug [st|runSqlImpl fetchAllRows' #{psiT r}|]
          zz <- liftIO $ H.nextResultSet stmt
          --                        $logDebug [st|runSqlImpl nextResultSet' #{psiT zz}|]
          yy <- go (i + 1) zz
          return $ Right (meta, rs) : yy
        go !i (Just (Left n)) = do
          $logDebug [st|runSqlImpl: Update statement! ie rc==#{n} i=#{i}|]
          zz <- liftIO $ H.nextResultSet stmt
          --                        $logDebug [st|runSqlImpl nextResultSet' #{psiT zz}|]
          yy <- go (i + 1) zz
          return $ Left n : yy
    go (0 :: Int) (Just rc)

-- | 'runRawCol' just returns the metadata for the first resultset and ignores the rest used by TH
runRawCol ::
  (ML e m, GConn db) =>
  db ->
  [SqlValue] ->
  Text ->
  m [Sql.RMeta]
runRawCol db hs sql = withDB (sql <> "\n" <> psiT hs) db $ \conn -> do
  $logDebug [st|runRawCol:encoded hs=#{psiT hs}|]
  runSqlMetaImpl True "runRawCol" conn hs (T.unpack sql)

-- | 'runSqlMetaImpl' returns the metadata
runSqlMetaImpl ::
  (ML e m, H.IConnection b) =>
  Bool ->
  String ->
  b ->
  [SqlValue] ->
  String ->
  m [Sql.RMeta]
runSqlMetaImpl domany desc conn ps sql = do
  $logDebug [st|runSqlMetaImpl: running #{desc} sql=#{newline}#{sql}|]
  U.bracket (liftIO $ H.prepare conn sql) (liftIO . H.finish) $ \stmt -> do
    rc <- liftIO $ H.execute stmt ps
    let go _ Nothing = return []
        go !i (Just (Right meta)) = do
          $logDebug [st|runSqlMetaImpl: Result set! i=#{i}|]
          if domany
            then do
              zz <- liftIO $ H.nextResultSet stmt
              yy <- go (i + 1) zz
              return (meta : yy)
            else return [meta]
        go !i (Just (Left n)) =
          U.throwIO $ GBException [st|runSqlMetaImpl: Update not allowed! rc==#{n} i=#{i}|]
    go (0 :: Int) (Just rc)

-- dont need to unwrap HConn cos made it an instance of H.IConnection so it is isomorphic

-- | 'withDB' opens a database connection and allows the caller to run commands using the connection and closes the connection at the end
withDB ::
  (ML e m, GConn a) =>
  Text ->
  a ->
  (HConn a -> m b) ->
  m b
withDB sqltxt db fn =
  bracketDB sqltxt db $ \canyx ->
    withTransaction canyx $ \conn -> fn conn

-- | 'bracketDB' handles closing the connection. If there are errors then they are logged
bracketDB ::
  (ML e m, GConn a) =>
  Text ->
  a ->
  (HConn a -> m c) ->
  m c
bracketDB sqltxt db fn = do
  let ma =
        U.bracket
          (HConn <$> liftIO (getConn db))
          ( \c ->
              U.handle
                ( \(e :: H.SqlError) ->
                    if ignoreDisconnectError (Just db)
                      then wwarnLS [st|bracketDB: #{psiT e} db=#{showDb db}|]
                      else werrLS [st|bracketDB: #{psiT e} db=#{showDb db}|]
                )
                (liftIO (H.disconnect c))
          )
          fn
  lr <- U.tryAny ma
  case lr of
    Left (e :: U.SomeException) -> do
      -- H.SqlError
      werrLS [st|bracketDB: #{psiT e} db=#{showDb db}|]
      U.throwIO $ GBWrapException [st|bracketDB(1): !XYZZY!: sqltxt=#{sqltxt}|] e
    Right r -> return r

-- | tracks the number of rows and metadata for a table
data TableCount a = TableCount
  { tcTable :: !(Table a)
  , tcRows :: !Int
  , tcCreated :: !(Maybe UTCTime)
  , tcUpdated :: !(Maybe UTCTime)
  }
  deriving stock (Ord, Eq, Show)

-- | dump out counts for all tables
allTablesCount' :: forall a m e. (GConn a, ML e m) => a -> m [TableCount a]
allTablesCount' = allTablesCount (const True)

-- | 'allTablesCount' returns a list of table plus number of rows filtered by a predicate
allTablesCount ::
  (GConn a, ML e m) =>
  (Table a -> Bool) ->
  a ->
  m [TableCount a]
allTablesCount p db =
  case getAllTablesCountSql (Just db) of
    Just s -> do
      xs <- runSqlE db RNil s
      return $
        map
          ( \z ->
              TableCount
                { tcTable = rvalf #name z
                , tcRows = rvalf #size z
                , tcCreated = rvalf #created z
                , tcUpdated = rvalf #updated z
                }
          )
          xs
    Nothing -> do
      ts <- allTables p db
      forM ts $ \t -> do
        n <- getOneTableRowCount db t -- removed catch exception:do we still need it?
        return $ TableCount t n Nothing Nothing

-- | return views filtered by a predicate
allViews ::
  (GConn a, ML e m) =>
  (Table a -> Bool) ->
  a ->
  m [Table a]
allViews = allTablesAndViews TVView

-- | return tables filtered by a predicate
allTables ::
  (GConn a, ML e m) =>
  (Table a -> Bool) ->
  a ->
  m [Table a]
allTables = allTablesAndViews TVTable

-- | return tables and views filtered by a predicate
allTablesAndViews ::
  forall a m e.
  (GConn a, ML e m) =>
  TVTable ->
  (Table a -> Bool) ->
  a ->
  m [Table a]
allTablesAndViews tv p db = do
  vs <- runSqlE db RNil ((if isTable tv then getAllTablesSql else getAllViewsSql) db)
  return $ filter p $ map (\t -> t{tTable = isTable tv}) vs

-- | return all tables for use with wprint
getAllTablesSqlImpl ::
  forall db m e.
  (ML e m, GConn db) =>
  db ->
  m (Rec Sql.RState '[Sql.Sel (Table db)])
getAllTablesSqlImpl db = runSql db RNil $ getAllTablesSql db

-- | return all views for use with wprint
getAllViewsSqlImpl ::
  forall db m e.
  (ML e m, GConn db) =>
  db ->
  m (Rec Sql.RState '[Sql.Sel (Table db)])
getAllViewsSqlImpl db = runSql db RNil $ getAllViewsSql db

-- doesnt work for oracle ie will try to drop or will fail!

-- | 'dropView' is a convenience method for dropping a view
dropView ::
  (ML e m, GConnWrite db) =>
  db ->
  Table db ->
  m ()
dropView db =
  void . runSqlUnsafe db RNil . dropViewIfExistsSql db

-- | 'dropTable' is a convenience method for dropping a table
dropTable ::
  (ML e m, GConnWrite db) =>
  db ->
  Table db ->
  m ()
dropTable db =
  void . runSqlUnsafe db RNil . dropTableIfExistsSql db

-- | checks for existence of a table
existsTable ::
  (GConn a, ML e m) =>
  a ->
  Table a ->
  m Bool
existsTable db table = do
  ts <- runSqlE db RNil (existsTableSql db table)
  if ts == found
    then do
      $logDebug [st|found #{table} db=#{showDb db}|]
      return True
    else
      if ts == notfound
        then do
          $logWarn [st|could not find #{table} db=#{showDb db}|]
          return False
        else U.throwIO $ GBException [st|unexpected value found[#{ts}] table[#{table}]|]

-- | my version of the withTransaction from hdbc: need to do this to lift over ML
withTransaction ::
  forall e m conn a.
  (ML e m, H.IConnection conn) =>
  conn ->
  (conn -> m a) ->
  m a
withTransaction conn func =
  do
    r <- U.onException (func conn) doRollback
    liftIO $ H.commit conn
    return r
 where
  doRollback :: m ()
  doRollback =
    -- Discard any exception from (rollback conn) so original
    -- exception can be re-raised
    U.catch (liftIO $ H.rollback conn) doRollbackHandler
  doRollbackHandler :: E.SomeException -> m ()
  doRollbackHandler (_ :: E.SomeException) = return ()

-- | 'compareDatabase' compares tables in two databases by name and count of rows. in A and in B / in A only / in B only -- ie These
compareDatabase ::
  forall a b e m.
  (GConn a, GConn b, ML e m) =>
  a ->
  b ->
  m
    ( Set (Table a, Int)
    , Set (Table b, Int)
    , Map Ordering (NonEmpty ((Table a, Int), (Table b, Int)))
    )
compareDatabase a b = do
  let fn :: forall x. Table x -> Text
      fn = T.strip . T.toLower . fromTNameRaw . tName
  let chk :: forall x. [TableCount x] -> m [Table x]
      chk = liftIO . forceErrorIO . expectNoDups Invariant "compareDatabase" . map tcTable
  x1 <- allTablesCount (const True) a
  x2 <- allTablesCount (const True) b
  void $ chk x1
  void $ chk x2
  let fnZ :: forall x. [TableCount x] -> [(Table x, Int)]
      fnZ = map (tcTable &&& tcRows)
  let ret = divvyKeyed fn fn (fnZ x1) (fnZ x2)
  mapM_ $logInfo (compareIt ret) -- cos stdout gets truncated at 8000 chars
  return ret

-- | compares tables with rowcounts from different databases and groups them in a user friendly format
compareIt ::
  forall a b.
  ( Set (Table a, Int)
  , Set (Table b, Int)
  , Map Ordering (NonEmpty ((Table a, Int), (Table b, Int)))
  ) ->
  [T.Text]
compareIt (Set.toList -> ll, Set.toList -> rr, M.map N.toList -> m) =
  let fmtOne :: Table x -> Int -> T.Text
      fmtOne a i = sformat (F.right 50 ' ' % " " % F.rpadded 10 ' ' int) a i
      fmtBoth :: Table x -> Int -> Int -> T.Text
      fmtBoth a i j = sformat (F.right 50 ' ' % " " % F.rpadded 10 ' ' int % " " % F.rpadded 10 ' ' int % "     " % F.right 10 ' ') a i j (pct i j)
      fn :: Ordering -> [((Table a, Int), (Table b, Int))]
      fn o = fromMaybe mempty (m M.!? o)
      lterrs, oks, gterrs :: [((Table a, Int), (Table b, Int))]
      (lterrs, oks, gterrs) = (fn LT, fn EQ, fn GT)
      ret =
        [sformat ("left only " % fmtLen) ll]
          <> map (uncurry fmtOne) ll
          <> [sformat ("right only " % fmtLen) rr]
          <> map (uncurry fmtOne) rr
          <> [sformat ("both and all good " % fmtLen) oks]
          <> map (uncurry fmtOne . fst) oks
          <> [sformat ("both but with errors LT " % fmtLen) lterrs]
          <> map (\((a, i), (_b, j)) -> fmtBoth a i j) lterrs
          <> [sformat ("both but with errors GT " % fmtLen) gterrs]
          <> map (\((a, i), (_b, j)) -> fmtBoth a i j) gterrs
          {-
                       <> [sformat ("Left" % fmtKV % "Right" % fmtKV % "Ok" % fmtKV % "Errs" % fmtKV % "LTErrs" % fmtKV % "GTErrs" % fmtKV)
                                     ll               rr                oks            errs             lterrs             gterrs
                          ]
          -}
          <> [ T.intercalate
                " "
                [ fmtKV' "Left" ll
                , fmtKV' "Right" rr
                , fmtKV' "Ok" oks
                , fmtKV' "Errs" (lterrs <> gterrs)
                , fmtKV' "LTErrs" lterrs
                , fmtKV' "GTErrs" gterrs
                ]
             ]
   in ret
 where
  -- sfmtKV = "=" % fmap (. length) int % " "
  fmtKV' :: forall a1 a2. F.Format ([a2] -> Text) a1 -> a1
  fmtKV' x = sformat (x % "=" % fmap (. length) int % " ")
  fmtLen :: forall x. F.Format Text ([x] -> Text)
  fmtLen = fmap (. length) int

-- | shows user-friendly percent difference for row counts
pct :: Int -> Int -> String
pct a b =
  if a == 0 || b == 0
    then "empty!"
    else
      let x :: Double
          x = fromIntegral a / fromIntegral b
          y :: Int
          y = abs $ truncate $ 100 * if x < 1 then 1 - x else x - 1
       in if y < 5
            then "< 5%"
            else
              if y < 10
                then "< 10%"
                else show y ++ "% !"

-- | 'logDatabaseAll' logs table name and row counts for a given database
logDatabaseAll :: (GConn a, ML e m) => a -> m ()
logDatabaseAll = logDatabase "" (const True)

-- | logs table name and row counts filtered by a predicate
logDatabase ::
  (GConn a, ML e m) =>
  Text ->
  (Table a -> Bool) ->
  a ->
  m ()
logDatabase txt p anydb = do
  tps <- allTablesCount p anydb
  case tps of
    [] -> U.throwIO $ GBException [st|logDatabase has no tables after filtering: #{txt}|]
    r : rs -> mapM_ $logInfo (logDatabaseImpl txt anydb (r :| rs))

-- | pretty print the table count information
logDatabaseImpl ::
  GConn a =>
  Text ->
  a ->
  NonEmpty (TableCount a) ->
  [T.Text]
logDatabaseImpl txt anydb tps =
  let len = T.length $ NE.maximumBy1 (comparing T.length) $ N.map (showTable . tcTable) tps
      x1, x2, x3 :: Int
      (x1, x2, x3) = (5, 4, 8)
      len2 :: Int
      len2 = x1 + len + x2 + x3
      pad = T.replicate len2 "-"
      ff :: Int -> TableCount a -> Text
      ff j t =
        sformat ((F.left 5 ' ' %. int) % " " % F.right (len + 4) ' ' % (F.left 8 ' ' %. int)) j (tcTable t) (tcRows t)
      ret =
        pure "\n"
          <> pure pad
          <> pure [st|start logDatabase [#{txt}] #{showDb anydb} #{length tps} tables|]
          <> pure pad
          <> N.zipWith ff (1 :| [2 ..]) tps
          <> pure pad
          <> N.zipWith ff (1 :| [2 ..]) (N.sortBy (comparing (Down . tcRows &&& T.toLower . fromTNameRaw . tName . tcTable)) tps)
          <> pure [st|end logDatabase [#{txt}] #{showDb anydb} #{length tps} tables|]
   in N.toList ret

-- | display differences between two sets of tables
prtDiff ::
  TP a b ->
  ((Int, These (Table a) (Table b)), T.Text)
prtDiff tp =
  let x0, x3, len :: Int
      (x0, x3, len) = (14, 10, 55)
      fmt :: Text -> Table x -> Int -> T.Text
      fmt = sformat ((F.right x0 '_' %. F.stext) % F.right len ' ' % (F.left x3 ' ' %. int))
      fmtend :: Int -> Int -> Text
      fmtend n2 avg = sformat ((F.left x3 ' ' %. int) % (F.left x3 ' ' %. int) % "%") n2 (-avg)
      (thtab, txt) =
        case tp of
          LeftOnly (n, t) -> (This t, fmt "Left only" t n)
          RightOnly (n, t) -> (That t, fmt "Right only" t n)
          LEmpty (n, (t1, t2)) -> (These t1 t2, fmt "Left empty" t1 n)
          REmpty (n, (t1, t2)) -> (These t1 t2, fmt "Right empty" t1 n)
          Less (n1, n2, avg, (t1, t2)) -> (These t1 t2, fmt "Less" t1 n1 <> fmtend n2 avg)
          More (n1, n2, avg, (t1, t2)) -> (These t1 t2, fmt "More" t1 n1 <> fmtend n2 avg)
          Same (n, (t1, t2)) -> (These t1 t2, fmt "Same" t1 n)
   in ((GU.getIndex tp, thtab), txt)

-- in order of badness  -- have to make Less and More more negative for sorting cos the worst is higher values
-- tuple with tablename to get (TP,Text) and then L.sort on this

-- | represents differences between two tables
data TP a b
  = LeftOnly !(Int, Table a)
  | REmpty !(Int, (Table a, Table b))
  | RightOnly !(Int, Table b)
  | LEmpty !(Int, (Table a, Table b))
  | Less !(Int, Int, Int, (Table a, Table b)) -- -ve values for sorting
  | More !(Int, Int, Int, (Table a, Table b)) -- -ve values for sorting
  | Same !(Int, (Table a, Table b)) -- defaults to by tablename cos mostly zero
  deriving stock (Generic, Show, Eq, Ord)

-- | take the diff of two databases and log
logDiffDatabase ::
  (ML e m, GConn a, GConn b) =>
  a ->
  b ->
  m ()
logDiffDatabase db1 db2 = do
  txt <- diffDatabase db1 db2
  $logInfo txt

-- | take the diff of two databases
diffDatabase ::
  (ML e m, GConn a, GConn b) =>
  a ->
  b ->
  m T.Text
diffDatabase db1 db2 = do
  xs <- allTablesCount (const True) db1
  ys <- allTablesCount (const True) db2
  let (lencs, cs) = diffDatabase' xs ys
  let hdr2 :: T.Text -> T.Text
      hdr2 txt = [st|#{txt} logDiffDatabase #{db1} #{db2} #{lencs} db1=#{length xs} db2=#{length ys} tables|]
  let hdr = T.replicate (T.length (hdr2 "start")) "-"
  return $ T.unlines ["\n", hdr, hdr2 "start", cs, hdr2 "end  ", hdr]

-- | internal: used by 'diffDatabase'
diffDatabase' ::
  forall a b.
  HasCallStack =>
  [TableCount a] ->
  [TableCount b] ->
  (Int, T.Text)
diffDatabase' xs ys =
  let fmat :: Either (TableCount a) (TableCount b) -> TName
      fmat = either (tName . tcTable) (tName . tcTable)
      cs =
        NE.groupAllWith fmat (map Left xs <> map Right ys) <&> \x ->
          case N.sort x of
            Left (TableCount t1 n1 _ _) :| [Right (TableCount t2 n2 _ _)]
              | tName t1 /= tName t2 -> normalError "diffDatabase': mismatching names"
              | otherwise -> handleBoth (t1, n1) (t2, n2)
            Left t :| [] -> LeftOnly (tcRows t, tcTable t)
            Right t :| [] -> RightOnly (tcRows t, tcTable t)
            o -> normalError $ "diffDatabase': " ++ psiS o
      ds = N.groupAllWith fst $ map prtDiff cs
   in (length cs, T.unlines $ "\n" : concatMap (map snd . N.toList) ds)

-- | deeper compare of two table with the same name
handleBoth :: (Table a, Int) -> (Table b, Int) -> TP a b
handleBoth (t1, n1) (t2, n2) =
  let z = (t1, t2)
   in case (n1, n2) of
        (0, 0) -> Same (0, z)
        (0, _) -> LEmpty (n2, z)
        (_, 0) -> REmpty (n1, z)
        (_, _) ->
          let avg = round ((100 * (fromIntegral (n2 - n1) / fromIntegral n1)) :: Double)
           in case compare n1 n2 of
                LT -> Less (n1, n2, -avg, z)
                GT -> More (n1, n2, avg, z)
                EQ -> Same (n1, z)

-- | remove newlines and extra spaces
flattenSql :: Text -> Text
flattenSql = RM.streamEdit @Void (Z.takeWhile1P (Just "flattenSql") isSpace) (const " ") . T.strip

-- | convert from sqlvalue to bcp compatible string
toBcpFromSql :: HasCallStack => SqlValue -> ByteString
toBcpFromSql = \case
  SqlByteString bs -> bs
  SqlNull -> ""
  SqlString ss -> stringToByteString ss
  SqlInteger i -> stringToByteString (show i)
  SqlInt64 i -> stringToByteString (show i)
  SqlInt32 i -> stringToByteString (show i)
  SqlDouble d -> stringToByteString (show d)
  SqlChar c -> stringToByteString (show c)
  SqlUTCTime dt -> stringToByteString (formatTime defaultTimeLocale "%F %T" dt)
  SqlLocalDate dt -> stringToByteString (formatTime defaultTimeLocale "%F %T" dt)
  SqlLocalTime dt -> stringToByteString (formatTime defaultTimeLocale "%F %T" dt)
  SqlZonedTime dt -> stringToByteString (formatTime defaultTimeLocale "%F %T" dt)
  o -> normalError $ T.unpack [st|copyDBSqltoBCPFile: dont know how to handle [#{psiT o}]|]

-- | create a copy of table: works across rdbms
createDBTableFrom ::
  (ML e m, GConn src, GConnWrite tgt) =>
  src ->
  Table src ->
  tgt ->
  Table tgt ->
  m ()
createDBTableFrom srcdb tabin tgtdb tabout = do
  meta <- getColumnInfo srcdb tabin
  void $ runSqlUnsafe tgtdb RNil (createDBTableFromSql meta tabout)

-- todo: can we have table with no fields in any db (apparently in postgres you can)

-- | sql to create a table from metadata
createDBTableFromSql ::
  (HasCallStack, GConn tgt, Sql.WriteableDB tgt ~ 'True) =>
  NonEmpty (ColDataType, ColumnMeta) ->
  Table tgt ->
  Sql tgt '[] '[Sql.Upd]
createDBTableFromSql metas tabout =
  let mid (cd, meta) =
        escapeField tabout (cName meta)
          <> " "
          <> translateColumnMeta tabout (cd, meta)
          <> " "
          <> if cIsNull meta
            then "null"
            else
              "not null"
                <> " "
   in Sql.mkSql
        "createDBTableFromSql"
        [st|create table #{tabout}

                (
                  #{intercalateMap1 "\n  , " mid metas}
                )
             |]

-- | create select statement based on meta data
getDBSelectSql ::
  GConn src =>
  NonEmpty ColumnMeta ->
  Table src ->
  Text
getDBSelectSql metas srctable =
  [st|select #{intercalateMap1 "," (escapeField srctable . cName) metas} from #{srctable}|]

-- | create insert statement based on metas data
getDBInsertSql ::
  GConnWrite tgt =>
  NonEmpty ColumnMeta ->
  Table tgt ->
  Text
getDBInsertSql metas tgttable =
  let xs = N.map (escapeField tgttable . cName) metas
   in [st|insert into #{tgttable} #{Sql.vvs xs} values#{Sql.qqs metas}|]

-- | get the number of rows for a table
getOneTableRowCount ::
  (GConn db, ML e m) =>
  db ->
  Table db ->
  m Int
getOneTableRowCount anydb table =
  runSqlE anydb RNil (Sql.mkSql [st|getOneTableRowCount #{table}|] [st|select count(*) from #{table}|] :: Sql db '[] '[Sql.SelRowCol Int])

-- | get meta data from a table
getColumnInfo ::
  (ML e m, GConn db) =>
  db ->
  Table db ->
  m (NonEmpty (ColDataType, ColumnMeta))
getColumnInfo db t = do
  let (f, sql) = getColumnMetaSql db t
  cs <- runSqlE db RNil sql
  case cs of
    [] -> U.throwIO $ GBException [st|getColumnInfo: missing metadata for #{t} in #{showDb db}|]
    a : as -> return $ N.map (f &&& id) (a :| as)

-- | compare all fields from two tables
compareFields ::
  (ML e m, GConn db1, GConn db2) =>
  db1 ->
  Table db1 ->
  db2 ->
  Table db2 ->
  m ()
compareFields db1 t1 db2 t2 = do
  m1 <- getColumnInfo db1 t1
  m2 <- getColumnInfo db2 t2
  let ts = compareFields' m1 m2
  mapM_ $logInfo ts

-- | internal: used by 'compareFields'
compareFields' ::
  HasCallStack =>
  NonEmpty (ColDataType, ColumnMeta) ->
  NonEmpty (ColDataType, ColumnMeta) ->
  NonEmpty Text
compareFields' m1 m2 =
  let fmat :: Either (ColDataType, ColumnMeta) (ColDataType, ColumnMeta) -> Text
      fmat = either (T.toLower . cName . snd) (T.toLower . cName . snd)
      cs =
        N.groupAllWith1 fmat (N.map Left m1 <> N.map Right m2) <&> \case
          Left t1@(_, z1) :| [Right t2@(_, z2)]
            | T.toLower (cName z1) /= T.toLower (cName z2) -> normalError "compareFields': mismatching names"
            | otherwise -> ((0 :: Int, cName z1), These t1 t2)
          Left t@(_, m) :| [] -> ((1, cName m), This t)
          Right t@(_, m) :| [] -> ((2, cName m), That t)
          o -> normalError $ "compareFields': " ++ psiS o
   in N.map (prtFieldDiff . snd) (N.sortBy (comparing fst) cs)

-- | pretty print the difference between two columns
prtFieldDiff :: These (ColDataType, ColumnMeta) (ColDataType, ColumnMeta) -> Text
prtFieldDiff tab =
  case tab of
    These (c1, t1) (c2, t2)
      | c1 == c2 -> [st|Same #{cName t1} #{cType t1} #{cType t2} #{psiT c1}|]
      | otherwise -> [st|***error: Same name but different types c1=#{psiT c1} c2=#{psiT c2} t1=#{psiT t1} t2=#{psiT t2}|]
    This (_, t) -> [st|Left only #{cName t} #{cType t}|]
    That (_, t) -> [st|Right only #{cName t} #{cType t}|]

-- | compare two blocks of text for data in common ignoring case
defaultCompareData ::
  [Text] ->
  [Text] ->
  [(Text, Text)]
defaultCompareData xs ys = map (\x -> (x, x)) (L.intersectBy (on (==) T.toLower) xs ys)

-- | remove any tuples that have keys from the first parameter
removeKeys :: [Text] -> [(Text, Text)] -> [(Text, Text)]
removeKeys (map (T.strip . T.toLower) -> cs) tps =
  let ff = T.strip . T.toLower
   in filter (\(a, b) -> ff a `notElem` cs && ff b `notElem` cs) tps

-- | compare two tables from left to right then flip around
compareTableDataBoth ::
  (GConn a, ML e m) =>
  a ->
  Table a ->
  Table a ->
  ( [Text] ->
    [Text] ->
    [(Text, Text)]
  ) ->
  m [[SqlValue]]
compareTableDataBoth db t1 t2 cmp = do
  xs <- compareTableData db t1 t2 cmp
  ys <- compareTableData db t2 t1 cmp
  return (xs <> ys)

-- | compare two tables using a comparator
compareTableData ::
  forall m e db.
  (GConn db, ML e m) =>
  db ->
  Table db ->
  Table db ->
  ( [Text] ->
    [Text] ->
    [(Text, Text)]
  ) ->
  m [[SqlValue]]
compareTableData db t1 t2 cmp = do
  when (t1 == t2) $ U.throwIO $ GBException [st|compareTableData comparing the same tables t1==t2 t1=#{t1} t2=#{t2}|]
  let ff tab = do
        ms <- getColumnInfo db tab
        return $ ms ^.. traverse . _2 . to cName
  c1 <- ff t1
  c2 <- ff t2
  let (fs1, fs2) = (T.intercalate "," *** T.intercalate ",") (unzip (cmp c1 c2))
  xs <- runSqlE db RNil (Sql.mkSql [st|compareTableData (#{t1},#{t2})|] [st|select #{fs1} from #{t1} except select #{fs2} from #{t2}|] :: Sql db '[] '[Sql.SelRaw])
  if null xs
    then $logInfo [st|compareTableData #{t1} #{t2} is good!!|]
    else werrLS [st|compareTableData #{t1} #{t2} is bad!! len=#{length xs}|]
  return xs

-- | convert from a sql column type to 'TH.Name' for use in template haskell functions
convertTypeMeta :: H.SqlTypeId -> Maybe TH.Name
convertTypeMeta tp
  | tp `elem` [H.SqlCharT, H.SqlWCharT, H.SqlVarCharT, H.SqlWVarCharT, H.SqlLongVarCharT, H.SqlWLongVarCharT, H.SqlCharT, H.SqlWCharT] = Just ''String
  | tp `elem` [H.SqlDecimalT, H.SqlNumericT, H.SqlRealT, H.SqlFloatT, H.SqlDoubleT] = Just ''Double
  | tp `elem` [H.SqlSmallIntT, H.SqlIntegerT, H.SqlBitT, H.SqlTinyIntT] = Just ''Int
  | tp == H.SqlBigIntT = Just ''Integer
  | tp `elem` [H.SqlBinaryT, H.SqlVarBinaryT, H.SqlLongVarBinaryT] = Just ''ByteString
  | tp `elem` [H.SqlDateT, H.SqlTimeT, H.SqlTimeWithZoneT, H.SqlTimestampT, H.SqlTimestampWithZoneT, H.SqlUTCTimeT, H.SqlUTCDateTimeT] = Just ''UTCTime
  | tp == H.SqlGUIDT = Just ''UUID -- change to char(32) and add a conversion
  | otherwise = Nothing -- normalError $ "convertTypeMeta: no idea " ++ show tp

-- | convert from a sql column type to a mssql column definition clause
convertType :: HasCallStack => H.SqlTypeId -> Maybe Int -> String
convertType tp (Just sz)
  | tp `elem` [H.SqlCharT, H.SqlWCharT] = "char(" <> show sz <> ")"
  | tp `elem` [H.SqlVarCharT, H.SqlWVarCharT, H.SqlLongVarCharT, H.SqlWLongVarCharT] = "varchar(" <> show sz <> ")"
  | tp `elem` [H.SqlDecimalT, H.SqlNumericT, H.SqlRealT, H.SqlFloatT, H.SqlDoubleT] = "float" --  <> show sz <> ")" -- could be more specific here
  | tp `elem` [H.SqlSmallIntT, H.SqlIntegerT, H.SqlBitT, H.SqlTinyIntT] = "int"
  | tp == H.SqlBigIntT = "bigint"
  | tp `elem` [H.SqlCharT, H.SqlWCharT] = "char(" <> show sz <> ")"
  | tp == H.SqlBinaryT = "binary(" <> show sz <> ")"
  | tp `elem` [H.SqlVarBinaryT, H.SqlLongVarBinaryT] = "varbinary(" <> show sz <> ")"
  | tp == H.SqlDateT = "date"
  | tp `elem` [H.SqlTimeT, H.SqlTimeWithZoneT] = "time"
  | tp `elem` [H.SqlTimestampT, H.SqlTimestampWithZoneT, H.SqlUTCTimeT, H.SqlUTCDateTimeT] = "datetime2"
  | tp == H.SqlGUIDT = "uniqueidentifier" -- change to char(32) and add a conversion
  | otherwise = normalError $ "convertType: no idea " ++ psiS tp
convertType tp Nothing = normalError $ "convertType: size is not set for " ++ psiS tp

-- | 'Map' holding the sql server column type and its haskell equivalent
systypes :: Map Text TH.Name
systypes =
  M.fromList
    [ ("image", ''String)
    , ("text", ''String)
    , ("uniqueidentifier", ''String)
    , ("date", ''UTCTime)
    , ("time", ''String)
    , ("datetime2", ''UTCTime)
    , ("datetimeoffset", ''String)
    , ("tinyint", ''Int)
    , ("smallint", ''Int)
    , ("int", ''Integer)
    , ("smalldatetime", ''UTCTime)
    , ("real", ''Double)
    , ("money", ''Double)
    , ("datetime", ''UTCTime)
    , ("float", ''Double)
    , ("sql_variant", ''ByteString)
    , ("ntext", ''Text)
    , ("bit", ''Bool)
    , ("decimal", ''Double)
    , ("numeric", ''Double)
    , ("smallmoney", ''Double)
    , ("bigint", ''Integer)
    , ("hierarchyid", ''ByteString)
    , ("geometry", ''ByteString)
    , ("geography", ''ByteString)
    , ("varbinary", ''ByteString)
    , ("varchar", ''String)
    , ("binary", ''ByteString)
    , ("char", ''String)
    , ("timestamp", ''ByteString)
    , ("nvarchar", ''ByteString)
    , ("nchar", ''ByteString)
    , ("xml", ''ByteString)
    , ("sysname", ''ByteString)
    ]

-- | convenience function for converting a string to a bytestring
stringToByteString :: String -> ByteString
stringToByteString = TE.encodeUtf8 . T.pack
