{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DBFake where

import BaseUtils.Extra
import qualified Conduit as C
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Char
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import qualified Database.HDBC as H
import qualified Database.HDBC.ODBC.ConnectionImpl as HC
import qualified Database.HDBC.Statement as H
import HSql.Core
import HSql.Core.VinylUtils
import HSql.ODBC.DBConn
import HSql.ODBC.StreamingUtils
import qualified Language.Haskell.TH.Syntax as TH
import Logging
import Text.Shakespeare.Text
import qualified UnliftIO.Exception as U
import Utils.Positive

data DBFake a = DBFake
  { _fRef :: !Ref
  , _fStmt :: !H.Statement
  , _fInfo :: !Text
  , _fStype :: !SType
  }

data SType = SS | SC | IC | UPD | SEL deriving stock (Show, Eq)

instance ToText SType where
  toText = fromText . T.pack . show

type instance WriteableDB (DBFake _) = 'True

type ExecPred = [SqlValue] -> Bool
fexecp :: Int -> ExecPred
fexecp i xs = length xs == i

data FOut
  = FFetchRow !(Maybe [SqlValue])
  | FExecuteSelect ![SqlValue]
  | FExecuteUpdate ![SqlValue] !Int
  | FFinish
  | FExecuteMany ![[SqlValue]]
  | FExecuteRaw
  | FGetColumnNames ![String]
  | FDescribeResult ![(String, H.SqlColDesc)]
  | FNextResultSet !(Maybe (Either Int RMeta))
  | FError !Text
  | FWarn !Text
  | FCommit
  deriving stock (Show, Eq)

type TVCmd = TVar [CCmd SType]
type TVOut = TVar [FOut]

-- have to demarcate start and end of each type
data CCmd stype
  = CUpdate !stype ![SqlValue] !Int
  | CSelectStart !stype ![SqlValue]
  | CSelect !stype ![SqlValue]
  | CSelectEnd !stype
  | CEnd !stype
  | CFinish !stype
  | CCommit !stype
  | CDisc !stype
  deriving stock (Show, Eq)

data Ref = Ref {refCmd :: !TVCmd, refOut :: !TVOut, refLogger :: !MyLogger}

tst1 :: ML () m => m ((Int, [[SqlValue]]), ([CCmd SType], [FOut]))
tst1 = do
  c <- liftIO $ newTVarIO [CUpdate UPD [] 99, CEnd UPD, CFinish UPD, CCommit UPD, CDisc UPD, CSelectStart SEL [], CSelect SEL [SqlInt32 0, SqlString "asdf"], CSelect SEL [SqlInt32 0, SqlString "asdf"], CSelectEnd SEL, CEnd SEL, CFinish SEL, CCommit SEL, CDisc SEL]
  ret <- liftIO $ newTVarIO []
  lg <- getLogger $(TH.qLocation >>= liftLoc)
  let ref = Ref c ret lg
  let rdb stype = DBFake @Writeable ref (mystmt stype ref) "testfakedb" stype
  xs <- runSqlE (rdb UPD) (I2 222 "myupd") (mkSql "tst3' insert" "update something" :: Sql db '[Int, String] '[Upd])
  ys <- runSqlE (rdb SEL) (I2 123 "mysel") (mkSql "tst3' select" "select something" :: Sql db '[Int, String] '[SelRaw])
  cs <- liftIO $ readTVarIO c
  os <- liftIO $ reverse <$> readTVarIO ret
  return ((xs, ys), (cs, os))

t3ok :: [CCmd SType]
t3ok =
  [ CUpdate UPD [SqlInt32 222, SqlString "myupd"] 13
  , CEnd UPD
  , CFinish UPD
  , CCommit UPD
  , CDisc UPD
  ]
    <> sel SEL [SqlInt32 123, SqlString "mysel"] 0 5
    <> [ CFinish SEL
       , CCommit SEL
       , CDisc SEL
       ]

tst3' :: ML () m => [CCmd SType] -> m (GOut (Either Text (Int, [[SqlValue]])))
tst3' cmds = do
  zz1 <- askLoggerIO
  tst4generic cmds $ \rdb ->
    U.handle (\(GBException e) -> $logError [st|exception #{e}|] >> return (Left (T.pack (show e)))) $ do
      xs <- liftIO $ flip runLoggingT zz1 $ flip runReaderT () $ runSqlE (rdb UPD) (I1 (222, "myupd")) (mkSql "tst3' insert" "update something" :: Sql db '[(Int, String)] '[Upd])
      ys <- liftIO $ flip runLoggingT zz1 $ flip runReaderT () $ runSqlE (rdb SEL) (I1 (123, "mysel")) (mkSql "tst3' select" "select something" :: Sql db '[(Int, String)] '[SelRaw])
      return $ Right (xs, ys)

t4ok, t4ok' :: [CCmd SType]
t4ok =
  [ CSelectStart SS []
  , CSelect SS [SqlInt32 0, SqlString "aaaa"]
  , CSelect SS [SqlInt32 1, SqlString "bbbb"]
  , CSelectEnd SS
  , CFinish SS
  , CCommit SS
  ]
t4ok' =
  [CSelectStart SS [SqlInt32 212]]
    <> map (\i -> CSelect SS [SqlInt32 i, SqlString ("a" ++ show i)]) [0 .. 23]
    <> [CSelectEnd SS, CFinish SS, CCommit SS, CDisc SS]

t4ok4 :: [CCmd SType]
t4ok4 =
  [CSelectStart SS [SqlInt32 212]]
    <> sel' SS 0 1
    <> upd' IC 1 (concat (ssel 0 1))
    <> sel' SS 1 1
    <> upd' IC 1 (concat (ssel 1 1))
    <> sel' SS 2 1
    <> upd' IC 1 (concat (ssel 2 1))
    <> [CCommit IC, CSelectEnd SS, CFinish SS, CCommit SS, CFinish IC, CFinish IC, CCommit IC]

sel :: SType -> [SqlValue] -> Int -> Int -> [CCmd SType]
sel stype hs i n = [CSelectStart stype hs] <> map (CSelect stype) (ssel i n) <> [CSelectEnd stype, CEnd stype]

sel' :: SType -> Int -> Int -> [CCmd SType]
sel' stype i n = map (CSelect stype) (ssel i n)

ssel :: Int -> Int -> [[SqlValue]]
ssel i n = map (\j -> [SqlInt32 (fromIntegral j), SqlString ("a" ++ show j)]) [i .. n + i - 1]

upd, upd', upd'' :: SType -> Int -> [SqlValue] -> [CCmd SType]
upd stype rc hs = [CUpdate stype hs rc, CEnd stype, CCommit stype]
upd' stype rc hs = [CUpdate stype hs rc, CEnd stype]
upd'' stype rc hs = [CUpdate stype hs rc, CCommit stype]

tst4' :: ML () m => [CCmd SType] -> m (GOut [[SqlValue]])
tst4' cmds =
  tst4generic cmds $ \rdb ->
    C.runConduitRes $
      selectSourceLazyTypedC "tst4" (mkSql "tst4" "select stuff" :: Sql db '[Int] '[Sel Raw]) (I1 212) (rdb SS)
        C..| C.mapC unRaw
        C..| C.sinkList

tst4'' :: ML () m => [CCmd SType] -> m (GOut Int)
tst4'' cmds =
  tst4generic cmds $ \rdb -> do
    let sc = defSC @3 -- 3 is just the commit interval which we cant track in this
    let pcnt = _P @2
    let ins r = (mkSql' @'[U1] @'[Raw] [st|insert znork r=#{showP r}|], r *! pcnt)
    C.runConduitRes $
      selectSourceLazyTypedC "tst4''" (mkSql "tst4''" "select stuff" :: Sql db '[Int] '[Sel Raw]) (I1 212) (rdb SS)
        C..| C.mapC unRaw
        C..| fit1 sc
        C..| insertConduitC "tst4''" sc ins (rdb IC)

t5ok, t5ok' :: [CCmd SType]
t5ok =
  [CSelectStart SS [SqlInt32 212]]
    <> sel' SS 0 5
    <> upd IC 5 (concat (ssel 0 5))
    <> sel' SS 5 5
    <> upd IC 5 (concat (ssel 5 5))
    <> sel' SS 10 5
    <> upd IC 5 (concat (ssel 10 5))
    <> [CSelectEnd SS, CFinish SS, CCommit SS, CDisc SS, CFinish IC, CFinish IC, CCommit IC, CDisc IC]
-- intersperses the bulk updates then at the end runs individual updates
t5ok' =
  [CSelectStart SS [SqlInt32 212]]
    <> sel' SS 0 5
    <> upd IC 5 (concat (ssel 0 5))
    <> sel' SS 5 5
    <> upd IC 5 (concat (ssel 5 5))
    <> sel' SS 10 5
    <> upd IC 5 (concat (ssel 10 5))
    <> sel' SS 15 3
    <> [CSelectEnd SS]
    <> upd' IC 1 (concat (ssel 15 1))
    <> upd' IC 1 (concat (ssel 16 1))
    <> upd' IC 1 (concat (ssel 17 1))
    <> [CCommit IC, CFinish SS, CCommit SS, CDisc SS, CFinish IC, CFinish IC, CCommit IC, CDisc IC]

tst5 :: ML () m => [CCmd SType] -> m (GOut Int)
tst5 cmds =
  tst4generic cmds $ \rdb -> do
    let sc = (defSC @1){sOneInsert = _P @5}
    let pcnt = _P @2
    let ins r = (mkSql' @'[U1] @'[Raw] [st|insert znork r=#{showP r}|], r *! pcnt)
    C.runConduitRes $
      selectSourceLazyTypedC "tst5" (mkSql "tst5" "select stuff" :: Sql db '[Int] '[Sel Raw]) (I1 212) (rdb SS)
        C..| C.mapC unRaw
        C..| fit1 sc
        C..| insertConduitC "tst5" sc ins (rdb IC)

tst6 :: ML () m => [CCmd SType] -> m (GOut Int)
tst6 cmds =
  tst4generic cmds $ \rdb -> do
    let sc = (defSC @1){sOneInsert = _P @5}
    let pcnt = _P @3
    let ins r = (mkSql' @'[U1] @'[Raw] [st|insert znork r=#{showP r}|], r *! pcnt)
    C.runConduitRes $
      selectSourceLazyTypedC "tst6" (mkSql "tst6" "select stuff" :: Sql db '[Int] '[Sel Raw]) (I1 212) (rdb SS)
        C..| C.mapC unRaw
        C..| zipC 'a' succ (\a b -> SqlChar a : b)
        C..| fit1 sc
        C..| insertConduitC "tst6" sc ins (rdb IC)

zipC :: ML () m => a -> (a -> a) -> (a -> i -> o) -> C.ConduitT i o m ()
zipC a' f g = go a'
 where
  go a = do
    mi <- C.await
    case mi of
      Nothing -> return ()
      Just i -> C.yield (g a i) >> go (f a)

t6ok :: [CCmd SType]
t6ok =
  [CSelectStart SS [SqlInt32 212]]
    <> sel' SS 0 5
    <> upd IC 5 (concat $ zipWith (\a b -> SqlChar a : b) ['a' ..] (ssel 0 5))
    <> sel' SS 5 5
    <> upd IC 5 (concat $ zipWith (\a b -> SqlChar a : b) (drop 5 ['a' ..]) (ssel 5 5))
    <> sel' SS 10 5
    <> upd IC 5 (concat $ zipWith (\a b -> SqlChar a : b) (drop 10 ['a' ..]) (ssel 10 5))
    <> sel' SS 15 3
    <> [CSelectEnd SS]
    <> upd' IC 1 (SqlChar 'p' : concat (ssel 15 1))
    <> upd' IC 1 (SqlChar 'q' : concat (ssel 16 1))
    <> upd' IC 1 (SqlChar 'r' : concat (ssel 17 1))
    <> [CCommit IC, CFinish SS, CCommit SS, CDisc SS, CFinish IC, CFinish IC, CCommit IC, CDisc IC]

tst6a :: ML () m => [CCmd SType] -> m (GOut Int)
tst6a cmds =
  tst4generic cmds $ \rdb -> do
    let sc = (defSC @1){sOneInsert = _P @5}
    let pcnt = _P @3
    let ins r = (mkSql "tst4generic" [st|insert znork r=#{showP r}|] :: Sql db '[Raw] '[U1], r *! pcnt)
    C.runConduitRes $
      selectSourceLazyTypedC "tst6a" (mkSql "tst6a" "select stuff" :: Sql db '[Int] '[Sel Raw]) (I1 212) (rdb SS)
        C..| C.mapC unRaw
        C..| CL.chunksOf 1
        --               C..| S.mapped S.toList
        C..| C.concatMapC (map cv6a)
        C..| fit1 sc
        C..| insertConduitC "tst6a" sc ins (rdb IC)

cv6a :: [SqlValue] -> [SqlValue]
cv6a b@(SqlInt32 i : _) = SqlChar (chr (fromIntegral i + ord 'a')) : b
cv6a o = error $ "cv6a: oops: wrong number of columns: " ++ show o

type GOut a = (a, ([CCmd SType], [FOut]))

-- wrapper to make it simpler
tst4generic :: ML () m => [CCmd SType] -> ((SType -> DBFake Writeable) -> m a) -> m (GOut a)
tst4generic cmds ma = do
  c <- liftIO $ newTVarIO cmds
  (rdb, ret) <- dbfake c
  a <- ma rdb
  cs <- liftIO $ readTVarIO c
  os <- liftIO $ reverse <$> readTVarIO ret
  unless (null cs) $ $logError [st|did not process all the commands len=#{length cs} #{show cs}|]
  forM_ os $ \case
    FWarn e -> $logWarn [st|#{e}|]
    FError e -> $logError [st|#{e}|]
    _otherskip -> return ()
  return (a, (cs, os))

dbfake :: ML () m => TVCmd -> m (SType -> DBFake Writeable, TVOut)
dbfake cmd = do
  lg <- getLogger $(TH.qLocation >>= liftLoc)
  ret <- liftIO $ newTVarIO []
  let ref = Ref cmd ret lg
  return (\s -> DBFake ref (mystmt s ref) "fakedb" s, ret)

-- todo: pass in MyLogger
addRet :: TVOut -> FOut -> STM ()
addRet tv x = modifyTVar tv (x :)

addRet' :: TVOut -> FOut -> IO ()
addRet' tv = atomically . addRet tv

mystmt :: SType -> Ref -> H.Statement
mystmt txt Ref{..} =
  H.Statement
    { H.execute = \hs -> do
        refLogger LevelDebug [st|H.execute before: #{txt} hs=#{show hs}|]
        (rc, lr) <- atomically $ do
          xs <- readTVar refCmd
          case xs of
            (c@(CUpdate stype' hs' rc) : rs) -> do
              writeTVar refCmd rs
              addRet refOut (FExecuteUpdate hs rc)
              unless (hs == hs') $ addRet refOut (FWarn [st|H.execute #{show c} actual<>expected actual=#{show hs} expected=#{show hs'}|])
              unless (stype' == txt) $ addRet refOut $ FError [st|H.execute: CUpdate: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return (Left rc, Right c)
            (c@(CSelectStart stype' hs') : rs) -> do
              writeTVar refCmd rs
              addRet refOut (FExecuteSelect hs)
              unless (hs == hs') $ addRet refOut (FWarn [st|H.execute #{show c} actual<>expected actual=#{show hs} expected=#{show hs'}|])
              unless (stype' == txt) $ addRet refOut $ FError [st|H.execute: CSelectStart: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return (Right [], Right c)
            (_ : _) ->
              let e = "H.execute: oops " <> T.pack (show xs)
               in addRet refOut (FError e) >> return (Left (-97), Left e)
            [] ->
              let e = "H.execute: no cmds left!!" <> T.pack (show hs)
               in addRet refOut (FError e) >> return (Left (-98), Left e)
        case lr of
          Left e -> refLogger LevelError ("H.execute: after " <> e)
          Right a -> refLogger LevelDebug ("H.execute: after " <> T.pack (show (rc, a)))
        return rc
    , H.executeRaw = refLogger LevelError "in executeRaw" >> addRet' refOut FExecuteRaw
    , -- need to process this
      H.executeMany = \hhs -> refLogger LevelError [st|in executeMany #{txt} hhs=#{show hhs}|] >> addRet' refOut (FExecuteMany hhs)
    , H.finish = do
        (ret, lr) <- atomically $ do
          xs <- readTVar refCmd
          case xs of
            (CFinish stype' : rs) -> do
              addRet refOut FFinish
              writeTVar refCmd rs
              unless (stype' == txt) $ addRet refOut $ FError [st|H.finish: CFinish: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return ((), Right ())
            _unexpected -> do
              let e = "expected a CFinish but found!! " <> T.pack (show xs)
              addRet refOut (FError e)
              return ((), Left e)
        case lr of
          Left e -> refLogger LevelError [st|H.finish: #{txt} e=#{e}|]
          Right a -> refLogger LevelDebug [st|H.finish: #{txt} #{show (ret,a)}|]
        return ret
    , H.fetchRow = do
        (ret, lr) <- atomically $ do
          xs <- readTVar refCmd
          case xs of
            (c@CUpdate{} : _) ->
              let e = "no rows to fetch cos update " <> T.pack (show c)
               in addRet refOut (FError e) >> return (Nothing, Left e)
            (CSelect stype' hs : rs) -> do
              addRet refOut (FFetchRow (Just hs))
              writeTVar refCmd rs
              unless (stype' == txt) $ addRet refOut $ FError [st|H.fetchRow: CSelect: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return (Just hs, Right ())
            (CSelectEnd stype' : rs) -> do
              addRet refOut (FFetchRow Nothing)
              writeTVar refCmd rs
              unless (stype' == txt) $ addRet refOut $ FError [st|H.fetchRow: CSelectEnd: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return (Nothing, Right ())
            (_ : _) ->
              let e = "H.fetchRow: oops " <> T.pack (show xs)
               in addRet refOut (FError e) >> return (Nothing, Left e)
            [] ->
              let e = "H.fetchRow: no cmds left!!"
               in addRet refOut (FError e) >> return (Nothing, Left e)
        case lr of
          Left e -> refLogger LevelError [st|H.fetchRow #{txt} e=#{e}|]
          Right a -> refLogger LevelDebug [st|H.fetchRow #{txt} #{show (ret,a)}|]
        return ret
    , H.getColumnNames = refLogger LevelError "in getColumnNames" >> addRet' refOut (FGetColumnNames []) >> return ["cola", "colb"]
    , H.originalQuery = "in originalQuery: select abcd from xyzw"
    , H.describeResult = refLogger LevelError "in describeResult" >> addRet' refOut (FDescribeResult []) >> return []
    , -- can catch errors if a select treated as an update!
      -- not sure how to deal with multiple resultsets
      -- also will not see empty selects
      H.nextResultSet = do
        (ret, lr) <- atomically $ do
          xs <- readTVar refCmd
          case xs of
            (CEnd stype' : rs) -> do
              addRet refOut (FNextResultSet Nothing)
              writeTVar refCmd rs
              unless (stype' == txt) $ addRet refOut $ FError [st|H.nextResult: CEnd: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return (Nothing, Right ())
            _unexpected -> do
              let e = "nextResultSet expected a CEnd but found!! " <> T.pack (show xs)
              addRet refOut (FError e)
              return (Nothing, Left e)
        case lr of
          Left e -> refLogger LevelError [st|H.nextResultSet #{txt} e=#{e}|]
          Right a -> refLogger LevelDebug [st|H.nextResultSet #{txt} #{show (ret,a)}|]
        return ret
    }

fakeconn :: SType -> Ref -> H.Statement -> HC.Connection
fakeconn txt ref@Ref{..} stmt =
  HC.Connection
    { HC.getQueryInfo = \s -> refLogger LevelInfo [st|in getQueryInfo s=#{s}|] >> addRet' refOut (FError "HC.getQueryInfo") >> return ([], [])
    , HC.disconnect = do
        (ret, lr) <- atomically $ do
          xs <- readTVar refCmd
          case xs of
            (CDisc stype' : rs) -> do
              addRet refOut FCommit
              writeTVar refCmd rs
              unless (stype' == txt) $ addRet refOut $ FError [st|HC.disconnect: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return ((), Right ())
            _unexpected -> do
              let e = "HC.disconnect: expected a CDisc but found!! " <> T.pack (show xs)
              addRet refOut (FError e)
              return ((), Left e)
        case lr of
          Left e -> refLogger LevelError [st|H.commit #{txt} e=#{e}|]
          Right a -> refLogger LevelDebug [st|H.commit #{txt} #{show (ret,a)}|]
        return ret
    , HC.commit = do
        (ret, lr) <- atomically $ do
          xs <- readTVar refCmd
          case xs of
            (CCommit stype' : rs) -> do
              addRet refOut FCommit
              writeTVar refCmd rs
              unless (stype' == txt) $ addRet refOut $ FError [st|HC.commit: stype dont match expected=#{stype'} found=#{txt} cmds=#{show xs}|]
              return ((), Right ())
            _unexpected -> do
              let e = "commit: expected a CCommit but found!! " <> T.pack (show xs)
              addRet refOut (FError e)
              return ((), Left e)
        case lr of
          Left e -> refLogger LevelError [st|H.commit #{txt} e=#{e}|]
          Right a -> refLogger LevelDebug [st|H.commit #{txt} #{show (ret,a)}|]
        return ret
    , HC.rollback = refLogger LevelError [st|HC.rollback #{txt}|] >> addRet' refOut (FError "HC.rollback")
    , HC.run = \s hs -> refLogger LevelInfo [st|HC.run #{txt} s=#{s} hs=#{show hs}|] >> addRet' refOut (FError "HC.run") >> return (Left 0)
    , HC.prepare = \s -> refLogger LevelInfo [st|HC.prepare #{txt} s=#{s}|] >> return stmt
    , HC.clone = refLogger LevelError [st|HC.clone #{txt}|] >> addRet' refOut (FError "HC.clone") >> return (fakeconn txt ref stmt)
    , HC.hdbcDriverName = "in hdbcDriverName"
    , HC.hdbcClientVer = "in hdbcClientVer"
    , HC.proxiedClientName = "in proxiedClientName"
    , HC.proxiedClientVer = "in proxiedClientVer"
    , HC.dbServerVer = "in dbServerVer"
    , HC.dbTransactionSupport = True -- ???
    , HC.getTables = refLogger LevelInfo [st|HC.getTables #{txt}|] >> addRet' refOut (FError "HC.getTables") >> return ["atable", "btable"]
    , HC.describeTable = \s -> refLogger LevelInfo [st|HC.describeTable #{txt} s=#{s}|] >> addRet' refOut (FError "HC.describeTable") >> return []
    , HC.setAutoCommit = \b -> refLogger LevelInfo [st|HC.setAutoCommit #{txt} b=#{show b}|] >> addRet' refOut (FError "HC.setAutoCommit") >> return False
    }

instance ToText (DBFake a) where
  toText = fromText . T.pack . show
instance Show (DBFake a) where
  show DBFake{..} = "DBFake (" ++ T.unpack _fInfo ++ " SType=" ++ show _fStype ++ ")"

instance DConn (DBFake a) where
  connList db = [("Driver", "nonexistent"), ("db", [st|dummy driver: not used #{show db}|])]
  getDbDefault _ = ''DBFake
  showDb db = [st|showDb: #{show db}|]
  getSchema = const Nothing
  getDb _ = Nothing
  getDelims _ = pure ('[', ']')

instance GConn (DBFake a) where
  loadConnTH _ _ = fail "loadConnTH is not defined for DBFake"
  getConn db@DBFake{..} = putStrLn ("opening fake connection " ++ show db) >> return (fakeconn _fStype _fRef _fStmt)
  getAllTablesSql _ = error "dbfake:getAllTablesSql" -- Sql [st|not sure ...|]
  getAllViewsSql _ = error "dbfake:getAllViewsSql" -- Sql [st|not sure ...|]
  existsTableSql db table = error $ "dbfake:existsTableSql " ++ show (db, table) -- Sql [st|not sure ...|]
  dropTableIfExistsSql db table = error $ "dbfake:dropTableIfExistsSql " ++ show (db, table) -- Sql [st|not sure ...|]
  dropViewIfExistsSql db table = error $ "dbfake:dropViewIfExistsSql " ++ show (db, table) -- Sql [st|not sure ...|]
  getColumnMetaSql db table = error $ "dbfake:getColumnMetaSql " ++ show (db, table)
  translateColumnMeta _ _ = error "dbfake: translateColumnMeta"
  limitSql _ _ = error "dbfake: limitSql"
