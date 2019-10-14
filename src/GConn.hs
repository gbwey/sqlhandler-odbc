{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS -Wall #-}
{-|
Module      : GConn
Description : Contains GConn class
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Each database type needs to implement GConn
-}
module GConn where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import qualified Data.Text as T
import Data.Text (Text)
import qualified Database.HDBC.ODBC as H
import Database.HDBC (SqlValue(..))
import Data.Maybe
import Data.Text.Lazy.Builder (fromText)
import Data.Proxy
import Control.Arrow
import Data.String
import Data.Time
import SqlParser
import Control.Lens hiding (at, (<.>), (:>))
import Text.Regex.Applicative (RE,(=~))
import Sql
import TablePrinter (FromField(..))
import qualified Generics.SOP as GS
import qualified GHC.Generics as G
import GHC.Stack
import Data.Vinyl
import qualified Language.Haskell.TH as TH
import Dhall hiding (maybe,string)
import Data.List
import Data.Char

loadConn :: forall a . Interpret a => Text -> IO a
loadConn key = input auto ("let x = ./conn.dhall in x." <> key)

data Writeable
data ReadOnly

data Schema = ConnSchema | Schema !(Maybe Text) deriving (Show,Eq,Ord,G.Generic)
$(makePrisms ''Schema)

data Table a = Table {
                 _tDb :: !(Maybe Text)
               , _tSchema :: !Schema
               , _tName :: !Text
               , _tTable :: !Bool
               } deriving (Show, Eq, Ord, Functor, G.Generic)

instance GS.Generic (Table a)
instance GS.HasDatatypeInfo (Table a)

instance FromField Schema where
  fromField = (:[]) . show

makeLenses ''Table

instance GConn a => FromField (Table a) where
  fromField = (:[]) . T.unpack . showTable

instance GConn a => IsString (Table a) where
  fromString ss =
    let mdelims = getDelims (Proxy @a)
    in case parseTableLR ss of -- should be fail?? and not use IsString?
      Left _ -> Table Nothing ConnSchema (stripQuotes mdelims (T.strip (T.pack ss))) True
      Right a -> a

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseTableLR :: forall a. GConn a => String -> Either String (Table a)
parseTableLR ss' =
    let mdelims = getDelims (Proxy @a)
        ss = trim ss'
    in maybe (Left ("parseTableLR: failed to parse[" ++ ss' ++ "]")) Right (ss =~ pp1 mdelims)

pp1 :: Maybe (Char, Char) -> RE Char (Table a)
pp1 mdelims = (\(ma, b, c) -> Table ma (Schema b) c True) <$> tableParser mdelims

instance GConn a => ToText (Table a) where
  toText = fromText . showTable

-- bearbeiten: does this make sense for eg mysql and oracle
showTableImpl :: Maybe (Char,Char) -> Table a -> Text
showTableImpl mq Table {..} =
  let (q1,q2) = maybe (mempty,mempty) (T.singleton *** T.singleton) mq
      q0 = case (_tDb,_tSchema) of
             (Nothing, Schema Nothing) -> ""
             (Nothing, ConnSchema) -> ""
             (Just a, Schema Nothing)  -> a <> ".."
             (Just a, ConnSchema)  -> a <> ".."
             (Nothing, Schema (Just b))  -> b <> "."
             (Just a, Schema (Just b))   -> a <> "." <> b <> "."
  in q0 <> q1 <> _tName <> q2

type GConnWrite db = (WriteableDB db ~ 'True, GConn db)

-- todo: provide runSqlRawE that allows you to pass in extra odbc params or just use runSqlRawI that allows you to pass in the connection!
-- | creates a database connection
getConn' :: GConn a
      => [(Text, Text)]
      -> a
      -> IO H.Connection
getConn' odbcparams db =
  let cs = connText db
      ret = case odbcparams of
              [] -> mempty
              _ -> let xs = T.intercalate ";" (map (\(a,b) -> a <> "=" <> b) odbcparams)
                   in (if T.last cs == ';' then "" else ";") <> xs
  in H.connectODBC (T.unpack (cs <> ret))

-- | 'GConn' is the central class to this package. Each database type needs to implement this.
class ToText a => GConn a where  -- Show a was causing infinite loop on compile if we omit MyLogger: to do with Streaming undecidableinstances and show instance for the stream
  -- | given a key it loads a Template Haskell expression for the database connection
  loadConnTH :: p a -> Text -> TH.Q TH.Exp
  getConn :: a -> IO H.Connection
  getConn = getConn' []
  -- | return the odbc connection string
  connText :: a -> Text
  -- | the sqlite odbc driver misbehaves so we need to ignore the disconnect error
  -- todo: is this still a problem (still on windows but need to test)
  ignoreDisconnectError :: proxy a -> Bool
  ignoreDisconnectError _ = False
  -- | c# connection string
  connCSharpText :: a -> String
  showDb :: a -> Text
  getDb :: a -> Maybe Text
  getSchema :: a -> Maybe Text
  -- | start and end deimiters for each database type
  getDelims :: proxy a -> Maybe (Char, Char)
  -- | lists each table in a given database
  getAllTablesSql :: a -> Sql a '[] '[Sel (Table a)]
  -- | lists each view in a given database
  getAllViewsSql :: a -> Sql a '[] '[Sel (Table a)]
  -- | does the table exist
  existsTableSql :: a -> Table a -> Sql a '[] '[SelOne Text]
  -- | drops the table if it exists
  dropTableIfExistsSql :: a -> Table a -> Sql a '[] '[Upd]
  -- | drops the view if it exists
  dropViewIfExistsSql :: a -> Table a -> Sql a '[] '[Upd]
  -- | returns column metadata for a table -- ignores the database!
  getColumnMetaSql :: a -> Table a -> (ColumnMeta -> ColDataType, Sql a '[] '[Sel ColumnMeta])
  translateColumnMeta :: HasCallStack => p a -> (ColDataType, ColumnMeta) -> Text
  -- | optional fast way to get list of tables and rowcounts
  getAllTablesCountSql :: proxy a -> Maybe (Sql a '[] '[Sel (GetAllTablesCount a)])
  getAllTablesCountSql = const Nothing
  -- | limit clause per database. eg rownum for oracle / limit for postgres / top for mssql
  limitSql :: p a -> Maybe Int -> Text
  -- | Template haskell name for this database type
  getDbDefault :: p a -> TH.Name

type GetAllTablesCount a = F '["name" ::: Table a, "size" ::: Int, "created" ::: Maybe UTCTime, "updated" ::: Maybe UTCTime]

-- | metadata types
data ColDataType =
   CFixedString
 | CString
 | CInt
 | CDateTime
 | CDate
 | CFloat
 | CBool
 | CBinary
 | CCLOB
 | CBLOB
 | COther !Text deriving (Show, Eq, Ord)

instance FromField ColDataType where
  fromField = (:[]) . show

data ColumnMeta = ColumnMeta {
   cName :: !Text
 , cType :: !Text
 , cIsNull :: !Bool
 , cLength :: !Int
 , cPrecision :: !(Maybe Int)
 , cScale :: !(Maybe Int)
 , cComputed :: !Bool
 , cIdentity :: !Bool
 , cPkey :: !Int
 } deriving (Show, Eq, G.Generic)

instance GS.Generic ColumnMeta
instance GS.HasDatatypeInfo ColumnMeta

instance DefDec (Dec ColumnMeta) where
  defDec = defD9 ColumnMeta

unsafeCastTable :: GConn b => b -> Table a -> Table b
unsafeCastTable db Table {..} = Table Nothing (Schema (getSchema db)) _tName True

unsafeCastTableWithDB :: GConn b => b -> Table a -> Table b
unsafeCastTableWithDB db Table {..} = Table (getDb db) (Schema (getSchema db)) _tName True

showTable :: GConn a => Table a -> Text
showTable t = showTableImpl (getDelims t) t

showTableForBCP :: Table a -> String
showTableForBCP = T.unpack . showTableImpl Nothing

escapeField :: GConn a => p a -> Text -> Text
escapeField p fld =
  case getDelims p of
    Just (b,e) -> T.singleton b <> fld <> T.singleton e
    Nothing -> fld

dropped, notfound, found :: Text
dropped = "Dropped"
notfound = "NotFound"
found = "Found"

getEffectiveSchema :: GConn a => a -> Table a -> Maybe Text
getEffectiveSchema db t =
  case _tSchema t of
    ConnSchema -> getSchema db
    Schema a -> a

getEffectiveTable :: GConn a => a -> Table a -> Table a
getEffectiveTable db t =
  case _tSchema t of
    ConnSchema -> t { _tSchema = Schema (getSchema db) }
    Schema _ -> t

instance GConn a => DefDec (Dec (Table a)) where
  defDec = decGeneric

instance GConn a => Conv (Table a) where
  conv xs = conv @String xs >>= \x -> case parseTableLR x of
                                        Right y -> return y
                                        Left e -> failCE "Table" e xs

instance GConn a => DefEnc (Enc (Table a)) where
  defEnc = encTable

encTable :: GConn a => Enc (Table a)
encTable = Enc $ \t -> [SqlString (T.unpack (showTable t))]

cntField :: PType -> Int
cntField = \case
  PIdentity {} -> 0
  PColumn {} -> 1
  PConstraint {} -> 0
  POther {} -> 0

getField3 :: PType -> [String]
getField3 = \case
  PIdentity _ _ s -> [s]
  PColumn _ _ s -> [s]
  PConstraint {} -> []
  POther {} -> []

getField1 :: PType -> [String]
getField1 = \case
   PIdentity s _ _ -> [s]
   PColumn s _ _ -> [s]
   PConstraint {} -> []
   POther {} -> []

parseCreateTableSql :: GConn db => Sql db a b -> Either Text (Table db, [PType])
parseCreateTableSql (T.unpack . _sSql -> s) =
  fmap (\(PTable t xs) -> (toTableName t, xs)) (parseCreateTableSqlImpl s)

-- uses Upd with a predicate for the number of rows that need to be inserted
insertTableSqlAuto :: GConn db => Sql db a b -> Either Text (ISql db a '[Upd])
insertTableSqlAuto s =
  parseCreateTableSql s <&>
   \(tab,flds) r ->
      let (x,i) = insertTableSqlPrivate (r, sum (map cntField flds)) tab
      in (Sql (_sDescription s) (_sEncoders s) (E1 (ueq r)) x, i)

insertTableSqlPrivate :: GConn db => (Int,Int) -> Table db -> (Text, Int)
insertTableSqlPrivate (r,c) tab =
  ([st|insert into #{tab} values #{qqrc (r,c)}|], r*c)

toTableName :: GConn a => String -> Table a
toTableName = fromString . trim

createTableFields' :: GConn db => Sql db a b -> (Table db, [String])
createTableFields' x = getCreateTableFields x ^?! _Right

countCreateTableFields :: GConn db => Sql db a b -> Either Text Int
countCreateTableFields x = getCreateTableFields x & _Right %~ length . snd

getCreateTableFields' :: GConn db => Sql db a b -> [String]
getCreateTableFields' x = getCreateTableFields x ^?! _Right . _2

getCreateTableFields :: GConn db => Sql db a b -> Either Text (Table db, [String])
getCreateTableFields s =
  parseCreateTableSql s & _Right . _2 %~ concatMap getField1

commonFields :: Maybe Text -> Text -> Text
commonFields mpref sql =
  let ys = mapM (matchSqlField mpref) (lines $ T.unpack sql)
  in case ys of
       Left e -> error $ "commonFields: failed to extract fields " <> T.unpack e
       Right zs -> T.pack (unlines (concatMap getField3 $ concat zs))

showSchema :: Schema -> Text
showSchema ConnSchema = "ConnSchema"
showSchema (Schema (fromMaybe "" -> s)) = s
