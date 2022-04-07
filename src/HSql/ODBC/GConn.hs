{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.ODBC.GConn
Description : Contains GConn class
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Each rdbms type needs to implement GConn
-}
module HSql.ODBC.GConn (
  module HSql.ODBC.GConn,
  module Database.Util,
) where

import Control.Arrow
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Either
import Data.Generics.Product
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Maybe
import Data.Pos
import Data.Proxy
import Data.Semigroup.Foldable (intercalate1)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time
import Data.Vinyl
import qualified Database.HDBC.ODBC as H
import Database.Util
import qualified Dhall as D (FromDhall, auto, input)
import DocUtils.Condition
import DocUtils.Doc
import DocUtils.Time
import qualified Formatting.Buildable as FF
import qualified GHC.Generics as G (Generic)
import GHC.Stack (HasCallStack)
import qualified Generics.SOP as GS (Generic, HasDatatypeInfo)
import HSql.Core
import HSql.Core.TablePrinter (FromCell (..))
import HSql.Core.VinylUtils
import qualified Language.Haskell.TH as TH (Exp, Q)
import Primus.Error
import Primus.NonEmpty
import Text.Shakespeare.Text
import Prelude hiding (FilePath)

-- | load dhall connection configuration using the key
loadConn :: forall a. D.FromDhall a => Text -> IO a
loadConn key = D.input D.auto ("let x = ./conn.dhall in x." <> key)

-- | writeable sql marker
data Writeable

-- | readonly sql marker
data ReadOnly

{- | support for various schema types across rdbmss'
 if a table has connschema then will grab the schema from the connection
-}
data Schema = ConnSchema | Schema !(Maybe TName) deriving stock (Show, Eq, Ord, G.Generic)

instance NFData Schema

-- | sql table definition
data Table a = Table
  { tDb :: !(Maybe TName)
  , tSchema :: !Schema
  , tName :: !TName
  , tTable :: !Bool
  }
  deriving stock (Show, Eq, Ord, Functor, G.Generic)

instance FF.Buildable (Table a) where
  build = TLB.fromText . showTable

instance NFData a => NFData (Table a)

instance GS.Generic (Table a)
instance GS.HasDatatypeInfo (Table a)

instance FromCell Schema where
  fromCell o iss lr = fromCell o (iss |> (1, "Schema")) lr . show

instance FromCell (Table a) where
  fromCell o iss lr = fromCell o (iss |> (1, "Table")) lr . T.unpack . showTable

instance GConn a => IsString (Table a) where
  fromString ss =
    case parseTableLR ss of
      Left e -> normalError $ "Table:fromString:" ++ e
      Right a -> a

-- | get a temporary table name using current date and a prefix
getTempTableName :: Table db -> IO (Table db)
getTempTableName t = do
  tm <- getUtcTimeCorrected
  return $ t & the @"tName" . tNameRawLens <>~ ('_' :| formatUtc tm)

-- | parse a mssql create table definition
parseTableLR :: forall a. (HasCallStack, GConn a) => String -> Either String (Table a)
parseTableLR ss =
  case tableParser (getDelims (Proxy @a)) (T.pack ss) of
    Left e -> Left $ "parseTableLR: failed to parse[" ++ ss ++ "] e=" ++ e
    Right (TNames ma mb c) -> Right $ Table ma (maybe ConnSchema (Schema . Just) mb) c True

instance ToText (Table a) where
  toText = fromText . showTable

-- bearbeiten: does this make sense for eg mysql and oracle

-- | render a table as text for use in sql expressions
showTable :: Table a -> Text
showTable t =
  (<> showTName (tName t)) $ case (tDb t, tSchema t) of
    (Nothing, Schema Nothing) -> mempty
    (Nothing, ConnSchema) -> mempty
    (Just a, Schema Nothing) -> showTName a <> ".."
    (Just a, ConnSchema) -> showTName a <> ".."
    (Nothing, Schema (Just b)) -> showTName b <> "."
    (Just a, Schema (Just b)) -> showTName a <> "." <> showTName b <> "."

-- | writeable constraint
type GConnWrite db = (WriteableDB db ~ 'True, GConn db)

-- | creates a database connection
getConn' ::
  (HasCallStack, GConn a) =>
  [(Text, Text)] ->
  a ->
  IO H.Connection
getConn' odbcparams db =
  let cs = T.strip $ connText db
      lastchar = case T.unsnoc cs of
        Nothing -> normalError $ T.unpack [st|getConn': connText is empty! odbcparams=#{psi odbcparams} db=#{db}|]
        Just (_, a) -> a
      ret = case odbcparams of
        [] -> mempty
        _ : _ ->
          let xs = T.intercalate ";" (map (\(a, b) -> a <> "=" <> b) odbcparams)
           in if lastchar == ';'
                then xs
                else T.cons ';' xs
   in H.connectODBC (T.unpack (cs <> ret))

-- | 'GConn' is the central class to this package. Each database type needs to implement this.
class (DConn a, ToText a) => GConn a where -- Show a was causing infinite loop on compile if we omit MyLogger: to do with Streaming undecidableinstances and show instance for the stream

  -- | given a key it loads a Template Haskell expression for the database connection
  loadConnTH :: p a -> Text -> TH.Q TH.Exp

  getConn :: a -> IO H.Connection
  getConn = getConn' []

  -- | the sqlite odbc driver misbehaves so we need to ignore the disconnect error
  ignoreDisconnectError :: proxy a -> Bool
  ignoreDisconnectError _ = False

  -- | lists each table in a given database
  getAllTablesSql :: a -> Sql a '[] '[Sel (Table a)]

  -- | lists each view in a given database
  getAllViewsSql :: a -> Sql a '[] '[Sel (Table a)]

  -- | does the table exist
  existsTableSql :: a -> Table a -> Sql a '[] '[SelRowCol Text]

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

-- | type level record describing a sql table
type GetAllTablesCount a = F '["name" ::: Table a, "size" ::: Int, "created" ::: Maybe UTCTime, "updated" ::: Maybe UTCTime]

-- | metadata types
data ColDataType
  = CFixedString
  | CString
  | CInt
  | CDateTime
  | CDate
  | CFloat
  | CBool
  | CBinary
  | CCLOB
  | CBLOB
  | COther !Text
  deriving stock (Show, Eq, Ord, G.Generic)

instance NFData ColDataType

instance FromCell ColDataType where
  fromCell o iss lr = fromCell o (iss |> (1, "ColDataType")) lr . show

-- | column meta data
data ColumnMeta = ColumnMeta
  { cName :: !Text
  , cType :: !Text
  , cIsNull :: !Bool
  , cLength :: !Int
  , cPrecision :: !(Maybe Int)
  , cScale :: !(Maybe Int)
  , cComputed :: !Bool
  , cIdentity :: !Bool
  , cPkey :: !Int
  }
  deriving stock (Show, Eq, G.Generic)

instance NFData ColumnMeta

instance GS.Generic ColumnMeta
instance GS.HasDatatypeInfo ColumnMeta

instance DefDec (Dec ColumnMeta) where
  defDec = defD9 ColumnMeta

-- | cast a table to another connection
unsafeCastTable :: GConn b => b -> Table a -> Table b
unsafeCastTable db Table{..} = Table Nothing (Schema (toTNameUnsafe <$> getSchema db)) tName True

-- | cast a table to another connection and including the database name from the connection
unsafeCastTableWithDB :: GConn b => b -> Table a -> Table b
unsafeCastTableWithDB db Table{..} = Table (toTNameUnsafe <$> getDb db) (Schema (toTNameUnsafe <$> getSchema db)) tName True

{- | display table
showTable :: GConn a => Table a -> Text
showTable t = showTableImpl t
-}

-- | escape a field using rdbms delimiters
escapeField :: GConn a => p a -> Text -> Text
escapeField p fld =
  let (b, e) = N.head (getDelims p)
   in T.singleton b <> fld <> T.singleton e

-- | dropped table text
dropped :: Text
dropped = "Dropped"

-- | not found table text
notfound :: Text
notfound = "NotFound"

-- | found table text
found :: Text
found = "Found"

-- | get effective schema name if present
getEffectiveSchema :: GConn a => a -> Table a -> Maybe Text
getEffectiveSchema db t =
  case tSchema t of
    ConnSchema -> getSchema db
    Schema a -> showTName <$> a

-- | get effective table name
getEffectiveTable :: GConn a => a -> Table a -> Table a
getEffectiveTable db t =
  case tSchema t of
    ConnSchema -> t{tSchema = Schema (toTNameUnsafe <$> getSchema db)}
    Schema _ -> t

instance GConn a => DefDec (Dec (Table a)) where
  defDec = decGeneric

instance GConn a => Conv (Table a) where
  conv xs =
    conv @String xs >>= \x -> case parseTableLR x of
      Right y -> return y
      Left e -> failCE "Table" e xs

instance DefEnc (Enc (Table a)) where
  defEnc = encTable

-- | sql encoder for the table name
encTable :: Enc (Table a)
encTable = Enc $ \t -> [SqlString (T.unpack (showTable t))]

-- | parse a create table definition and pull out the table name and columns
parseCreateTableSql :: GConn db => Sql db a b -> VE (Table db, NonEmpty Text)
parseCreateTableSql = fmap (((toTableName . T.unpack) *** fromList1 "parseCreateTableSql") . getInsertableFields) . createTable . sSql

-- | create an insert statement based on create table sql
insertTableSqlAuto :: GConn db => Sql db a b -> VE (ISql db a '[Upd])
insertTableSqlAuto s =
  parseCreateTableSql s
    <&> \(tab, cols) rows ->
      let (x, i) = insertTableSqlPrivate (rows, lengthP cols) tab
       in (Sql (sDescription s) (sEncoders s) (E1 UpdP) x, i)

-- | create a multi-insert statement with placeholders for the number of columns and rows
insertTableSqlPrivate :: (Pos, Pos) -> Table db -> (Text, Pos)
insertTableSqlPrivate (r, c) tab =
  ([st|insert into #{tab} values #{qqrc (r,c)}|], r *! c)

{- | generates the insert statement based on sql create statement (named columns)
   all two must match in column order:
     create statement in the code
     adt in the code
-}
insertTableSqlNamedAuto :: GConn db => Sql db a b -> VE (ISql db a '[Upd])
insertTableSqlNamedAuto s =
  parseCreateTableSql s
    <&> \(tab, cols) rows ->
      let (x, i) = insertTableSqlNamedPrivate (rows, lengthP cols, intercalate1 "," cols) tab
       in (Sql (sDescription s) (sEncoders s) (E1 UpdP) x, i)

-- | create a multi-insert statement with placeholders for the number of columns and rows
insertTableSqlNamedPrivate :: (Pos, Pos, Text) -> Table db -> (Text, Pos)
insertTableSqlNamedPrivate (r, c, colsText) tab =
  ([st|insert into #{tab} (#{colsText}) values #{qqrc (r,c)}|], r *! c)

-- | unsafe convert a string to a 'Table'
toTableName :: GConn a => String -> Table a
toTableName = fromString . trim

-- | display schema name
showSchema :: Schema -> Text
showSchema ConnSchema = "ConnSchema"
showSchema (Schema (maybe "" showTName -> s)) = s
