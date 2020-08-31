{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- need this if not using a proxy but using only type applications [eg createFrameSql]
{- |
Module      : HSql.ODBC.DBFrame
Description : Convenience methods for reading and writing a frame to a database
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com
-}
module HSql.ODBC.DBFrame where
import Data.Time (UTCTime,ZonedTime,LocalTime,Day)
import Prelude hiding (FilePath)
import Text.Shakespeare.Text (st)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (forM_)
import Data.Proxy (Proxy(..))
import HSql.ODBC.DBConn
import HSql.Core.Sql
import HSql.Core.Encoder (DefEnc,Enc)
import HSql.Core.VinylUtils
import Data.Vinyl
import qualified Data.Vinyl.Functor as V (Identity)
import GHC.TypeLits
import qualified Frames as F
import Logging

-- U0 works in Mssql but in postgres it use a random number so have to make it Upd
-- | 'createFrameSql' generates a sql statement to create a table based on the fieldnames and types in the proxy
createFrameSql :: forall db rs . (ToMetas rs, GConnWrite db)
  => Table db -> Sql db '[] '[Upd]
createFrameSql = createDBTableFromSql (toMetas (Proxy @rs))
{-
>createFrameSql @(DBMS Writeable) @'["aa" ::: Int] "fred"
create table [fred]
                (
                  [aa] bigint not null
                )

it :: Sql (DBMS Writeable) '[] '[Upd]
-}

-- | 'insertFrameSql' generates sql insert statements for the list of vinyl records
insertFrameSql :: forall db rs t . (DefEnc (Rec Enc (Unlabeled rs)), F.ColumnHeaders rs, GConnWrite db)
   => Table db -> t (F rs) -> Sql db (Unlabeled rs) '[U1]
insertFrameSql tab _ =
  let cnames = F.columnHeaders (Proxy @(F rs))
      len = length cnames
      flds = T.intercalate "," $ map (escapeField tab . T.pack) cnames
      tt = escapeField tab (_tName tab)
  in mkSql [st|insertFrameSql #{tab}|] [st|insert into #{tt} (#{flds}) values#{qqsn len}|]

-- | 'insertFrameLoad' loads a table with the frame using the names and types from the frame
insertFrameLoad :: forall db m e rs t
   . (Foldable t, ML e m, ToMetas rs, DefEnc (Rec Enc (Unlabeled rs)), F.ColumnHeaders rs, RecordToList (Unlabeled rs), ReifyConstraint Show V.Identity (Unlabeled rs), StripFieldNames rs, RMap (Unlabeled rs), GConnWrite db)
    => CreateTable
    -> db
    -> Table db
    -> t (F rs)
    -> m ()
insertFrameLoad cre db tab ff = do
  case cre of
    DropCreateTable -> runSql_ db RNil $ createFrameSql @db @rs tab -- could also drop the table ... but dangerous
    CreateTable -> runSql_ db RNil $ createFrameSql @db @rs tab
    SkipCreate -> return ()
  let ins = insertFrameSql tab ff
  withDB db $ \conn ->
    forM_ ff $ \row ->
        runSqlI conn (stripNames row) ins

-- useful if you want to get rs as the proxy cos nested another layer: Frame -> Record -> Rec ElField rs
proxyFrameToRs :: p (F rs) -> Proxy rs
--proxyFrameToRs :: Frame (F rs) -> Proxy rs
proxyFrameToRs _ = Proxy
{-
-- tricky cos if we use a Frame then will be a Proxy Record so we need an instance of ToMetas for Record
type FrameRec rs = Frame (Record rs)
type Record   rs = FieldRec rs
type FieldRec rs = Rec ElField rs

Frame (F rs) is the equivalent of FrameRec
-}

-- can use a straight Frame or Proxy @'["aa" :-> Bool, "bb" :-> Double]
-- | 'ToMetas' generates metadata information from the names and types in 'a'
class ToMetas (a :: k) where -- k means you need polykinds: can leave out the 'k' but must have polykinds
  toMetas :: p a -> [(ColDataType,ColumnMeta)]
instance ToMetas '[] where
  toMetas _ = []
instance (ToMeta t, ToMetas ts) => ToMetas (t ': ts) where
  toMetas _ = toMeta (Proxy @t) : toMetas (Proxy @ts)
instance ToMetas ts => ToMetas (F.Record ts) where
  toMetas _ = toMetas (Proxy @ts)

-- | 'ToMeta' generates metadata information from a single column
class ToMeta a where -- could specify a :: (Symbol,*) but that is too specific: let ghc figure it out with polykinds
-- tried with a::k but could not get type application to work
  toMeta :: p a -> (ColDataType,ColumnMeta)
instance KnownSymbol s => ToMeta '(s, Int) where
  toMeta _ = (CInt, ColumnMeta (T.pack (symbolVal (Proxy @s))) "Int" False 10 Nothing Nothing False False 0)
instance KnownSymbol s => ToMeta '(s, Integer) where
  toMeta _ = toMeta @'(s, Int) Proxy
instance KnownSymbol s => ToMeta '(s, Double) where
  toMeta _ = (CFloat, ColumnMeta (T.pack (symbolVal (Proxy @s))) "Double" False 10 Nothing Nothing False False 0)
instance KnownSymbol s => ToMeta '(s, Float) where
  toMeta _ = toMeta @'(s, Double) Proxy
instance KnownSymbol s => ToMeta '(s, String) where
  toMeta _ = (CString, ColumnMeta (T.pack (symbolVal (Proxy @s))) "String" False 1000 Nothing Nothing False False 0)
instance KnownSymbol s => ToMeta '(s, Text) where
  toMeta _ = toMeta @'(s, String) Proxy
instance KnownSymbol s => ToMeta '(s, UTCTime) where
  toMeta _ = (CDateTime, ColumnMeta (T.pack (symbolVal (Proxy @s))) "UTCTime" False 10 Nothing Nothing False False 0)
instance KnownSymbol s => ToMeta '(s, ZonedTime) where
  toMeta _ = toMeta @'(s, UTCTime) Proxy
instance KnownSymbol s => ToMeta '(s, LocalTime) where
  toMeta _ = toMeta @'(s, UTCTime) Proxy
instance KnownSymbol s => ToMeta '(s, Day) where
  toMeta _ = (CDate, ColumnMeta (T.pack (symbolVal (Proxy @s))) "Day" False 10 Nothing Nothing False False 0)
instance KnownSymbol s => ToMeta '(s, Bool) where
  toMeta _ = (CBool, ColumnMeta (T.pack (symbolVal (Proxy @s))) "Bool" False 10 Nothing Nothing False False 0)
instance (ToMeta '(s, a)) => ToMeta '(s, Maybe a) where
  toMeta _ = let (x,y) = toMeta @'(s, a) Proxy
             in (x, y { cIsNull = True })
