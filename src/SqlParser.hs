{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{- |
Module      : SqlParser
Description : simple utilities for parsing mssql statements
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com
-}
module SqlParser
 (
 parseCreateTableSqlImpl
-- ,validName
 ,matchBlankSqlRE
 ,matchSqlFieldRE
 ,matchSqlField
 ,matchBcp
 ,matchBcps
 ,tableParser
 ,stripQuotes
 ,PTable(..)
 ,PType(..)
 ) where

import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.IO.Class
import System.IO
import System.Directory
import Data.List
import Data.Char
import Data.Maybe
import Control.Applicative
import Text.Regex.Applicative
import Data.Foldable
import Data.Function
--import RegexHelper

space :: RE Char Char
space = psym isSpace

spaces, spaces1 :: RE Char String
spaces = many space
spaces1 = some space

symCI :: Char -> RE Char Char
symCI c = psym (on (==) toLower c)

-- case insenstive match on a string
stringCI :: String -> RE Char String
stringCI = traverse symCI

validName :: RE Char String
validName = (:) <$> psym isAlpha <*> many (psym (\c -> isAlphaNum c || c `elem` ("-_[]" :: String)))

-- only matches a line at a time and not nested comments
-- | matches a blank line
matchBlankSqlRE :: RE Char ()
matchBlankSqlRE = () <$ (spaces <* optional "," <* spaces <* (spaces <|> (("--" <|> "/*") <* many anySym)))

-- | matches a table column definition
matchSqlFieldRE :: Maybe Text -> RE Char (String, String, String)
matchSqlFieldRE mprefix =
  (\a b c d e -> (d, e, concat [a,b,c,maybe mempty ((<> ".") . T.unpack) mprefix, d]))
         <$> spaces
         <*> (fromMaybe mempty <$> optional ",")
         <*> spaces
         <*> (dropWhileEnd isSpace <$> validName)
         <* spaces1
         <*> (dropWhileEnd isSpace <$> some anySym)

-- | 'matchSqlField' very simple parser for line by line matching of fields in a create statement
matchSqlField :: Maybe Text -> String -> Either Text [PType]
matchSqlField mprefix ss =
  let x = match (Nothing <$ matchBlankSqlRE
             <|> Just <$> matchSqlFieldRE mprefix) ss
  in case x of
      Nothing -> Left [st|expected a valid column name and any column type [#{ss}]|]
      Just Nothing -> Right []
      Just (Just z@(a,b,c)) -> Right [fromMaybe (PColumn a b c) $ asum $ map ($ z) [chkIdentity, chkConstraint]]

chkIdentity, chkConstraint :: (String, String, String) -> Maybe PType
chkIdentity (a,b,c) = PIdentity a b c <$ match (many anySym *> stringCI "identity" <* many anySym) b
chkConstraint (a,b,c) = PConstraint (unwords [a,b,c]) <$ match (stringCI "constraint") a

data PTable = PTable { ptable :: !String, pcols :: ![PType] } deriving (Show,Eq)

data PType = PIdentity !String !String !String
           | PColumn !String !String !String
           | PConstraint !String
           | POther !String
           deriving (Show,Eq)

parseCreateTableSqlImpl :: String -> Either Text PTable
parseCreateTableSqlImpl sql =
  let pp = (,)
       <$ spaces
       <* stringCI "create"
       <* spaces1
       <* stringCI "table"
       <* spaces1
       <*> some (psym (/= '('))
       <* sym '('
       <*> some anySym
       <* sym ')'
       <* spaces
  in case match pp sql of
        Nothing -> Left [st|cannot parse this as a CREATE TABLE sql[#{sql}]|]
        Just (tab, ys) -> PTable (dropWhileEnd isSpace tab) . concat <$> mapM (matchSqlField Nothing) (lines ys)

matchBcp :: String -> String -> Maybe (Int,String)
matchBcp table = match (withMatched (stringCI table *> (read <$> some (psym isDigit)) <* stringCI ".bcp"))

matchBcps :: String -> IO [(Int, FilePath)]
matchBcps base = do
  xs' <- liftIO $ getDirectoryContents "."
  return $ sort $ mapMaybe (matchBcp base) xs'

stripQuotes :: Maybe (Char,Char) -> Text -> Text
stripQuotes mdelims ts =
  case mdelims of
    Nothing -> ts
    Just (q1,q2) ->
      case (T.head ts,T.last ts) of
        (a,b) | a == q1 && b == q2 -> T.drop 1 $ T.init ts
        _ -> ts

-- need to make this a little tighter: last bit checks for any old chars:could have dots in it
tableParser :: Maybe (Char, Char) -> RE Char (Maybe Text,Maybe Text,Text)
tableParser mdelims = fmap (\(ma,b,c) -> ( fmap T.pack ma
                                         , if null b then Nothing else Just (T.pack b)
                                         , stripQuotes mdelims (T.pack c)
                                         )) tableParser'

tableParser' :: RE Char (Maybe String,String,String)
tableParser' = (,,)
    <$> optional (some (psym isAlphaNum) <*  sym '.')
    <*> many (psym isAlphaNum)
    <*  sym '.'
    <*> some anySym

