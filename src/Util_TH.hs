{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{- |
Module      : Util_TH
Description : template haskell methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com
-}
module Util_TH where
import Data.Text (Text)
import qualified Control.Exception as E
import Text.Shakespeare.Text
import Language.Haskell.TH.Syntax
import qualified UnliftIO.Exception as UE
import Data.Configurator
import Data.Configurator.Types
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

newtype GBException = GBException { gbMessage :: Text } deriving (Show,Eq)
instance E.Exception GBException

data Smtp = Smtp { _smtpServer :: !Text, _smtpFrom :: !Text, _smtpTo :: !Text } deriving (Show,Eq)

_Config :: FilePath
_Config = "db.cfg"

loadFromConfig :: IO Config
loadFromConfig = load [Required _Config]

req :: Configured a => Config -> C.Name -> IO a
req c nm = do
  ma <- C.lookup c nm
  case ma of
    Nothing -> UE.throwIO $ GBException [st|Required field [#{nm}] not found in config [#{_Config}]|]
    Just v -> return v

loadSmtp :: String -> Q [Dec]
loadSmtp nm' = do
  let k = "email"
  c <- runIO loadFromConfig
  server <- runIO $ req c (k <> ".server")
  fromx <- runIO $ req c (k <> ".from")
  tox <- runIO $ req c (k <> ".to")
  let nm = mkName nm'
  return [SigD nm (ConT ''Smtp),ValD (VarP nm) (NormalB (AppE (AppE (AppE (ConE 'Smtp) (LitE (StringL server))) (LitE (StringL fromx))) (LitE (StringL tox)))) []]

  -- this works but we get a binding warning cos not in one splice
  -- (SigD nm (ConT ''Smtp) :) <$> [d| $(varP nm) = Smtp $(stringE server) $(stringE fromx) $(stringE tox) |]

{-
>TH.runQ $ let nm=TH.mkName "xyz"; server="ss";fromx="from";tox="to" in [d| $(TH.varP nm) = Smtp $(TH.stringE server) $(TH.stringE fromx) $(TH.stringE tox) |]

<interactive>:151:76: warning: [-Wunused-pattern-binds]
    This pattern-binding binds no variables:
      $(TH.varP nm)
        = Smtp $(TH.stringE server) $(TH.stringE fromx) $(TH.stringE tox)
[ValD (VarP xyz) (NormalB (AppE (AppE (AppE (ConE Util_TH.Smtp) (LitE (StringL "ss"))) (LitE (StringL "from"))) (LitE (StringL "to")))) []]
it :: [TH.Dec]
-}