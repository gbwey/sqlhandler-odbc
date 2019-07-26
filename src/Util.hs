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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module      : Util
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

Mainly has various logging functions and timing of commands.
Allows you to log to a file or the screen or both
-}
module Util (
    module Util
  , module Util_TH
  ) where
import Data.Time
import Data.Time.Format (FormatTime)
import System.Clock
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Control.Monad.Reader
import qualified Control.Exception as E
import Control.Monad.Logger
import System.Log.FastLogger
import Data.Time.ISO8601
import qualified Data.ByteString.Char8 as B
import Text.Shakespeare.Text
import Formatting
import qualified Formatting.Time as F
import Control.Arrow
import qualified Data.Map.Strict as M
import Language.Haskell.TH.Syntax
import Data.Char (ord)
import Numeric (showHex)
import Data.List
import System.IO
import Data.These
import Data.Function (on)
import Control.Lens
import Data.Time.Lens
import System.Time.Extra (showDuration)
import Data.Tuple.Extra (dupe)
import Network.Mail.SMTP
import Network.Mail.Mime hiding (simpleMail)
import System.Environment
import qualified UnliftIO as U
import qualified UnliftIO.Exception as UE
import GHC.Stack
import Data.String
import Util_TH
import qualified GHC.Generics as G
import Data.Maybe

-- | 'ML' has the minimum set of constraints for running sql commands in sqlhandler-odbc
-- use MonadReader e to customise the environment
type ML e m = (MLog m, MonadLogger m, MonadLoggerIO m, MonadReader e m)
type MLog m = U.MonadUnliftIO m
-- | 'RL' defines the outer two layers of 'ML'
type RL e m a = ReaderT e (LoggingT m) a

-- | loads email settings from db.cfg using the key "myemail"
loadSmtp "myemail"

-- | predicate for log messages
type LogP = LogLevel -> Bool

-- | log to a file
data LogFn = LogFn {
      _lfName :: FilePath -- ^ basename of log file
    , _lfEmail :: Bool  -- ^ whether to send an email on error
    , _lfLongName :: Bool -- ^ whether to use a unique name based on datetime or the 'lfName' as is
    } deriving (Show, Eq, G.Generic)

makeLenses ''LogFn

-- | log to the screen
data LogScreen = LogOut | LogErr deriving (Show, Eq, G.Generic)
makePrisms ''LogScreen

data LogOpts = LogOpts { _loFn :: Maybe (LogFn, LogP), _loScreen :: Maybe (LogScreen, LogP) }
makeLenses ''LogOpts

-- | set logfile name
loName :: FilePath -> LogOpts -> LogOpts
loName fn o = fixFn o & loFn . _Just . _1 . lfName .~ fn

fixFn :: LogOpts -> LogOpts
fixFn (LogOpts Nothing x) = LogOpts (Just defLogFn) x
fixFn x = x

fixScreen :: LogOpts -> LogOpts
fixScreen (LogOpts x Nothing) = LogOpts x (Just defLogScreen)
fixScreen x = x

-- | sends an email when a task has completed
loEmail :: LogOpts -> LogOpts
loEmail o = fixFn o & loFn . _Just . _1 . lfEmail .~ True

-- | turns off sending email when a task has completed
loNoEmail :: LogOpts -> LogOpts
loNoEmail o = fixFn o & loFn . _Just . _1 . lfEmail .~ False

-- | writes to a unique file name based on the current time and using _loName as a base
loLongName :: LogOpts -> LogOpts
loLongName o = fixFn o & loFn . _Just . _1 . lfLongName .~ True

-- | dont use a unique filename
loNoLongName :: LogOpts -> LogOpts
loNoLongName o = fixFn o & loFn . _Just . _1 . lfLongName .~ False

-- | set the logging predicate for writing to a file
loPredFn :: LogP -> LogOpts -> LogOpts
loPredFn lp o = fixFn o & loFn . _Just . _2 .~ lp


-- | set the logging predicate for writing to the display
loPredScreen :: LogP -> LogOpts -> LogOpts
loPredScreen lp o = fixScreen o & loScreen . _Just . _2 .~ lp

-- | write to stdout
loOut :: LogOpts -> LogOpts
loOut o = fixScreen o & loScreen . _Just . _1 .~ LogOut

-- | write to stderr
loErr :: LogOpts -> LogOpts
loErr o = fixScreen o & loScreen . _Just . _1 .~ LogErr

unsetLogFn, unsetLogScreen :: LogOpts -> LogOpts
-- | turn off logging to a file
unsetLogFn = set loFn Nothing
-- | turn off logging to the screen
unsetLogScreen = set loScreen Nothing

defLogFn :: (LogFn, LogP)
defLogFn = (LogFn "abc" False False, levelD)

defLogFnBatch :: (LogFn, LogP)
defLogFnBatch = (LogFn "def" True True, levelD)

defLogScreen :: (LogScreen, LogP)
defLogScreen = (LogOut, levelD)

-- | predicates for logging
levelD, levelI, levelW, levelE, levelC :: LogP
levelD = (LevelDebug<=)
levelI = (LevelInfo<=)
levelW = (LevelWarn<=)
levelE = (LevelError<=)
levelC = (LevelOther "C"==)

defOpts, defOptsBatch, defOptsBatchW :: LogOpts
defOpts = LogOpts (Just defLogFn) (Just defLogScreen)
defOptsBatch = LogOpts (Just defLogFnBatch) (Just (LogOut, levelI))
defOptsBatchW = LogOpts (Just defLogFnBatch) (Just (LogOut, levelW))

defOut, defErr :: LogOpts
defOut = LogOpts Nothing (Just defLogScreen)
defErr = LogOpts Nothing (Just (LogErr, levelD))

-- | specify predicates on file and screen at the same time
setBoth :: (LogP, LogP) -> LogOpts -> LogOpts
setBoth (p,q) = loPredFn p . loPredScreen q

fdWith :: MLog m => e -> RL e m a -> m a
fdWith e = logWith e (setBoth (dupe levelD) defOpts)

fd :: MLog m => RL () m a -> m a
fd = fdWith ()

fiWith :: MLog m => e -> RL e m a -> m a
fiWith e = logWith e (setBoth (dupe levelI) defOpts)

fi :: MLog m => RL () m a -> m a
fi = fiWith ()

fdiWith :: MLog m => e -> RL e m a -> m a
fdiWith e = logWith e (setBoth (levelD, levelI) defOpts)

fdi :: MLog m => RL () m a -> m a
fdi = logWith () (setBoth (levelD, levelI) defOpts)

fw :: MLog m => RL () m a -> m a
fw = fwWith ()

fwWith :: MLog m => e -> RL e m a -> m a
fwWith e = logWith e (setBoth (dupe levelW) defOpts)

fe :: MLog m => RL () m a -> m a
fe = feWith ()

feWith :: MLog m => e -> RL e m a -> m a
feWith e = logWith e (setBoth (dupe levelE) defOpts)

fd' :: MLog m => RL () m a -> m a
fd' = logWith () (setBoth (dupe levelD) defOptsBatch)

fi' :: MLog m => RL () m a -> m a
fi' = logWith () (setBoth (levelD, levelI) defOptsBatch)

-- | log to stdout
fsWith :: MLog m => e -> RL e m a -> m a
fsWith e = logWith e defOut

fs :: MLog m => RL () m a -> m a
fs = fsWith ()

-- log to stderr
ferrWith :: MLog m => e -> RL e m a -> m a
ferrWith e = logWith e defErr

ferr :: MLog m => RL () m a -> m a
ferr = ferrWith ()

-- | no logging
fnullWith :: MLog m => e -> RL e m a -> m a
fnullWith e = logWith e (LogOpts Nothing Nothing)

fnull :: MLog m => RL () m a -> m a
fnull = fnullWith ()

-- | allow the user to override the log level from info
lgiOverride :: MLog m => e -> Maybe LogLevel -> RL e m a -> m a
lgiOverride e lvl = logWith e $ loPredScreen (fromMaybe LevelInfo lvl <=) defOptsBatch

-- | allow the user to override the log level from warning
lgwOverride :: MLog m => e -> Maybe LogLevel -> RL e m a -> m a
lgwOverride e lvl = logWith e $ loPredScreen (fromMaybe LevelWarn lvl <=) defOptsBatch

-- | log using the LogOpts and pass in the reader value
logWith :: MLog m => e -> LogOpts -> RL e m a -> m a
logWith e opts mra = do
  let ma = runReaderT mra e
  case opts of
    LogOpts (Just (logfn, p)) mscreen -> do
      tm <- liftIO getZonedTime
      let fn = fileNameDate tm (if _lfLongName logfn then fmtLong else fmtShort) (_lfName logfn) ".log"
      runMyFileLoggingT (p,mscreen) fn $ UE.catchAny ma $ \x -> do
        $logError [st|sqlhandler-odbc: outermost error: #{show x}|]
        when (_lfEmail logfn) $ do
          es <- liftIO loadEnvs
          liftIO $ emailMessage ("sqlhandler-odbc failure: " <> T.pack fn) [TL.pack (show x), es]
        UE.throwIO x
    LogOpts Nothing o ->
      ma & case o of
                  Nothing -> flip runLoggingT (\_ _ _ _  -> return ()) -- skip logging entirely
                  Just (LogOut, p) -> runStdoutLoggingT . filterLogger (\_ lvl -> p lvl)
                  Just (LogErr, p) -> runStderrLoggingT . filterLogger (\_ lvl -> p lvl)

-- | custom logger for writing to a file
runMyFileLoggingT :: U.MonadUnliftIO m => (LogP, Maybe (LogScreen, LogP)) -> FilePath -> LoggingT m b -> m b
runMyFileLoggingT p fn logt =
  UE.bracket (liftIO $ newFileLoggerSet defaultBufSize fn)
     (liftIO . rmLoggerSet)
     (runLoggingT logt . loggerSetOutput p)


loggerSetOutput :: (LogP, Maybe (LogScreen, LogP))
              -> LoggerSet
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
loggerSetOutput (pfile, mstdout) logt l s level msg = do
  case mstdout of
    Nothing -> return ()
    Just (x, pscreen) ->
      when (pscreen level) $ B.hPutStrLn (case x of LogOut -> stdout; LogErr -> stderr) $ dispLevel level <> B.take 2000 (fromLogStr msg) -- to avoid overflow to stdout
  when (pfile level) $ do
    utcTime <- localUTC <$> getZonedTime
    let timestampStr = formatISO8601Millis utcTime
    pushLogStr logt $ defaultLogStr l s level (toLogStr (timestampStr <> " ") <> msg)

fileNameDate :: tm -> Format (String -> String) (tm -> String -> String) -> String -> String -> FilePath
fileNameDate tm fmt pref = formatToString (string % "_" % fmt % string) pref tm

fileNameDateQualified :: FormatTime a => a -> String -> String -> String
fileNameDateQualified tm pref = formatToString (string % "_" % (F.year <> F.month <> F.dayOfMonth <> "_" % F.hour24 <> F.minute <> F.second) % string) pref tm

fmtShort, fmtLong :: FormatTime a => Format r (a -> r)
fmtShort = F.year <> F.month <> F.dayOfMonth
fmtLong = F.year <> F.month <> F.dayOfMonth <> "_" % F.hour24 <> F.minute <> F.second

loadEnvs :: IO TL.Text
loadEnvs = TL.pack . unlines . map (\(x,y) -> x <> " = " <> y) <$> getEnvironment

-- | send an email using 'myemail' which pulls the settings from db.cfg
emailMessage :: Text -> [TL.Text] -> IO ()
emailMessage subj bodys =
  sendMail (T.unpack (_smtpServer myemail))
          $ simpleMail (fromString (T.unpack (_smtpFrom myemail))) [fromString (T.unpack (_smtpTo myemail))] [] [] subj [plainPart $ TL.intercalate "\n\n" bodys]

-- | used for logging start and end time of a job
timeCommand :: ML e m => Text -> m a -> m a
timeCommand = timeCommand' (\_ _ -> return ())

timeCommand' :: ML e m => (Text -> (ZonedTime, ZonedTime) -> m ()) -> Text -> m a -> m a
timeCommand' callback txt cmd = do
  (c,a) <- do
    c <- liftIO getZonedTime
    let msg = [st|Start TimeCommand #{fmtZt c} #{txt}|]
    $logInfo msg
    a <- liftIO $ getTime Monotonic
    return (c,a)
  (ret :: Either E.SomeException a) <- UE.try $ cmd >>= \x -> return $! x
  do
    b <- liftIO $ getTime Monotonic
    d <- liftIO getZonedTime
    let xs = [st|#{difftimes a b} started=#{fmtZt c} ended=#{fmtZt d}|]
    case ret of
      Left e -> do
                  let msg = [st|FAILURE!!!! TimeCommand #{xs} #{txt} [#{show e}]|]
                  --liftIO $ T.putStrLn msg
                  $logError msg
                  UE.throwIO e
      Right x -> do
                   --liftIO $ T.putStrLn $ "OK TimeCommand " <> xs
                   $logInfo [st|OK TimeCommand #{xs} #{txt}|]
                   callback txt (c,d)
                   return x


difftimes :: TimeSpec -> TimeSpec -> Text
difftimes a b = T.pack $ showDuration (fromIntegral (sec (b - a)))

fmtZt :: ZonedTime -> String
fmtZt =  formatTime defaultTimeLocale "%T"

localUTC :: ZonedTime -> UTCTime
localUTC = roundSeconds . localTimeToUTC utc . zonedTimeToLocalTime

roundSeconds :: Timeable t => t -> t
roundSeconds = over seconds (fromIntegral @Integer . round)

zeroDate :: Timeable t => t -> t
zeroDate = over time (const (TimeOfDay 0 0 0))

logD :: MonadLogger m => Text -> m ()
logD = logOtherN (LevelOther "D")

logC :: Q Exp
logC = logOther "C"

dispLevel :: LogLevel -> B.ByteString
dispLevel LevelDebug = mempty
dispLevel LevelInfo = mempty
dispLevel LevelWarn = "WARN: "
dispLevel LevelError = "ERROR: "
dispLevel (LevelOther txt) = T.encodeUtf8 txt <> ": "

-- | MyLogger is a manual logger when you dont have access to MonadLogger
type MyLogger = LogLevel -> Text -> IO ()

getLogger :: MonadLoggerIO m => Loc -> m MyLogger
getLogger loc = do
  x <- askLoggerIO
--  return (\lvl msg -> x (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) "" lvl (toLogStr msg))
  return (\lvl msg -> x loc "" lvl (toLogStr msg))

{-
-- shows the Util location so not as good as above
getLoggerLoc :: MonadLoggerIO m => m MyLogger
getLoggerLoc = do
  x <- askLoggerIO
  return (\lvl msg -> x $(qLocation >>= liftLoc) "" lvl (toLogStr msg))
-}

lgDebug, lgInfo, lgWarn, lgError :: MonadIO m => MyLogger -> Text -> m ()
lgDebug lg = liftIO . lg LevelDebug
lgInfo lg = liftIO . lg LevelInfo
lgWarn lg = liftIO . lg LevelWarn
lgError lg = liftIO . lg LevelError

hoistEitherM :: U.MonadUnliftIO m => Text -> Either Text a -> m a
hoistEitherM txt = either (\e -> UE.throwIO $ GBException (txt <> e)) return

-- | compares 2 lists based on a comparator
divvyKeyed :: HasCallStack => (Ord x, Show a, Show b) => (Either a b -> x) -> [a] -> [b] -> [These a b]
divvyKeyed xt tp1 tp2 =
  let as = sortOn xt $ map Left tp1 <> map Right tp2
      bs = groupBy (on (==) xt) as
  in flip map bs $ \case
      [Left a] -> This a
      [Right a] -> That a
      [Left a, Right b] -> These a b
      [Right a, Left b] -> These b a
      o -> error $ "divvyKeyed: xt function returned duplicates! (are your keys unique?) " ++ show o

-- | handy function to group values based on a key
clusterBy :: Ord k => (a -> k) -> [a] -> [[a]]
clusterBy f = M.elems . M.fromListWith (++) . map (f &&& return)

clusterBy' :: forall k a b.
              Ord k =>
              (a -> k) -> (a -> b) -> [a] -> [(k, [b])]
clusterBy' f g xs =
  let ys = M.elems . M.fromListWith (++) . map (f &&& return) $ xs
  in map (f . head &&& map g) ys

newline :: Text
newline = "\n"

hexChar :: Char -> String
hexChar c =
  let (a,b) = quotRem (ord c) 16
  in showHex a (showHex b "")

dumpDecHex :: B.ByteString -> IO ()
dumpDecHex bs = do
  B.putStrLn bs
  putStrLn $ "hex=" ++ unwords (map hexChar (B.unpack bs))
  putStrLn $ "dec=" ++ unwords (map (\c -> [c,'_']) (B.unpack bs))

