{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
-- | This module lets you periodically flush metrics to a elastic
-- backend. Example usage:
--
-- > main = do
-- >     store <- newStore
-- >     forkElastic defaultElasticOptions store
--
-- You probably want to include some of the predefined metrics defined
-- in the ekg-core package, by calling e.g. the 'registerGcStats'
-- function defined in that package.
module System.Remote.Monitoring.Elastic
    (
      -- * The elastic syncer
      Elastic
    , elasticThreadId
    , forkElastic
    , ElasticOptions(..)
    , defaultElasticOptions
    ) where

import           Control.Concurrent                   (ThreadId, threadDelay)
import           Control.Lens
import           Control.Monad                        (void)
import           Data.Int                             (Int64)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T


import           Data.Text.Lens
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import           Network.Wreq                         as Wreq

import qualified System.Metrics                       as Metrics

import           Control.Concurrent                   (forkIO)


import System.Metrics.Json

-- | A handle that can be used to control the elastic sync thread.
-- Created by 'forkElastic'.
newtype Elastic = Elastic
    { threadId :: ThreadId
    }

-- | The thread ID of the elastic sync thread. You can stop the sync by
-- killing this thread (i.e. by throwing it an asynchronous
-- exception.)
elasticThreadId :: Elastic -> ThreadId
elasticThreadId = threadId

-- | Options to control how to connect to the elastic server and how
-- often to flush metrics. The flush interval should be shorter than
-- the flush interval elastic itself uses to flush data to its
-- backends.
data ElasticOptions = ElasticOptions
    { -- | Server hostname or IP address
      _host          :: !T.Text

      -- | Server port
    , _port          :: !Int

      -- | The elastic index to insert into
    , _indexBase  :: !T.Text

      -- | Prepend "-YYYY.MM.DD" onto index
    , _indexByDate :: !Bool

      -- | Data push interval, in ms.
    , _flushInterval :: !Int

      -- | Print debug output to stderr.
    , _debug         :: !Bool

      -- | Prefix to add to all metric names.
    , _prefix        :: !T.Text

      -- | Suffix to add to all metric names. This is particularly
      -- useful for sending per host stats by settings this value to:
      -- @takeWhile (/= \'.\') \<$\> getHostName@, using @getHostName@
      -- from the @Network.BSD@ module in the network package.
    , _suffix        :: !T.Text
      -- | Extra tags to add to events
    , _tags          :: ![T.Text]
    }

makeClassy ''ElasticOptions

-- | Defaults:
--
-- * @host@ = @\"127.0.0.1\"@
--
-- * @port@ = @8125@
--
-- * @flushInterval@ = @1000@
--
-- * @debug@ = @False@
defaultElasticOptions :: ElasticOptions
defaultElasticOptions = ElasticOptions
    { _host          = "127.0.0.1"
    , _port          = 9200
    , _indexBase     = "metricbeats"
    , _indexByDate   = True
    , _flushInterval = 1000
    , _debug         = False
    , _prefix        = ""
    , _suffix        = ""
    , _tags          = []
    }

-- | Create a thread that periodically flushes the metrics in the
-- store to elastic.
forkElastic :: ElasticOptions  -- ^ Options
           -> Metrics.Store  -- ^ Metric store
           -> IO Elastic      -- ^ Elastic sync handle
forkElastic opts store = Elastic <$> forkIO (loop store opts)

loop :: Metrics.Store   -- ^ Metric store
     -> ElasticOptions   -- ^ Options
     -> IO ()
loop store opts = do
    start <- time
    flushSample store opts
    end <- time
    threadDelay ((opts ^. flushInterval) * 1000 - fromIntegral (end - start))
    loop store opts

-- | Microseconds since epoch.
time :: IO Int64
time = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
  where toDouble = realToFrac :: Real a => a -> Double


elasticURL :: ElasticOptions -> String
elasticURL eo = "http://" ++ (eo ^. host.unpacked) ++ ":" ++ show (eo ^. port) ++ "/_bulk"

mkIndex :: ElasticOptions -> IO CreateBulk
mkIndex eo =
  CreateBulk <$> if eo ^. indexByDate
    then appendDate (eo ^. indexBase)
    else return (eo ^. indexBase)
  where
    appendDate base = do
      day <- toGregorian . utctDay <$> getCurrentTime
      return $ base <> "-" <> dateStr day
    dateStr (y, m, d) = T.intercalate "." [toT y, toT m, toT d]
    toT :: Show a => a -> T.Text
    toT = T.pack . show

flushSample :: Metrics.Store -> ElasticOptions -> IO ()
flushSample store eo = do
  createBulk <- mkIndex eo
  bulkEvts <- sampleBeatEvents store (eo ^. tags)
  void $ Wreq.post (elasticURL eo) $ BulkRequest $ (createBulk,) <$> bulkEvts
  --   forM_ (M.toList sample) $ \ (name, val) ->
  --       let fullName = dottedPrefix <> name <> dottedSuffix
  --       in  flushMetric fullName val
  -- where
  --   flushMetric name (Metrics.Counter n)      = send "|c" name (show n)
  --   flushMetric name (Metrics.Gauge n)        = send "|g" name (show n)
  --   flushMetric name (Metrics.Distribution d) = sendDistribution name d
  --   flushMetric _    (Metrics.Label _)        = return ()

  --   isDebug = debug opts
  --   dottedPrefix = if T.null (prefix opts) then "" else prefix opts <> "."
  --   dottedSuffix = if T.null (suffix opts) then "" else "." <> polatesuffix opts
  --   send ty name val = do
  --       let !msg = B8.concat [T.encodeUtf8 name, ":", B8.pack val, ty]
  --       when isDebug $ B8.h pkgs.buildEnvPutStrLn stderr $ B8.concat [intero "DEBUG: ", msg]
  --       Socket.sendAll socket msg `catch` \ (e intero:: IOException) -> do
  --           T.hPutStrLn stderr $ "ERROR: Couldn't send message: " <>
  --               T.pack (show e)
  --           return ()
