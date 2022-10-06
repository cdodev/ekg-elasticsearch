{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
-- | This module lets you periodically flush metrics to elasticsearch. Example
-- usage:
--
-- > main = do
-- >     store <- newStore
-- >     forkElasticSearch defaultESOptions store
--
-- You probably want to include some of the predefined metrics defined
-- in the ekg-core package, by calling e.g. the 'registerGcStats'
-- function defined in that package.
module System.Remote.Monitoring.ElasticSearch
    (
      -- * The elasticsearch syncer
      ElasticSearch
    , elasticSearchThreadId
    , forkElasticSearch
      -- * ElasticSearch options
    , ESOptions(..)
    , defaultESOptions
    ) where

import           Control.Concurrent    (ThreadId, forkIO, threadDelay)
import           Control.Exception     (catch)
import           Control.Lens
import           Control.Monad         (forever, void)
import qualified Data.HashMap.Strict   as M
import           Data.Int              (Int64)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Lens
import           Data.Time.Clock       (getCurrentTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Format      (defaultTimeLocale, formatTime)
import           Network.HostName      (getHostName)
import           Network.HTTP.Req      (HttpException, POST (..),
                                        ReqBodyLbs (..), Scheme (Http), runReq)
import qualified Network.HTTP.Req      as Req
import qualified System.Metrics        as Metrics

import           System.Metrics.Json

--------------------------------------------------------------------------------
-- | A handle that can be used to control the elasticsearch sync thread.
-- Created by 'forkElasticSearch'.
newtype ElasticSearch = ElasticSearch { threadId :: ThreadId }

-- | The thread ID of the elasticsearch sync thread. You can stop the sync by
-- killing this thread (i.e. by throwing it an asynchronous
-- exception.)
elasticSearchThreadId :: ElasticSearch -> ThreadId
elasticSearchThreadId = threadId

--------------------------------------------------------------------------------
-- | Options to control how to connect to the elasticsearch server and how
-- often to flush metrics. The flush interval should be shorter than
-- the flush interval elasticsearch itself uses to flush data to its
-- backends.
data ESOptions = ESOptions
    { -- | Server hostname or IP address
      _host          :: !Text

      -- | Server port
    , _port          :: !Int

      -- | Error handler
    , _onError       :: !(HttpException -> IO ())

      -- | The elasticsearch index to insert into
    , _indexBase     :: !Text

      -- | Append "-YYYY.MM.DD" onto index?
    , _indexByDate   :: !Bool

      -- | What to put in the @beat.name@ field
    , _beatName      :: !Text

      -- | Data push interval, in ms.
    , _flushInterval :: !Int

      -- | Print debug output to stderr.
    , _debug         :: !Bool

      -- | Prefix to add to all metric names.
    , _prefix        :: !Text

      -- | Suffix to add to all metric names. This is particularly
      -- useful for sending per host stats by settings this value to:
      -- @takeWhile (/= \'.\') \<$\> getHostName@, using @getHostName@
      -- from the @Network.BSD@ module in the network package.
    , _suffix        :: !Text
      -- | Extra tags to add to events
    , _tags          :: ![Text]
    }

makeClassy ''ESOptions

-- | Defaults:
--
-- * @host@ = @\"127.0.0.1\"@
--
-- * @port@ = @8125@
--
-- * @onException@ = @print@
--
-- * @indexBase@ = @metricbeats@
--
-- * @indexByDate@ = @True@
--
-- * @beatName@ = @\"ekg\"@
--
-- * @flushInterval@ = @1000@
--
-- * @debug@ = @False@
defaultESOptions :: ESOptions
defaultESOptions = ESOptions
    { _host          = "127.0.0.1"
    , _port          = 9200
    , _onError       = print
    , _indexBase     = "metricbeat"
    , _indexByDate   = True
    , _beatName      = "ekg"
    , _flushInterval = 1000
    , _debug         = False
    , _prefix        = ""
    , _suffix        = ""
    , _tags          = []
    }

--------------------------------------------------------------------------------
-- | Create a thread that flushes the metrics in the store to elasticsearch.
forkElasticSearch :: ESOptions -- ^ Options
           -> Metrics.Store    -- ^ Metric store
           -> IO ElasticSearch -- ^ ElasticSearch sync handle
forkElasticSearch opts store = ElasticSearch <$> forkIO (loop store opts)

loop :: Metrics.Store   -- ^ Metric store
     -> ESOptions   -- ^ Options
     -> IO ()
loop store opts = forever $ do
    start <- time
    flushSample store opts
    end <- time
    threadDelay ((opts ^. flushInterval) * 1000 - fromIntegral (end - start))
    loop store opts

-- | Microseconds since epoch.
time :: IO Int64
time = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
  where toDouble = realToFrac :: Real a => a -> Double


--------------------------------------------------------------------------------
-- | Construct the correct URL to send metrics too from '_host' and '_port'
elasticURL :: ESOptions -> Req.Url 'Http
elasticURL eo = Req.http . T.pack $ (eo ^. host.unpacked) <> ":" ++ show (eo ^. port) <> "/_bulk"

-- | Construct the index to send to
--
-- if '_indexByDate' is @True@ this will be '_indexBase'-YYYY.MM.DD otherwise it
-- will just be '_indexBase'
mkIndex :: ESOptions -> IO CreateBulk
mkIndex eo =
  CreateBulk <$> if eo ^. indexByDate
    then appendDate (eo ^. indexBase)
    else return (eo ^. indexBase)
  where
    appendDate base = do
      day <- formatTime defaultTimeLocale "%Y.%m.%d" <$> getCurrentTime
      return $ base <> "-" <> (day ^. packed)

--------------------------------------------------------------------------------
-- | Generate a 'BeatEvent' for each metric in the 'Metrics.Store'
sampleBeatEvents :: Metrics.Store -> ESOptions -> IO [BeatEvent]
sampleBeatEvents store eo = do
  now <- getPOSIXTime
  sample <- Metrics.sampleAll store
  hostName <- T.pack <$> getHostName
  finish <- getPOSIXTime
  let took = floor $ (finish - now) * 1000
      theBeat = Beat hostName (eo ^. beatName) "0.1"
      mkBeatEvt evts k v = BeatEvent theBeat now(eo ^. tags)  took (M.singleton k v) : evts
  return $ M.foldlWithKey' mkBeatEvt [] sample

--------------------------------------------------------------------------------
-- | Create a 'BulkRequest' and send it elasticsearch
flushSample :: Metrics.Store -> ESOptions -> IO ()
flushSample store eo = do
  createBulk <- mkIndex eo
  bulkEvts <- sampleBeatEvents store eo
  let body = ReqBodyLbs . bulkRequestBody . BulkRequest $ (createBulk, ) <$> bulkEvts
  (void . runReq Req.defaultHttpConfig $ Req.req POST
    (elasticURL eo)
    body
    Req.ignoreResponse
    mempty)
    `catch` _onError eo
