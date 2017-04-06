{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Encoding of ekg metrics as metricbeats compliant JSON. This module is
-- originally from egk-json with some tweaks
module System.Metrics.Json
    ( -- * Converting metrics to JSON values
      sampleToJson
    , valueToJson
    , Beat(..)
    , BeatEvent(..)
    , beat, timestamp, beatTags, ekg, rtt
    , sampleBeatEvents
    , BulkRequest(..)
    , CreateBulk(..)

      -- ** Newtype wrappers with instances
    , Sample(..)
    , Value(..)
    ) where

import           Control.Lens                hiding ((.=))
import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as A
import qualified Data.Aeson.Types            as A
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as C8
import qualified Data.HashMap.Strict         as M
import           Data.Int                    (Int64)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX       (POSIXTime, getPOSIXTime)
import           GHC.Generics                (Generic)
import           Network.HostName            (getHostName)

import           Network.HTTP.Client         (RequestBody (..), requestBody)
import           Network.Wreq.Types
import qualified System.Metrics              as Metrics
import qualified System.Metrics.Distribution as Distribution

--------------------------------------------------------------------------------
-- * Converting metrics to JSON values


-- | Encode metrics as nested JSON objects. Each "." in the metric
-- name introduces a new level of nesting. For example, the metrics
-- @[("foo.bar", 10), ("foo.baz", "label")]@ are encoded as
--
-- > {
-- >   "foo": {
-- >     "bar": {
-- >       "type:", "c",
-- >       "val": 10
-- >     },
-- >     "baz": {
-- >       "type": "l",
-- >       "val": "label"
-- >     }
-- >   }
-- > }
--

--------------------------------------------------------------------------------
data Beat = Beat {
    hostname :: !Text
  , name     :: !Text
  , version  :: !Text
  } deriving (Generic)

instance A.ToJSON Beat

--------------------------------------------------------------------------------
newtype CreateBulk = CreateBulk { _index :: Text }

instance A.ToJSON CreateBulk where
  toJSON (CreateBulk idx) = A.object
    [ "create" .= A.object [ "_index" .= idx
                           , "_type" .= ("metricsets" :: Text)
                           ]
    ]

--------------------------------------------------------------------------------
data BeatEvent = BeatEvent {
    _beat      :: !Beat
  , _timestamp :: !POSIXTime
  , _beatTags  :: ![Text]
  , _rtt       :: !Int
  , _ekg       :: !Metrics.Sample
  }

makeLenses ''BeatEvent

instance A.ToJSON BeatEvent where
  toJSON b =
    A.object
     [ "beat" .= (b ^. beat)
     , "metricset" .= metricset
     , "@timestamp" .= (floor . (*1000) $ b ^. timestamp :: Integer)
     , "ekg" .= sampleToJson (b ^. ekg)
     , "type" .= ("metricsets" :: Text)
     ]
    where
      metricset = A.object
        [ "module" .= ("ekg" :: Text)
        , "name"   .= ("ekg" :: Text)
        , "rtt"    .= (b ^. rtt)
        ]


--------------------------------------------------------------------------------
newtype BulkRequest = BulkRequest [(CreateBulk, BeatEvent)]

instance Postable BulkRequest where
  postPayload (BulkRequest docs) req = do
    putStrLn $ C8.unpack body
    return $ req { requestBody = RequestBodyLBS body}
    where
      body = (<> "\n") . LBS.intercalate "\n" . concatMap encodeBoth $ docs
      encodeBoth (cb, be) = [A.encode cb, A.encode be]

--------------------------------------------------------------------------------
sampleBeatEvents :: Metrics.Store -> [Text] -> IO [BeatEvent]
sampleBeatEvents store extraTags = do
  now <- getPOSIXTime
  sample <- Metrics.sampleAll store
  host <- T.pack <$> getHostName
  finish <- getPOSIXTime
  let took = floor $ (finish - now) * 1000
      theBeat = Beat host "ekg" "0.1"
      mkBeatEvt evts k v = BeatEvent theBeat now extraTags took (M.singleton k v) : evts
  return $ M.foldlWithKey' mkBeatEvt [] sample

--------------------------------------------------------------------------------
sampleToJson :: Metrics.Sample -> A.Value
sampleToJson metrics =
    buildOne metrics A.emptyObject
  where
    buildOne :: M.HashMap T.Text Metrics.Value -> A.Value -> A.Value
    buildOne m o = M.foldlWithKey' build o m

    build :: A.Value -> T.Text -> Metrics.Value -> A.Value
    build m key = go m (T.splitOn "." key)

    go :: A.Value -> [T.Text] -> Metrics.Value -> A.Value
    go (A.Object m) [str] val      = A.Object $ M.insert str metric m
      where metric = valueToJson val
    go (A.Object m) (str:rest) val = case M.lookup str m of
        Nothing -> A.Object $ M.insert str (go A.emptyObject rest val) m
        Just m' -> A.Object $ M.insert str (go m' rest val) m
    go v _ _                        = typeMismatch "Object" v

typeMismatch :: String   -- ^ The expected type
             -> A.Value  -- ^ The actual value encountered
             -> a
typeMismatch expected actual =
    error $ "when expecting a " ++ expected ++ ", encountered " ++ typ ++
    " instead"
  where
    typ = case actual of
        A.Object _ -> "Object"
        A.Array _  -> "Array"
        A.String _ -> "String"
        A.Number _ -> "Number"
        A.Bool _   -> "Boolean"
        A.Null     -> "Null"

-- | Encodes a single metric as a JSON object. Example:
--
-- > {
-- >   "type": "c",
-- >   "val": 89460
-- > }
--
valueToJson :: Metrics.Value -> A.Value
valueToJson (Metrics.Counter n)      = scalarToJson n CounterType
valueToJson (Metrics.Gauge n)        = scalarToJson n GaugeType
valueToJson (Metrics.Label l)        = scalarToJson l LabelType
valueToJson (Metrics.Distribution l) = distrubtionToJson l

-- | Convert a scalar metric (i.e. counter, gauge, or label) to a JSON
-- value.
scalarToJson :: A.ToJSON a => a -> MetricType -> A.Value
scalarToJson val ty = A.object
    [metricType ty .= val]
{-# SPECIALIZE scalarToJson :: Int64 -> MetricType -> A.Value #-}
{-# SPECIALIZE scalarToJson :: T.Text -> MetricType -> A.Value #-}

data MetricType =
      CounterType
    | GaugeType
    | LabelType
    | DistributionType

metricType :: MetricType -> T.Text
metricType CounterType      = "count"
metricType GaugeType        = "gauge"
metricType LabelType        = "label"
metricType DistributionType = "dist"

-- | Convert a distribution to a JSON value.
distrubtionToJson :: Distribution.Stats -> A.Value
distrubtionToJson stats = A.object
    [ "mean" .= Distribution.mean stats
    , "variance" .= Distribution.variance stats
    , "count" .= Distribution.count stats
    , "sum" .= Distribution.sum stats
    , "min" .= Distribution.min stats
    , "max" .= Distribution.max stats
    ]

------------------------------------------------------------------------
-- ** Newtype wrappers with instances

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Sample' without creating an orphan instance.
newtype Sample = Sample Metrics.Sample
    deriving Show

-- | Uses 'sampleToJson'.
instance A.ToJSON Sample where
    toJSON (Sample s) = sampleToJson s

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Value' without creating an orphan instance.
newtype Value = Value Metrics.Value
    deriving Show

-- | Uses 'valueToJson'.
instance A.ToJSON Value where
    toJSON (Value v) = valueToJson v
