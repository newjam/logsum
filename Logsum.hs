{-# LANGUAGE OverloadedStrings #-}

module Logsum where

import Prelude hiding (sum, foldr1, foldr, foldl)

import           Data.Semigroup
import           Data.Pointed
import           Data.Copointed
import           Data.Foldable
import           Data.Traversable

import qualified Data.Map                   as M
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T
import           Data.Aeson                 as A
import qualified Data.Sequence              as Seq
import           Data.Sequence (Seq, (<|), (|>), (><), ViewR (..), ViewL (..))

import Data.Time.Clock
import Control.Exception.Base (evaluate)
import Data.Function (on)
import Data.String
import Control.Concurrent.MVar
import Control.Monad.Loops
import System.Random
import System.Log (Priority(..))
import Data.Time.Format
import System.Locale


-- data types
data LogEvent a = LogEvent {startTime::UTCTime, endTime::UTCTime, deLogEvent::a}

newtype Metric = Metric (Int, NominalDiffTime)
  deriving (Show, Eq)

newtype MetricMap a = MetricMap {deMetricMap :: (M.Map a Metric)}
  deriving (Eq)

newtype PriorityMap a = PriorityMap {dePriorityMap :: (M.Map Priority a)}
  deriving (Eq, Show)

type Log a = Seq (LogEvent a)

-- Semigroup instances.
-- Semigroup is used because it is more general than Monoid; some types don't
-- have an appropriate mempty
instance Semigroup Metric where
  Metric (x1, t1) <> Metric (x2, t2) = Metric (x1 + x2, t1 + t2)

instance Semigroup a => Semigroup (PriorityMap a) where
  x <> y = PriorityMap $ (M.unionWith (<>) `on` dePriorityMap) x y

instance Ord a => Semigroup (MetricMap a) where
  x <> y = MetricMap $ (M.unionWith (<>) `on` deMetricMap) x y

instance Semigroup a => Semigroup (LogEvent a) where
  LogEvent begin end event <> LogEvent begin' end' event' =
      LogEvent (min begin begin') (max end end') (event <> event')

-- Monoid instances, for when we want an mempty
instance Monoid Metric where
  mappend = (<>)
  mempty  = Metric (0, 0.0)

instance (Monoid a, Semigroup a) => Monoid (PriorityMap a) where
  mappend = (<>)
  mempty = PriorityMap . M.fromList $ [(p, mempty) | p <- priorities]

priorities :: [Priority]
priorities = [DEBUG, WARNING, ERROR {- , INFO, NOTICE, CRITICAL, ALERT, EMERGENCY -} ]

instance Ord a => Monoid (MetricMap a) where
  mappend = (<>)
  mempty = MetricMap M.empty

-- pointed and copointed instances, because why not.
instance Pointed PriorityMap where
  point = PriorityMap . M.singleton DEBUG -- why not...

instance Pointed MetricMap where
  point x = MetricMap $ M.singleton x (Metric (1, 0.0))

instance Copointed LogEvent where
  copoint (LogEvent _ _ x) = x

-- Aeson instances, for rendering results.

instance Show a => Show (LogEvent a) where
  show (LogEvent t0 t1 x) = "LogEvent " ++ (show . utctDayTime $ t0) ++ " " ++ (show . utctDayTime $ t1) ++ " " ++ (show x)

instance ToString a => ToJSON (MetricMap a) where
  toJSON = object . M.foldrWithKey insertKV mempty . deMetricMap

insertKV k v xs = ( (T.pack . toString) k .= v) : xs

instance ToJSON a => ToJSON (PriorityMap a) where
  toJSON = object . M.foldrWithKey insertKV mempty . dePriorityMap

instance ToJSON a => ToJSON (LogEvent a) where
  toJSON (LogEvent s e x) = object ["start" .= timefmt s, "end" .= timefmt e, "event" .= x]

instance ToJSON Metric where
  toJSON (Metric (count, duration)) = A.object ["count" .= count, "duration" .= duration]

instance ToJSON NominalDiffTime where
  toJSON = String . T.pack . show

instance ToJSON a => ToJSON (Seq a) where
  toJSON = toJSON . toList

class ToString a where
  toString :: a -> String
instance ToString B.ByteString where
  toString = B.unpack
instance ToString Priority where
  toString = show


{-
instance Show a => Show (MetricMap a) where
  show = B.unpack . A.encode . M.mapKeys show . deMetricMap
-}

-- misc instances
instance Functor LogEvent where
  fmap f (LogEvent s e x) = LogEvent s e (f x)

-- smart constructors for the data types
metric :: UTCTime -> UTCTime -> Metric
metric start end = Metric (1, diffUTCTime end start)

metricMap :: a -> Metric -> MetricMap a
metricMap x = MetricMap . M.singleton x

priorityMap :: Priority -> a -> PriorityMap a
priorityMap p = PriorityMap . M.singleton p

durationEvent :: Priority -> a -> UTCTime -> UTCTime ->  LogEvent (PriorityMap (MetricMap a))
durationEvent priority thing start end =
    LogEvent start end
  . priorityMap priority
  . metricMap thing $
    metric start end

-- Time related utility functions.
duration :: LogEvent a -> NominalDiffTime
duration x = diffUTCTime (endTime x) (startTime x)

averageTime :: UTCTime -> UTCTime -> UTCTime
averageTime x y = addUTCTime ((diffUTCTime y x) / 2) x

time desc f = do
  t0 <- getCurrentTime
  evaluate f
  t1 <- getCurrentTime
  let level = if diffUTCTime t1 t0 > 1.0 then WARNING else INFO
  return $ durationEvent level desc t0 t1

-- insert an event into a log, dropping events that are older than a particular time
insertEvent :: Semigroup a => NominalDiffTime -> LogEvent a -> Log a -> Log a
insertEvent s x log = dropOld . compress now . sortByTime $ log |> x where
  now         = endTime x
  dropOld    = Seq.dropWhileL (\y -> averageTime (startTime y) (endTime y) < t)
  t          = addUTCTime (-s) now
  sortByTime = Seq.unstableSortBy (compare `on` startTime)

-- compress log events that happened a long time ago so we can summarize
-- log events and save memory.
--compress2 :: Semigroup m => UTCTime -> Log m -> Log m
compress now log = case Seq.viewl log of
  x :< xs -> snd r |> fst r where
    r = foldl f (x, mempty) xs
    timeSince     = diffUTCTime now . startTime
    granularity x = (timeSince x) / 5.0
    shouldMerge x = duration x < granularity x
    f (buffer, output) x =
      if shouldMerge (x <> buffer)
      then (x <> buffer, output)
      else (x, output |> buffer)
  EmptyL  -> log

-- get a summary of the events that happened in the past x seconds.
logHistory :: (Monoid m, Semigroup m) =>
  UTCTime -> NominalDiffTime -> Log m -> LogEvent m
logHistory now t = fold1 . Seq.takeWhileR inRange where
  inRange y = diffUTCTime now (startTime y) <= t

-- A list of all of the names of a log event
keysOf :: Ord k => LogEvent (PriorityMap (MetricMap k)) -> [k]
keysOf = M.keys . deMetricMap . fold .  M.elems . dePriorityMap . deLogEvent

-- select metrics from a PriorityMap that satisfy a predicate that operates on
-- the log event priority.
ofPriority :: Monoid m => (Priority -> Bool) -> PriorityMap m -> m
ofPriority pred = fold . M.filterWithKey (\k _ -> pred k) . dePriorityMap

count :: Metric -> Int
count (Metric (c, _)) = c

foo m = do
  n <- randomRIO (50000, 100000) :: IO Int
  x <- time (if n > 90000 then "foo" else "bar") (sum [1..n])
  log <- takeMVar m
  putMVar m $ insertEvent 2.0 x log

main = do
  logMVar <- newMVar mempty
  doFor (foo logMVar) 4.0
  log <- takeMVar logMVar
  putStrLn "= = = = Bleh = = = ="
  putStrLn . LBS.unpack . A.encode $ log
  putStrLn "= = = = Pretty Log = = = ="
  printLog log
  putStrLn "= = = = Warnings = = = = "
  putStrLn . LBS.unpack . A.encode . ofPriority (>=WARNING) . copoint . fold1 $ log
  putStrLn "= = = = Last Second Summary = = = ="
  now <- getCurrentTime
  putStrLn . LBS.unpack . A.encode . logHistory now 1.0 $ log
  --putStrLn . show {- . ofPriority (>INFO) . copoint . fold1 . logHistory 1.0 foldr1 (<>) -} $ log

doFor a t = do
  t0 <- getCurrentTime
  untilM_ a (fmap (> addUTCTime t t0) getCurrentTime)

-- A function that prints out spreadsheet style representation of the data,
-- useful for plotting the data in gnuplot.
printLog :: Log (PriorityMap (MetricMap B.ByteString)) -> IO ()
printLog log = do
  let names = keysOf . fold1 $ log
      headers = ["start", "end"] <> names
      put = putStrLn . B.unpack . B.intercalate " "
  put headers
  for_ log $ \(LogEvent start end (PriorityMap mm)) -> do
    let counts = map getCount names
        getCount k = M.findWithDefault 0 k . M.map count . deMetricMap . fold $ mm
        timefmt = B.pack . formatTime defaultTimeLocale "%s%Q"
        values = map timefmt [start, end] <> map (B.pack . show) counts
    put values

timefmt :: FormatTime a => a -> B.ByteString
timefmt = B.pack . formatTime defaultTimeLocale "%s%Q"

fold1 :: (Semigroup m, Foldable t) => t m -> m
fold1 = foldr1 (<>)
