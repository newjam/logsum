{-# LANGUAGE OverloadedStrings #-}

module Logsum (
    metric
  , metricMap
  , priorityMap
  , durationEvent
  , duration
  , insertEvent
  , logHistory
  , ofPriority
  , count
  , printLog
) where

import Prelude hiding (sum, foldr1, foldr, foldl, log)

import Types.LogSum

import           Data.Copointed
import           Data.Foldable

import           Control.Monad

import qualified Data.Map                   as M
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Aeson                 as A
import           Data.Yaml                  as Y
--import qualified Data.Aeson.Encode.Pretty   as PA
import qualified Data.Sequence              as Seq
import           Data.Sequence ((|>), ViewL(..))

import Data.Time.Clock
import Control.Exception.Base (evaluate)
import Data.Function (on)
import Control.Concurrent.MVar
import Control.Monad.Loops
import System.Random
import System.Log (Priority(..))
import Data.Time.Format
import System.Locale


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

time :: a -> b -> IO (LogEvent (PriorityMap (MetricMap a)))
time desc f = do
  t0 <- getCurrentTime
  void $ evaluate f
  t1 <- getCurrentTime
  let level = if diffUTCTime t1 t0 > 0.05 then WARNING else INFO
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
compress :: Semigroup m => UTCTime -> Log m -> Log m
compress now log = case Seq.viewl log of
  x :< xs -> snd r |> fst r where
    r = foldl f (x, mempty) xs
    timeSince     = diffUTCTime now . startTime
    granularity y = fromIntegral . (round :: RealFrac a => a -> Integer) $ (timeSince y) / 2.0
    shouldMerge y = duration y < granularity y
    f (buffer, output) y =
      if shouldMerge (y <> buffer)
      then (y <> buffer, output)
      else (y, output |> buffer)
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
ofPriority p = fold . M.filterWithKey (\k _ -> p k) . dePriorityMap

count :: Metric -> Int
count (Metric (c, _)) = c

foo m = do
  n <- randomRIO (50000, 100000) :: IO Int
  x <- time (if n > 90000 then "foo" else "bar") (sum [1..n])
  log <- takeMVar m
  putMVar m $ insertEvent 8.0 x log

main :: IO ()
main = do
  logMVar <- newMVar mempty
  doFor (foo logMVar) 10.0
  log <- takeMVar logMVar
  putStrLn "= = = = Bleh = = = ="
  putStrLn . B.unpack . Y.encode {- PA.encodePretty -} $ log
  putStrLn "= = = = Pretty Log = = = ="
  printLog log
  putStrLn "= = = = Warnings = = = = "
  putStrLn . LBS.unpack . A.encode . ofPriority (>=WARNING) . copoint . fold1 $ log
  putStrLn "= = = = Last Second Summary = = = ="
  now <- getCurrentTime
  putStrLn . LBS.unpack . A.encode . logHistory now 1.0 $ log
  --putStrLn . show {- . ofPriority (>INFO) . copoint . fold1 . logHistory 1.0 foldr1 (<>) -} $ log

doFor :: IO a -> NominalDiffTime -> IO ()
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
        --timefmt = B.pack . formatTime defaultTimeLocale "%s%Q"
        values = map timefmt [start, end] <> map (B.pack . show) counts
    put values

timefmt :: FormatTime a => a -> B.ByteString
timefmt = B.pack . formatTime defaultTimeLocale "%s%Q"

fold1 :: (Semigroup m, Foldable t) => t m -> m
fold1 = foldr1 (<>)
