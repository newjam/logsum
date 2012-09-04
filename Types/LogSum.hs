{-# LANGUAGE OverloadedStrings #-}

module Types.LogSum (
    module Data.Semigroup
  , Metric (..)
  , MetricMap (..)
  , PriorityMap (..)
  , LogEvent (..)
  , Log
) where

import           Prelude hiding             (sum, foldr1, foldr, foldl, log)

import           Data.Semigroup
import           Data.Pointed
import           Data.Copointed
import           Data.Foldable

import           Control.Monad

import qualified Data.Map                   as M
import qualified Data.ByteString.Char8      as B
import qualified Data.Text                  as T
import           Data.Aeson                 as A
import           Data.Sequence              (Seq)

import           Data.Time.Clock
import           Data.Function (on)
import           System.Log (Priority(..))
import           Data.Time.Format
import           System.Locale


timefmt :: FormatTime a => a -> B.ByteString
timefmt = B.pack . formatTime defaultTimeLocale "%s%Q"

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

insertKV k v = (:) ((T.pack . toString) k .= v)

instance ToJSON a => ToJSON (PriorityMap a) where
  toJSON = object . M.foldrWithKey insertKV mempty . dePriorityMap

instance ToJSON a => ToJSON (LogEvent a) where
  toJSON (LogEvent s e x) = object ["start" .= timefmt s, "end" .= timefmt e, "event" .= x]

instance ToJSON Metric where
  toJSON (Metric (c, d)) = A.object ["count" .= c, "duration" .= d]

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

-- misc instances
instance Functor LogEvent where
  fmap f (LogEvent s e x) = LogEvent s e (f x)

