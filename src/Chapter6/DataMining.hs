{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.DataMining where

import Data.List
import qualified Data.Map as M
import Lens.Micro.Platform

class Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) ** 2 + (d - b) ** 2
  centroid lst =
    let (sx, sy) = foldl (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) lst
        n = fromIntegral $ length lst
     in (sx / n, sy / n)

class Vector v =>
      Vectorizable e v
  where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

data KMeansState e v =
  KMeansState
    { _centroids :: [v]
    , _points :: [e]
    , _err :: Double
    , _threshold :: Double
    , _steps :: Int
    }

makeLenses ''KMeansState

kMeans :: (Ord v, Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i n pts t = view centroids $ kMeans' (initializeState i n pts t)

kMeans' :: (Ord v, Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignmentPhase state
      state1 = state & centroids . traversed %~ (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~ sum (zipWith distance (state ^. centroids) (state1 ^. centroids))
      state3 = state2 & steps +~ 1
   in if state3 ^. err < state3 ^. threshold
        then state3
        else kMeans' state3

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignmentPhase state =
  let initialMap = M.fromList $ zip (state ^. centroids) (repeat [])
   in foldr
        (\p m ->
           let chosenC = minimumBy (compareDistance p) $ state ^. centroids
            in M.adjust (p :) chosenC m)
        initialMap $ state ^. points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0 / 0.0) t 0

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

generate :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
generate centroids points =
  let initialMap = M.fromList $ zip centroids []
   in foldr
        (\p m ->
           let chosenC = flip minimumBy centroids $ compareDistance p
            in M.adjust (p :) chosenC m)
        initialMap
        points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: Vector v => [(v, v)] -> Double -> Bool
shouldStop centroid threshold = foldr (\(x, y) s -> s + distance x y) 0.0 centroid < threshold

testInfo :: [(Double, Double)]
testInfo = [(1, 1), (1, 2), (4, 4), (4, 5)]