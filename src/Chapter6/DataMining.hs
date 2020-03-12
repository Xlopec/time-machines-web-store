{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter6.DataMining where

import Data.List
import qualified Data.Map as M

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

kMeans ::
     (Ord v, Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- initialization function
  -> Int -- number of centroids
  -> [e] -- the information
  -> Double -- threshold
  -> [v] -- final centroids
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
   in if shouldStop oldNewCentroids threshold
        then newCentroids
        else kMeans' newCentroids points threshold

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
   in foldr
        (\p m ->
           let chosenC = minimumBy (compareDistance p) centroids
            in M.adjust (p :) chosenC m)
        initialMap
        points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

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

initSimple :: Int -> [e] -> [(Double, Double)]
initSimple 0 _ = []
initSimple n v = (fromIntegral n, fromIntegral n) : initSimple (n - 1) v