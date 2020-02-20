{-# LANGUAGE LambdaCase #-}

module Chapter4.Maps where

import qualified Data.Map as M
import Data.Tree

insert :: Ord k => k -> v -> M.Map k v -> M.Map k v
insert k v = M.alter (\_ -> Just v) k

delete :: Ord k => k -> M.Map k v -> M.Map k v
delete = M.alter $ const Nothing

adjust :: Ord k => (v -> v) -> k -> M.Map k v -> M.Map k v
adjust f =
  M.alter
    (\case
       Just x -> Just $ f x
       _ -> Nothing)

testTree :: Tree Int
testTree = Node 1 [Node 2 [Node 3 [], Node 4 []]]

postOrder :: (a -> b) -> Tree a -> [b]
postOrder f (Node v subForest) =
  let subtreesTraversed = concatMap (postOrder f) subForest
   in subtreesTraversed ++ [f v]