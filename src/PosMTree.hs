-- positioned multitree
module PosMTree where

import qualified Data.Map as M
import Data.Maybe

-- multitree
data MTree m = MTree (M.Map m (MTree m)) deriving (Show, Eq)
-- positioned multitree
data MTreeContext m = Top | ChildOfViaSibs (MTreeContext m) m (MTree m)
  deriving (Show, Eq)
type PosMTree m = (MTree m, MTreeContext m)

empty :: PosMTree m
empty = (MTree (M.empty), Top)

-- follow child from current position
descend :: Ord m => m -> PosMTree m -> Maybe (PosMTree m)
descend child (MTree mTree, ctx) = case M.lookup child mTree of
  Nothing -> Nothing
  Just mTreeCh ->
    Just (mTreeCh, ChildOfViaSibs ctx child $ MTree $ M.delete child mTree)

-- go back up to immediate parent
ascend :: Ord m => PosMTree m -> Maybe (PosMTree m)
ascend (mTree, ctx) = case ctx of
  Top -> Nothing
  ChildOfViaSibs parent key (MTree sibs) ->
    Just (MTree $ M.insert key mTree sibs, parent)

{-
-- add key, overwriting
insert :: Ord m => m -> PosMTree m -> PosMTree m
insert key (MTree mTree, ctx) = (MTree $ M.insert key (MTree M.empty) mTree, ctx)
-}

-- add key, but only if it doesn't already exist
insIfNotKey :: Ord k => k -> a -> M.Map k a -> M.Map k a
insIfNotKey key val map =
  if M.member key map
    then map
    else M.insert key val map

-- follow child from current position, adding if needed
descAdd :: Ord m => m -> PosMTree m -> PosMTree m
descAdd child (MTree mTree, ctx) = fromJust $
  descend child (MTree $ insIfNotKey child (MTree M.empty) mTree, ctx)

-- return list of positions leading to current
getPath :: Ord m => PosMTree m -> [m]
getPath (_, Top) = []
getPath pmt@(_, ChildOfViaSibs _ key _) =
  getPath (fromJust (ascend pmt)) ++ [key]

{-
runTests :: IO ()
runTests = do
-}
