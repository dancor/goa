-- positioned ordered multitree
module PomTree where

import qualified Data.Map as M
import Data.Maybe

data OmTree m = OmTree [(m, OmTree m)] deriving (Show, Eq)
type PomTree m = (OmTree m, OmTreeContext m)

data OmTreeContext m =
  Top | ChildOfViaSibs (OmTreeContext m) m [(m, OmTree m)] [(m, OmTree m)]
  deriving (Show, Eq)

empty :: PomTree m
empty = (OmTree [], Top)

-- follow child from current position
descend :: Ord m => m -> PomTree m -> Maybe (PomTree m)
descend child (OmTree omTree, ctx) = case lookupAndPrePost child omTree of
  Nothing -> Nothing
  Just (omTreeCh, (pre, post)) ->
    Just (omTreeCh, ChildOfViaSibs ctx child pre post)

lookupAndPrePost :: (Eq a) => a -> [(a, b)] -> Maybe (b, ([(a, b)], [(a, b)]))
lookupAndPrePost = lookupAndPrePostAccum [] where
  lookupAndPrePostAccum accum k [] = Nothing
  lookupAndPrePostAccum accum k (x:xs) = if k == fst x
    then Just (snd x, (accum, xs))
    else lookupAndPrePostAccum (accum ++ [x]) k xs

-- go back up to immediate parent
ascend :: Ord m => PomTree m -> Maybe (PomTree m)
ascend (omTree, ctx) = case ctx of
  Top -> Nothing
  ChildOfViaSibs parent key pre post ->
    Just (OmTree $ pre ++ [(key, omTree)] ++ post, parent)

{-
-- add key, overwriting
insert :: Ord m => m -> PomTree m -> PomTree m
insert key (OmTree omTree, ctx) =
  (OmTree $ M.insert key (OmTree M.empty) omTree, ctx)
-}

-- add key, but only if it doesn't already exist
appendIfNotKey :: (Eq k) => k -> a -> [(k, a)] -> [(k, a)]
appendIfNotKey key val map = case lookup key map of
  Just _ ->  map
  Nothing -> map ++ [(key, val)]

-- follow child from current position, adding if needed
descAdd :: Ord m => m -> PomTree m -> PomTree m
descAdd child (OmTree omTree, ctx) = fromJust $
  descend child (OmTree $ appendIfNotKey child (OmTree []) omTree, ctx)

-- return list of positions leading to current
getPath :: Ord m => PomTree m -> [m]
getPath (_, Top) = []
getPath pmt@(_, ChildOfViaSibs _ key _ _) =
  getPath (fromJust (ascend pmt)) ++ [key]

{-
runTests :: IO ()
runTests = do
-}
