module Text.Search.NGram (
    Index,
    intersection,
    difference,
    IndexValue,
    search,
    ngram,
    hash,
    value,

    IndexAction,
    insertAction,
    removeAction,
    insert,
    remove,
    update,
    apply,

    matchPrecise
    ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.Ord (comparing)
import Data.List hiding (insert)
import Data.Maybe (mapMaybe)

import qualified Data.Hash as H

-- | Back indexing
data Index a = Index {
    indexMap :: IM.IntMap (S.Set a) }
        deriving (Eq, Ord, Read, Show)

instance Ord a => Monoid (Index a) where
    mempty = Index mempty
    mappend (Index l) (Index r) = Index $ IM.unionWith mappend l r

-- | Intersection of indices
intersection :: Ord a => Index a -> Index a -> Index a
intersection (Index l) (Index r) =
    Index $ IM.intersectionWith S.intersection l r

-- | Difference of indices
difference :: Ord a => Index a -> Index a -> Index a
difference (Index l) (Index r) =
    Index $ IM.differenceWith difference' l r
    where
        difference' x y = if S.null d then Nothing else Just d where
            d = S.difference x y

-- | Index keys
type IndexValue = [Int]

-- | Search for values
-- Returns results sorted by number of occurrences
search :: Ord a => Index a -> (b -> IndexValue) -> b -> [(a, Int)]
search i h v =
    reverse
    $ sortBy (comparing snd)
    $ M.toList
    $ M.unionsWith (+)
    $ map count
    $ mapMaybe (`IM.lookup` (indexMap i)) (h v)
    where
        count = M.fromAscList . (`zip` repeat 1) . S.toAscList

-- | NGram index method
ngram :: Int -> String -> IndexValue
ngram i =
    concatMap hash
    . takeWhile ((== i) . length)
    . map (take i)
    . tails

-- | Hash index method
-- TODO: Use Data.Hash
hash :: String -> IndexValue
hash = reference . fromIntegral . H.asWord64 . H.hash

-- | Simple back reference for index
reference :: Int -> IndexValue
reference = return

-- | Index one value
value :: Ord a => (b -> IndexValue) -> b -> a -> Index a
value h k v = mconcat $ map value' (h k) where
    value' k' = Index $ IM.singleton k' (S.singleton v)

-- | Action on index
-- Represented as difference and append
-- f x = x + a - b
-- assumed, that a `intersecion` b == mempty,
-- in this case x + a - b = x - b + a
data IndexAction a = IndexAction (Index a) (Index a)
    deriving (Eq, Ord, Read, Show)

-- | Insert index
insertAction :: Ord a => Index a -> IndexAction a
insertAction ix = IndexAction ix mempty

-- | Remove index
removeAction :: Ord a => Index a -> IndexAction a
removeAction ix = IndexAction mempty ix

-- | (f `mappend` g) `apply` x = f `apply` (g `apply` x)
instance Ord a => Monoid (IndexAction a) where
    mempty = IndexAction mempty mempty
    mappend (IndexAction il rl) (IndexAction ir rr) = IndexAction
        ((ir `difference` rl) `mappend` il)
        ((rr `difference` il) `mappend` rl)

-- | Insert action
insert :: Ord a => (b -> IndexValue) -> b -> a -> IndexAction a
insert h k v = IndexAction (value h k v) mempty

-- | Remove action
remove :: Ord a => (b -> IndexValue) -> b -> a -> IndexAction a
remove h k v = IndexAction mempty (value h k v)

-- | Update action
update :: Ord a => (b -> IndexValue) -> b -> b -> a -> IndexAction a
update h kr ki v = IndexAction ia (ra `difference` ia) where
    ia = value h ki v
    ra = value h kr v

-- | Apply action
apply :: Ord a => IndexAction a -> Index a -> Index a
apply (IndexAction i r) v = (v `difference` r) `mappend` i

-- | Check whether result fields matches query
matchPrecise :: [String] -> String -> Bool
matchPrecise fields query = null $ deleteFirstsBy (flip isInfixOf) (words query) fields
