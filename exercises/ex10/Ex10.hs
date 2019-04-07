{-# LANGUAGE InstanceSigs #-}

{- CSC324 Winter 2018: Exercise 10

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}
module Ex10 (Tree(..), treeSum, collectTreeMaybe, collectTreeM) where

import qualified Data.Foldable as Foldable
import qualified Control.Monad as Monad  -- You may or may not wish to use this.

-- | Imports used for testing purposes only.
import Control.Monad (liftM2)
import Test.QuickCheck (Property, quickCheck, (==>), oneof, sized, Arbitrary(..), Gen, resize, listOf)


-------------------------------------------------------------------------------
-- |
-- = Task 1: The Tree datatype

-- | A recursive datatype for *non-empty* trees with arbitrary branching.
data Tree a = Tree a [Tree a]
            deriving (Show, Eq)  -- Useful for debugging and testing


-- | Returns the sum of the numbers in the tree.
treeSum :: Tree Int -> Int
treeSum (Tree root subtrees) =
    if subtrees == []
    then root
    else root + (treeSum (head subtrees)) + (treeSum (Tree 0 (tail subtrees)))

-- | Make Tree an instance of Functor.
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Tree root subtrees) =
        if (length subtrees) == 0
        then Tree (f root) []
        else Tree (f root) ((fmap f (head subtrees)) : (reverse (foldl (\lst x -> (fmap f x) : lst) [] (tail subtrees))))

---------------------------------------------------------------------------------
-- | Task 2: A new typeclass

-- | Foldable typeclass. Only foldr is necessary to implement.
-- For the complete documentation of this typeclass, see
-- https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Foldable.html.
instance Foldable Tree where
    -- HINT: think of the "b" value in this type signature as the *accumulator*.
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f x (Tree root subtrees) =
        if (length subtrees) == 0
        then f root x
        else f root (foldr (\tree x -> foldr f x tree) x subtrees)


---------------------------------------------------------------------------------
-- | Task 3: Maybes and monads

-- You can use built-in functions imported from Control.Monad here,
-- and copy over work from Lab 10 (where you did something similar for lists).
collectTreeMaybe :: Tree (Maybe a) -> Maybe (Tree a)
collectTreeMaybe tree = collectTreeM tree

collectTreeM :: Monad m => Tree (m a) -> m (Tree a)
collectTreeM (Tree root subtrees) = helper (Tree root subtrees)
    --if (length subtrees) == 0
    --then root >>= (\rootVal -> return (Tree rootVal []))
    --else foldr helper tree (Tree root subtrees)
    --root >>= (\rootVal -> return (foldr helper [] (Tree root subtrees)))

helper :: Monad m => Tree (m a) -> m (Tree a)
helper (Tree root subtrees) = 
    (root >>= (\rootVal -> Monad.sequence (fmap helper subtrees) >>= (\sub -> return (Tree rootVal sub))))

--helper2 f (x:xs) =
--    if (length xs) == 0
--    then [f x]
--    else (f x) : (helper f xs)


--helper :: Monad m => Tree (m a) -> m (Tree a) -> m (Tree a)
--helper (Tree root subtrees) tree = --root >>= (\rootVal -> rootVal)
    --if (length subtrees) == 0
    --then root >>= (\rootVal -> return (Tree rootVal []))
    --else foldr helper tree (Tree root subtrees)
    --else root >>= (\rootVal -> return (Tree rootVal (foldr helper lst subtrees)))

--helper rootVal =
    --if rootVal == Nothing
    --then Nothing
    --else return rootVal

---------------------------------------------------------------------------------
-- Sample tests.

-- | For testing, we first need to define how to generate "random" Trees.
-- You can safely ignore this code for this exercise.
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized tree'
        where
            tree' :: Arbitrary a => Int -> Gen (Tree a)
            tree' 0 = liftM2 Tree arbitrary (return [])
            tree' 1 = liftM2 Tree arbitrary (return [])
            tree' n = oneof [
                liftM2 Tree arbitrary (return [])
              , liftM2 Tree arbitrary (resize 3 (listOf (resize (n-1) arbitrary)))
              ]

sampleTree :: Tree Int
sampleTree =
    Tree 5
        [ Tree 3 []
        , Tree 6 [Tree 4 [], Tree 10 []]
        , Tree 7 [Tree 15 [Tree 99 []]]
        , Tree (-1) []
        ]

-- | Testing one of the functor laws: "fmapping" the identity function
-- doesn't change the tree.
test_treeFunctor_id :: Tree Int -> Bool
test_treeFunctor_id t = fmap id t == t

-- | Uses the "sum" function from Foldable, which has a default implementation
-- in terms of Foldable.foldr that you should implement for Tree.
test_sample_FoldableSum :: Bool
test_sample_FoldableSum =
    Foldable.sum sampleTree == (5+3+6+4+10+7+15+99-1)

-- | Test relationship between Foldable.sum and Foldable.length.
test_treeSumAndLength :: Tree Int -> Bool
test_treeSumAndLength t =
    Foldable.length t == Foldable.sum (fmap (\_ -> 1) t)

-- | Test that collectTreeMaybe works when given a tree with only Justs.
test_collectMaybe_onlyJusts :: Tree Int -> Bool
test_collectMaybe_onlyJusts t =
    collectTreeMaybe (fmap Just t) == Just t

-- | Same as test_collectMaybe_onlyJusts, but using collectTreeM.
-- collectTreeM should have the same behaviour.
test_collectM_onlyJusts :: Tree Int -> Bool
test_collectM_onlyJusts t =
    collectTreeM (fmap Just t) == Just t


main :: IO ()
main = do
    quickCheck test_treeFunctor_id
    quickCheck test_sample_FoldableSum
    quickCheck test_treeSumAndLength
    quickCheck test_collectMaybe_onlyJusts
    quickCheck test_collectM_onlyJusts
