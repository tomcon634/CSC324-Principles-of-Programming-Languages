{- CSC324 Fall 2018: Exercise 9

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}
module Ex9 (
    Term(..),
    LVar(..), Substitution,
    walk, walkDeep, occurs, extend, unify) where

-- This is one of Haskell's built-in analogues of dictionaries.
import qualified Data.Map.Strict as Map

-- | Imports used for testing purposes only.
import Control.Monad (liftM, liftM2)
import Test.QuickCheck (
    Property, quickCheck, oneof, sized, Arbitrary(..)
    )


------------------------------------------------------------------------------
-- Data Definitions (don't change these)
------------------------------------------------------------------------------
-- This is the data Term that we will reason about. All Term constructors
-- begin with the letter "T".
data Term = TNull            -- null term
          | TBool Bool       -- boolean data
          | TInt Int         -- integer data
          | TSymbol String   -- symbolic data
          | TPair Term Term  -- pairs
          | TVar LVar        -- logic variable
          deriving (Show, Eq, Ord)

-- We use an (LVar Int) instead of a simple integer to represent
-- logic variables, so we know which integers represent logic variables.
data LVar = LVar Int
          deriving (Show, Eq, Ord)

-- Substitutions store mappings of logic variables (LVar Int) to terms.
type Substitution = Map.Map LVar Term

------------------------------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------------------------
-- | Recursively look up a term in a substitution until the result is
-- either not a logic variable, or an unbound logic variable (one that does
-- not exist in the substitution).
walk :: Term -> Substitution -> Term
walk (TVar lvar) sub =
    case Map.lookup lvar sub of
        Nothing -> TVar lvar
        Just term -> walk term sub
walk term substitution = term


-- Sample tests: uncomment each individually.
prop_walkExamples :: Bool
prop_walkExamples = and [
       walk true Map.empty == true
     , walk (TVar v0) (Map.fromList [(v0, true), (v1, TVar v0)]) == true
     , walk (TVar v1) (Map.fromList [(v0, true), (v1, TVar v0)]) == true
     , walk (TVar v2) (Map.fromList [(v0, true), (v1, TVar v0)]) == TVar v2
     -- Note that the inner logic variables are not looked up here.
     , walk (TPair (TVar v0) (TVar v1)) (Map.fromList [(v0, true), (v1, TVar v0)]) == TPair (TVar v0) (TVar v1)
    ]
    where
        true = TBool True
        false = TBool False
        v0 = LVar 0
        v1 = LVar 1
        v2 = LVar 2


-- | Similar to `walk`, but also replaces bound logic variables in the
-- recursive TPair nested structure.
-- The returned Term should have no bound logic variables.
walkDeep :: Term -> Substitution -> Term
walkDeep (TPair t0 t1) sub = TPair (walkDeep t0 sub) (walkDeep t1 sub)
walkDeep term substitution = walk term substitution


prop_walkDeepExamples :: Bool
prop_walkDeepExamples = and [
       walkDeep true Map.empty == true
     , walkDeep (TVar v0) (Map.fromList [(v0, true), (v1, TVar v0)]) == true
     , walkDeep (TVar v1) (Map.fromList [(v0, true), (v1, TVar v0)]) == true
     , walkDeep (TVar v2) (Map.fromList [(v0, true), (v1, TVar v0)]) == TVar v2
     -- Now, the nested logic variables are looked up.
     -- walk
     , walkDeep (TPair (TVar v0) (TVar v1)) (Map.fromList [(v0, true), (v1, TVar v0)]) == TPair true true
    ]
    where
        true = TBool True
        false = TBool False
        v0 = LVar 0
        v1 = LVar 1
        v2 = LVar 2


-- | Return whether the given logic variable occurs in the given term.
occurs :: LVar -> Term -> Bool
occurs lvar (TPair t0 t1) = (occurs lvar t0) || (occurs lvar t1)
occurs lvar (TVar var) = lvar == var
occurs lvar term = False


prop_occursExamples :: Bool
prop_occursExamples = and [
       not $ occurs v0 true
     , not $ occurs v0 (TVar v1)
     , occurs v1 (TVar v1)
     , occurs v1 (TPair (TVar v0) (TVar v1))
     , occurs v1 (TPair (TVar v2) (TPair true (TVar v1)))
     , occurs v0 (TPair (TVar v0) (TPair true (TVar v1)))
     , not $ occurs v2 (TPair (TVar v0) (TPair true (TVar v1)))
    ]
    where
        true = TBool True
        false = TBool False
        v0 = LVar 0
        v1 = LVar 1
        v2 = LVar 2


-- | Extend the given substitution with another (LVar -> Term) mapping,
-- but only if the logic variable does not occur in (walkDeep term sub).
extend :: LVar -> Term -> Substitution -> Maybe Substitution
extend lvar term sub =
    if walkDeep (TVar lvar) sub == TVar lvar
    then Just (Map.insert lvar term sub)
    else Nothing 

------------------------------------------------------------------------------
-- Task 2: Unify!
------------------------------------------------------------------------------
-- | Unify two terms with the given substitutions.
-- We have started this part for you. Note that to pattern-match on
-- u' and v', you'll either need to use a good helper function,
-- or use case expressions: http://learnyouahaskell.com/syntax-in-functions#case-expressions.
unify :: Term -> Term -> Substitution -> Maybe Substitution
unify (TVar lvar) v sub = extend lvar v sub
unify u (TVar lvar) sub = extend lvar u sub
unify (TPair t0 t1) (TPair t2 t3) sub =
    if (unify t0 t2 sub) == Nothing || (unify t1 t3 sub) == Nothing
    then Nothing
    else unify t1 t3 (fromJust (unify t0 t2 sub))
    --if (unify t0 t1 sub) /= Nothing
    --then unify t2 t3 (fromJust (unify t0 t1 sub))
    --else unify t2 t3 sub
unify u v sub =
    let u' = walk u sub
        v' = walk v sub
    in
        if u' == v'
        then Just sub
        else Nothing

fromJust (Just a) = a


------------------------------------------------------------------------------
-- Unify tests
------------------------------------------------------------------------------
-- These tests check some small cases for unify.
prop_unify1 :: Bool
prop_unify1 =
    unify (TVar v0) (TBool True) Map.empty ==
    Just (Map.fromList [(v0, TBool True)])
    where
        v0 = LVar 0

prop_unify2 :: Bool
prop_unify2 =
    unify (TBool True) (TBool True) Map.empty ==
    Just Map.empty

prop_unify3 :: Bool
prop_unify3 =
    unify (TBool True) (TBool False) Map.empty ==
    Nothing -- This unification fails

prop_unify4 :: Bool
prop_unify4 =
    unify (TPair (TVar v0) (TVar v1)) (TPair (TBool True) (TBool False)) Map.empty ==
    Just (Map.fromList [(v0, TBool True), (v1, TBool False)])
    where
        v0 = LVar 0
        v1 = LVar 1

prop_unify5 :: Bool
prop_unify5 =
    unify (TPair (TVar v0) (TVar v0)) (TPair (TBool True) (TBool False)) Map.empty ==
    Nothing -- This unification fails
    where
        v0 = LVar 0

------------------------------------------------------------------------------
-- Some property-based tests of unify.

-- Commutativity:
-- If (unify t1 t2 Map.empty) succeeds, so should (unify t2 t1 Map.empty).
-- The actual substitutions may differ.
prop_unify_commutativity :: Term -> Term -> Bool
prop_unify_commutativity t1 t2 =
    case (unify t1 t2 Map.empty, unify t2 t1 Map.empty) of
        (Nothing, Nothing) -> True
        (Just s1, Just s2) -> True
        (_, _)             -> False

-- Reflexivity:
-- (unify t t Map.empty) should aways succeed, and with an empty substitutions.
prop_unify_reflexivity :: Term -> Bool
prop_unify_reflexivity t =
    unify t t Map.empty == Just Map.empty

-- Idempotent:
-- If (unify t1 t2 Map.empty) succeeds with substitutions `sub`,
-- then (unify t1 t2 sub) should also succeed and produce the same substitutions.
prop_unify_idempotence :: Term -> Term -> Bool
prop_unify_idempotence t1 t2 =
    case unify t1 t2 Map.empty of
        Nothing -> True
        Just sub -> unify t1 t2 sub == Just sub


------------------------------------------------------------------------------
-- This section enables property-based tests on Terms, and can safely be ignored.
arbitraryAtom = oneof [
      return TNull
    , return (TBool True)
    , return (TBool False)
    , liftM TInt arbitrary
    , liftM TSymbol arbitrary
    ]

arbitraryPair n = liftM2 TPair subterm subterm
    where subterm = arbitraryTermN (n `div` 2)

arbitraryTermN 0 = arbitraryAtom
arbitraryTermN n = oneof [arbitraryAtom, arbitraryPair n, arbitraryPair n, arbitraryPair n]

arbitraryTerm = sized arbitraryTermN

instance Arbitrary Term where
    arbitrary = arbitraryTerm
------------------------------------------------------------------------------


-- This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    -- helper functions
    quickCheck prop_walkExamples
    quickCheck prop_walkDeepExamples
    quickCheck prop_occursExamples
    -- unification simple tests
    quickCheck prop_unify1
    quickCheck prop_unify2
    quickCheck prop_unify3
    quickCheck prop_unify4
    quickCheck prop_unify5

    -- unification property tests
    quickCheck prop_unify_reflexivity
    quickCheck prop_unify_commutativity
    quickCheck prop_unify_idempotence
