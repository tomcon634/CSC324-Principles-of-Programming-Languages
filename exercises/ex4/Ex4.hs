{- CSC324 Fall 2018: Exercise 4

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}

-- The module definition line, including exports. Don't change this!
module Ex4 (
    Expr(..),
    analyzeStrictness
    ) where

-- Imports the Data.Map library, to be used to represent the environment.
-- See http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html
-- for details.
-- Note that this is a qualified import, meaning you'll need to prefix all
-- identifiers imported from this library with "Map.", e.g. Map.findWithDefault.
import qualified Data.Map.Strict as Map

-- Use the operator ! to do a hash lookup,
-- e.g. `myMap ! myKey` returns the corersponding value
import Data.Map.Strict ((!))

-- Useful for index-based list operations.
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#g:16
import qualified Data.List as List

-- | Imports used for testing purposes only.
import Control.Monad (liftM, liftM2, liftM3)
import Test.QuickCheck (
    Property, quickCheck
    )


-------------------------------------------------------------------------------
-- |
-- = Task 1: Strictness analysis
-------------------------------------------------------------------------------
-- This is the main "Expr" data type described in the handout.
-- Note that we use strings for identifiers, and so string "+" represents the
-- special "built-in" addition operation.
data Expr
    = Number Int            -- ^ A numeric literal
    | Identifier String     -- ^ An identifier name
    | If Expr Expr Expr     -- ^ Ternary if: (if cond then else)
    | Call String [Expr]    -- ^ A function call
                            -- (first argument is the NAME of a function,
                            -- [Expr] is the argument list)

    | FuncDef String [String] Expr  -- ^ A function definition
                                    -- (function name, list of parameters, and body)
    deriving (Show, Eq)


-- |
-- == Your Task: implement `analyzeStrictness` according to the exercise specification.
-- You may assume that *every* input Expr is a FuncDef (so you don't have to pattern-match
-- on the other kinds of expressions).
-- Pay attention to the return type! We've included a link to the relevant documentation above.
--
-- Implementation hints:
--  1.  Same as Exercise 3, the main work can be done by processing the list of
--      function definitions in a call to foldl.
--      (But WARNING: the function passed to foldl takes its arguments in the opposite
--      order as Racket.)
--  2.  Working with list indexes is a bit more annoying in pure functional land.
--      Use the list function `List.findIndices`, which is similar to `filter` except
--      it returns indexes rather than elements.
analyzeStrictness :: [Expr] -> Map.Map String [Int]
analyzeStrictness funcDefs = undefined

helper :: Expr -> Map.Map String [Int] -> Map.Map String [Int--


-- |
-- Returns whether id is strict in the given expression.
-- Uses the given strictness map to determine strictness in function calls.
--
-- NOTE: this function isn't being tested explicitly, so you may freely change it or
-- ignore it for this exercise.
--
-- Implementation hints:
--   Lookup List.any and List.all to achieve something like "ormap"/"andmap" to a list.
--
--   Use the infix operator ! to to a map lookup. You can assume the identifier
--   you're looking for will always be in the given map.
strictIn :: Map.Map String [Int] -> String -> Expr -> Bool
strictIn sMap x (Number i) = False
strictIn sMap x (Identifier s) = s == x
strictIn sMap x (If cond t e) = (strictIn sMap x cond) || (strictIn sMap x t && strictIn sMap x e)
strictIn sMap x (Call s e) = (s == x) || (List.any (strictIn sMap x) e)
strictIn sMap x expr = False

-------------------------------------------------------------------------------

-- | For testing, we'll use hand-crafted test cases here, parallel to the Racket version.

prop_identity :: Bool
prop_identity =
    analyzeStrictness [FuncDef "f" ["x"] (Identifier "x")] ==
        Map.fromList[("f", [0])]

prop_oneFunctionIf1 :: Bool
prop_oneFunctionIf1 =
    analyzeStrictness [
        FuncDef "f" ["x"]
                (If (Identifier "x") (Number 3) (Number 5))
    ] ==
        Map.fromList[("f", [0])]


prop_oneFunctionPlus :: Bool
prop_oneFunctionPlus =
    analyzeStrictness [
        FuncDef "f" ["x", "y", "z"]
                (Call "+" [Identifier "x", Identifier "y"])
    ] ==
        Map.fromList[("f", [0, 1])]

-------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_identity
    quickCheck prop_oneFunctionIf1
    quickCheck prop_oneFunctionPlus
