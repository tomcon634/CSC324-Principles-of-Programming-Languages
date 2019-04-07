{- CSC324 Fall 2018: Exercise 6

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}

-- The module definition line, including exports. Don't change this!
module Ex6 (expressionsOfRank,
            arithmeticExpressions,
            BinaryGrammar(..),
            expressionsOfRankGen,
            expressions) where

-- You *may* add imports from Data.List (but no other imports).
import Data.List (sort)
import Test.QuickCheck (Property, quickCheck)


-------------------------------------------------------------------------------
-- |
-- = Task 1: A small arithmetic expression grammar

-- | Helpers to define the different parts of the grammar.
-- Remember that we're representing all types of expressions as strings here.
numbers :: [String]
numbers = ["1", "2", "3", "4"]

makePlus :: String -> String -> String
makePlus expr1 expr2 = "(+ " ++ expr1 ++ " " ++ expr2 ++ ")"

makeTimes :: String -> String -> String
makeTimes expr1 expr2 = "(* " ++ expr1 ++ " " ++ expr2 ++ ")"


-- | Returns a list of expressions *of rank k* following the arithmetic expression
-- grammar from the exercise handout. The expressions can be in any order.
-- Order matters in an expression (so `(+ 3 4)` is different from `(+ 4 3)`),
-- and no expression should appear twice in the output list.
--
-- Precondition: k >= 0.
--
-- Hints:
--   1. The only expressions at rank 0 are the numeric literals.
--   2. This function can (and probably should) be recursive!
--   3. Remember that you aren't *evaluating* any expressions here.
--      Don't get distracted trying to "evaluate" "(+ 3 4)" into "7".
--   4. Make helper function(s)! This function is quite elegant if you do some
--      design work to think about what helper(s) you really need here.
--   5. Spend some time reading through the Haskell List documentation
--      to get some ideas about the functions you might find useful.
--      https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html
--      In particular, concatMap is pretty sweet. :)
--
--      Along the same lines, http://learnyouahaskell.com/starting-out#texas-ranges.
--      (Note: [0..(-1)] is an empty list.)
expressionsOfRank :: Int -> [String]
expressionsOfRank 0 = ["1", "2", "3", "4"]
expressionsOfRank k = (concatMap 
    (mapExpression2 k)
    --(expressionsOfRank (k - 1))
    (expressionsOfRank (k - 1)))
    ++
    (concatMap (mapExpression3 k) (concatMap expressionsOfRank [0..(k - 2)]))

mapExpression3 k expr = concatMap (mapExpression expr) 
    (expressionsOfRank (k - 1))

mapExpression2 k expr = concatMap (mapExpression expr) 
    (concatMap expressionsOfRank [0..(k - 1)])

mapExpression :: String -> String -> [String]
mapExpression expr expr2 = [makePlus expr expr2, makeTimes expr expr2]

-- | An infinite list containing all arithmetic expressions (again, no duplicates),
-- in *non-decreasing rank order*. Expressions of the same rank may appear in any order.
arithmeticExpressions :: [String]
arithmeticExpressions = concatMap expressionsOfRank [0..]


-------------------------------------------------------------------------------
-- | Exact test for rank 0 expressions.
prop_arithRank0Exact :: Bool
prop_arithRank0Exact =
    sort (expressionsOfRank 0) == numbers

-- | Test for number of rank 1 expressions.
prop_arithRank1Length :: Bool
prop_arithRank1Length =
    length (sort (expressionsOfRank 1)) == 2 * (length numbers) * (length numbers)

-- | Most naive implementations will miss this. Be careful here!
-- Also, see the note on the handout about efficiency.
prop_arithRank3ElemCheck :: Bool
prop_arithRank3ElemCheck =
    elem "(+ (* (+ 3 2) 4) 1)" (expressionsOfRank 3)


-------------------------------------------------------------------------------
-- |
-- = Task 2: Generalize!

-- | This data type represents a binary recursive grammar.
-- The first argument to the constructor is a list of the *atoms* of the grammar,
-- and the second is a list of the *recursive rules* of the grammar.
data BinaryGrammar = BinaryGrammar [String] [String -> String -> String]

-- | Example: arithmetic expressions.
arithmeticGrammar :: BinaryGrammar
arithmeticGrammar = BinaryGrammar numbers [makePlus, makeTimes]

-- | Another example: the one from Task 2 in the handout.
lifeChoicesGrammar :: BinaryGrammar
lifeChoicesGrammar =
    BinaryGrammar
    ["cats", "dogs", "bird", "love", "terror", "hunger"]
    [makeAnd, makeOr]

    where
        -- We use the "where" keyword to define some local helper functions.
        -- Note that this is indented inside the definition of lifeChoicesGrammar.
        makeAnd :: String -> String -> String
        makeAnd expr1 expr2 = expr1 ++ " and " ++ expr2 ++ "!"

        makeOr :: String -> String -> String
        makeOr expr1 expr2 = expr1 ++ " or " ++ expr2 ++ "?"


-- | Returns a list of expressions *of rank k* following the given grammar.
-- The expressions can be in any order.
--
-- Precondition: k >= 0.
--
expressionsOfRankGen :: BinaryGrammar -> Int -> [String]
expressionsOfRankGen (BinaryGrammar atoms rules) 0 = atoms
expressionsOfRankGen (BinaryGrammar atoms rules) k = (concatMap 
    (mapExpr (BinaryGrammar atoms rules) k)
    (expressionsOfRankGen (BinaryGrammar atoms rules) (k - 1)))
    ++
    (concatMap (mapExpr2 (BinaryGrammar atoms rules) k)
    (concatMap (expressionsOfRankGen (BinaryGrammar atoms rules)) [0..(k - 2)]))

mapExpr2 (BinaryGrammar atoms rules) k expr =
    concatMap
    (makeExprs (BinaryGrammar atoms rules) expr)
    (expressionsOfRankGen (BinaryGrammar atoms rules) (k - 1))

mapExpr (BinaryGrammar atoms rules) k expr = 
    concatMap 
    (makeExprs (BinaryGrammar atoms rules) expr) 
    (concatMap (expressionsOfRankGen (BinaryGrammar atoms rules)) [0..(k - 1)])

makeExprs (BinaryGrammar atoms rules) expr1 expr2 = concatMap (makeExpr expr1 expr2) rules

--makeExpr :: String -> String -> (String -> String -> String) -> String
makeExpr expr1 expr2 rule = [rule expr1 expr2]

--makeExpr (BinaryGrammar atoms rules) expr1 expr2 = 
    --[(head rules) expr1 expr2, (last rules) expr1 expr2]
    --[makeAnd expr1 expr2, makeOr expr1 expr2]

-- | A generalization of arithmeticExpressions.
-- Same restrictions (e.g., ordered by rank) apply.
expressions :: BinaryGrammar -> [String]
expressions grammar = concatMap (expressionsOfRankGen grammar) [0..]


-------------------------------------------------------------------------------
-- | A few tests to show that these functions really do generalize Task 1.
prop_expressionsOfRankGen0 :: Bool
prop_expressionsOfRankGen0 =
    sort (expressionsOfRank 0) == sort (expressionsOfRankGen arithmeticGrammar 0)

prop_expressionsOfRankGen1 :: Bool
prop_expressionsOfRankGen1 =
    sort (expressionsOfRank 1) == sort (expressionsOfRankGen arithmeticGrammar 1)


-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_arithRank0Exact
    quickCheck prop_arithRank1Length
    quickCheck prop_arithRank3ElemCheck
    quickCheck prop_expressionsOfRankGen0
    quickCheck prop_expressionsOfRankGen1
