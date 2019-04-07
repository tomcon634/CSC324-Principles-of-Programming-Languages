{- CSC324 Fall 2018: Exercise 2

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}

-- The module definition line, including exports. Don't change this!
module Ex2 (
    -- Task 1
    List(Empty, Cons),
    numEvensList,
    mapList,
    -- Task 2
    Expr(..),
    tailCalls
    ) where

-- | Imports used for testing purposes only.
import Control.Monad (liftM, liftM2, liftM3)
import Test.QuickCheck (
    Property, quickCheck, (==>), oneof, listOf1, sized, scale, Arbitrary(..)
    )


-------------------------------------------------------------------------------
-- |
-- = Task 1: A recursive type definition
-------------------------------------------------------------------------------

-- | Recursive definition of a *list of integers* type. (Don't change this!)
data List = Empty              -- ^ An empty list
          | Cons Int List      -- ^ An integer "cons'd" with another list
          deriving (Show, Eq)


-- | numEvensList returns the number of even items in a List.
-- Here you'll need to use pattern-matching on the different forms of a List
-- (`Empty` or `Cons`), because we don't have an explicit "is-empty?" function
-- for this datatype.
numEvensList :: List -> Int
numEvensList Empty =
    -- In this case, the list is empty.
    0
numEvensList (Cons first rest) =
    -- In this case, `first` is an Int representing the first item in the list,
    -- and `rest` is a List representing the other items in the list.
    if first `mod` 2 == 0
    then
        1 + numEvensList (rest)
    else
        numEvensList (rest)


-- | mapList behaves the same as `map` for built-in lists in Haskell.
mapList :: (Int -> Int) -> List -> List
mapList _ Empty = Empty
mapList f (Cons first rest) = Cons (f first) (mapList f rest)


-------------------------------------------------------------------------------

-- | For testing, we first need to define how to generate "random" Lists.
-- You can safely ignore this code for this exercise.
instance Arbitrary List where
    arbitrary = sized list'
        where
            list' 0 = return Empty
            list' n = oneof [return Empty, liftM2 Cons arbitrary arbitrary]


-- | What does this property mean?
prop_numEvensFirstOdd :: List -> Bool
prop_numEvensFirstOdd nums =
    numEvensList nums == numEvensList (Cons 1 nums)


-- | What does this property mean? (Hint: `id` is the identity function.)
prop_mapIdentity :: List -> Bool
prop_mapIdentity nums =
    -- Note: the reason we can use (==) here to compare Lists
    -- is because of the "deriving Eq" in the type definition.
    -- More on this later in the course.
    mapList id nums == nums


-- | What does this property mean?
prop_mapAdd2 :: List -> Bool
prop_mapAdd2 nums =
    let newNums = mapList (\x -> x + 2) nums
    in
        numEvensList nums == numEvensList newNums


-------------------------------------------------------------------------------
-- |
-- = Task 2: Tail call positions
-------------------------------------------------------------------------------

data Expr
    = Number Int          -- ^ A numeric literal
    | Boolean Bool        -- ^ A boolean literal
    | Identifier String   -- ^ An identifier name
    | And [Expr]          -- ^ An (and ...) expression
    | Or [Expr]           -- ^ An (or ...) expression
    | If Expr Expr Expr   -- ^ Ternary if: (if cond then else)
    | Call Expr [Expr]    -- ^ A function call
                          -- (first Expr is the function, [Expr] is the argument list)
    deriving (Show, Eq)


-- |
-- == Examples of expressions

-- The number 20.
numberExpr = Number 20

-- (and x #t #f)
andExpr = And [Identifier "x", Boolean True, Boolean False]

-- (if (f 1 2) (g) 40)
ifExpr = If (Call (Identifier "f") [Number 1, Number 2])
          (Call (Identifier "g") [])
          (Number 40)


-- |
-- == Your Task: implement `tailCalls` according to the exercise specification.
-- We've started the pattern-matching for you; feel free to modify as needed.
tailCalls :: Expr -> [Expr]
tailCalls (Number n) = []
tailCalls (Boolean b) = []
tailCalls (Identifier ident) = []
tailCalls (And exprs) = tailCalls (last exprs)
tailCalls (Or exprs) = tailCalls (last exprs)
tailCalls (If condExpr thenExpr elseExpr) = tailCalls (thenExpr) ++ tailCalls (elseExpr)
tailCalls (Call funcExpr args) = [Call funcExpr args] --tailCalls (funcExpr)


-------------------------------------------------------------------------------

-- | For testing, we first need to define how to generate "random" Exprs.
-- You can safely ignore this code for this exercise.
instance Arbitrary Expr where
  arbitrary = sized expr'
      where
          expr' 0 = oneof [
              liftM Number arbitrary
            , liftM Boolean arbitrary
            , liftM Identifier arbitrary
            ]
          expr' n = scale (`div` 2) $ oneof [
              liftM And (listOf1 arbitrary)
            , liftM Or (listOf1 arbitrary)
            , liftM3 If arbitrary arbitrary arbitrary
            , liftM2 Call arbitrary arbitrary
            ]


-- | This property checks that the only values in the output of `tailCalls`
-- are function call expressions.
prop_returnsAllCalls :: Expr -> Bool
prop_returnsAllCalls expr = all isCall (tailCalls expr)
  where
      isCall :: Expr -> Bool
      isCall (Call _ _) = True
      isCall _          = False


-------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_numEvensFirstOdd
    quickCheck prop_mapIdentity
    quickCheck prop_mapAdd2
    quickCheck prop_returnsAllCalls
