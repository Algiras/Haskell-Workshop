# Syntax

Based on this [CheatSheet](http://cheatsheet.codeslower.com/CheatSheet.pdf) and [Haskell Wiki](https://wiki.haskell.org/Haskell).

## Terminology

- `::` - has type
  - `5 :: Integer` = 5 `has type` `Integer`
- `type`
  - `type String = [Char]` - alias for type `String` is `List`(`[]`) of `Char`
- `Algebraic data type`
  - `product type` 
    - `data Option a = Some a | None` = product type `Option` of `*` type has 2 constructors `Some` and `None`. `Some` constructor takes an argument that describes type of `Option`.
    - `(Int, String)` = tuple consisting out of `Int` and `String`. [tuple docs](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Tuple.html)
- `Lambda` - anonymous function, that can be expressed without giving it a name
  - `\a b -> a + b` = lambda function that takes 2 parameters that support `+` operation

## Module

`module {Module_Name} where` needs to be at the top of every file to describe the name of the Module. In cases where applicable using `module {Module_Name} ({function-in-scope-1}, {function-in-scope-2}) where` syntax we can select parts of the code to export.

```haskell
module MyModule (remove_e, add_two) where

add_one blah = blah + 1 -- module does not export this

remove_e text = filter (/= 'e') text

add_two blah = add_one . add_one $ blah
```

## Imports

Supposing that the module `Mod` exports three functions `x`, `y`, `z`.
We can either import all values `import Mod` or select particular function`import Mod (x, y)`. For more options read [AdditionalResource](/AdditionalResources.md#Imports)

## Language pragmas

A language pragma directs the Haskell compiler to enable an extension or modification of the Haskell language.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- OR
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
```

## User defined types

```xml
| data <Pascal-case-name> = <first-constructor>                                               | <second-constructor> |
| data <Pascal-case-name> <letter-denoting-type> = <first-constructor> <letter-denoting-type> | <second-constructor> |
```

```haskell

data Color = Red | Green | Blue
data Point a = Pnt a a
data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- Records
data Person = Person String Int deriving (Show)
data BetterPerson = { name :: String
                    , age  :: Int
                    } deriving (Show)
```

## Control structures

### If

```xml
if <condition> then <true-value> else <false-value>
```

```haskell

-- inline
g x y = (f x == 0 then 1 else sin x / x) * y

describeLetter :: Char -> String
describeLetter c = if c > 'a' && c <= 'z'
    then "Lower case"
    else if c >= 'A' && c <= 'Z'
        then "Upper case"
        else "Not an ASCII letter"

-- Guards

describeLetter :: Char -> String
describeLetter c
    | c > 'a' && c <= 'z' = "Lower case"
    | c >= 'A' && c <= 'Z' = "Upper case"
    | otherwise            = "Not an ASCII letter"
```

### Case Expression

```xml
case <matched-value> of
    <first-match>  -> <first-match-result>
    <second-match> -> <second-match-result>
    _              -> <default-match>
```

```haskell
f x = case x of
    0 -> 18
    1 -> 15
    2 -> 12
    _ -> 12 - x

data Colour = Black | White | RGB Int Int Int

describeBlackOrWhite :: Colour -> String
describeBlackOrWhite c =
    "This colour is"
    ++ case c of
        Black -> "black"
        White -> "White"
        RGB 0 0 0 -> "black"
        RGB 255 255 255 -> "black"
        _ -> "something else"

```

Pattern matching can also be used for:

```haskell
-- Separating type signatures
head :: [a] -> a
head (x:xs) = x
head [] = error "head of nil"

-- Capturing arguments
len :: Show a => [a] -> String
len ls@(l: _) = "List starts with " ++ show l ++ " and is " ++ show (length ls) ++ " items long."
len [] = "list is empty!"
```

## Scoped values

```xml
let <value-name> = <value>
    <value-name-2> = <value-2>
    in <expression-using-all-named-values>
```

```haskell
someValue = let y = 1 + 2
                z = 4 + 5
                in y + z

someValue = let {y = 1 + 2; z = 4 + 5} in y + z
```

```xml
<function-name> <params>
  | <condition1> = <result1> |
  | <condition2> = <result2> |
  where <value-used-in-both-contexts> = <value>
```

```haskell
hasVowel [] = False
hasVowel (x:xs)
  | x `elem` vowels = True |
  | otherwise = False      |
  where vowels = "AEIOUaeiou"
```

## Type Classes

```xml
class <type-class-name> <generic-type-reference> where
    <function1-name> :: <function1-type-signature>
    <function2-name> <function2-param> = <function2-implementation>
```

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    (/=) a b = not (a == b)

-- in some cases you can automatically derive TypeClasses
data Color = Black | White deriving (Eq)
```

### Type constrains on functions

```xml
<function-name> :: <constrain-and-name> => <name> -> <name>
<function-name> <function-parameter> = <function-implementation>
```

```haskell
add :: Num a => a -> a -> a
add a b = a + b
```

### Type instances

```xml
instance <Constrains> => <type-class-name> <instance-name> where
    <type-class-function1> = <type-class-function1-implementation>
```

```haskell
data Either a b = Left a | Right b deriving (Show)

instance Functor (Either e) where
    fmap fn fa = case fa of
        Right a -> Right $ fn a
        Left e -> Left e

instance Applicative (Either e) where
    pure = Right
    mFn <*> fa = case mFn of
        Right fn -> fmap fn fa
        Left e -> Left e

instance Monad (Either e) where
    fa >>= fn = case fa of
        Right v -> fn v
        Left e -> Left e
```

## Other

### List comprehensions

```xml
[<final-expression> | <first-expression>
                    , <second-expression>
                    , <third-expression-condition>
                    , <fourth-expression-let-block>]
```

```haskell
square :: [Integer]
square = [x * y | x <- [1..10]
                , y <- [1..20]]
```

## Resources

- [Learn you a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
- [Gentle introduction to Haskell](https://www.haskell.org/tutorial/haskell-98-tutorial.pdf)
- [zvon.org](http://zvon.org/other/haskell/Outputsyntax/)