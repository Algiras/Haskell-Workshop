# Syntax

Based on this [CheatSheet](http://cheatsheet.codeslower.com/CheatSheet.pdf).

## Values

`::` can be read as "has type". Functions in `Haskell` are declared as series of `equations`.

```haskell

type String = [Char]
type List a = Cons a (List a) | Nil

5 :: Integer
'a' :: Char
inc :: Integer -> Integer
[1, 2, 3] :: [Integer]
('b', 4) :: (Char, Integer)
```

### Lambdas

Anonymous functions or `lambda expressions` are noted as

```xml
\<function-params> -> <function-implementation>
```

```haskell
\c -> c
```

## User defined types

```xml
data <Pascal-case-name> = <first-constructor> | <second-constructor>
data <Pascal-case-name> <letter-denoting-type> = <first-constructor> <letter-denoting-type> | <second-constructor>
```

```haskell

data Color = Red | Green | Blue
data Point a = Pnt a a
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

## Scoped values

```xml
let <reference-name> = <reference-value>
    <reference-name-2> = <reference-value-2>
    in <expression-using-all-named-references>
```

```haskell
someValue = let y = 1 + 2
                z = 4 + 5
                in y + z

someValue = let {y = 1 + 2; z = 4 + 5} in y + z
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