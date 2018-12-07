# Additional Resources for curious readers

## Infix operators

Resource: [haskell infix documentation](https://www.haskell.org/onlinereport/decls.html)

There are three kinds of `fixity`, `non-`, `left-` and `right-associativity` (`infix`, `infixl`, and `infixr`, respectively), and ten precedence levels, `0` to `9` inclusive (level `0` binds least tightly, and level `9` binds most tightly). If the digit is omitted, level `9` is assumed. Any operator lacking a `fixity` declaration is assumed to be `infixl` `9`.

| Precedence |                           Left associative                            |                          Non associative                           |    Right associative     |
| ---------- | --------------------------------------------------------------------- | ------------------------------------------------------------------ | ------------------------ |
| 9          | `!!`                                                                  | -                                                                  | `.`                      |
| 8          | -                                                                     | -                                                                  | `^`, `^^`, `**`          |
| 7          | `*`, `/`, ``` `div` ```, ``` `mod` ```, ``` `rem` ```, ``` `quot` ``` | -                                                                  | -                        |
| 6          | `+`, `-`                                                              | -                                                                  | -                        |
| 5          | -                                                                     | -                                                                  | `:`, `++`                |
| 4          | -                                                                     | `==`, `/=`,`<`, `<=`, `>`, `>=`, ``` `elem` ```, ``` `notElem` ``` | -                        |
| 3          | -                                                                     | -                                                                  | `&&`                     |
| 2          | -                                                                     | -                                                                  | `||`                     |
| 1          | `>>`, `>>=`                                                           | -                                                                  | -                        |
| 0          | -                                                                     | -                                                                  | `$`, `$!`, ``` `seq` ``` |

## Imports

Resource: [`haskell import documentation`](https://wiki.haskell.org/Import)

Supposing that the module `Mod` exports three functions `x`, `y`, `z`.

|            Import command            |        What is brought into scope        |
| ------------------------------------ | ---------------------------------------- |
| `import Mod`                         | `x`, `y`, `z`, `Mod.x`, `Mod.y`, `Mod.z` |
| `import Mod ()`                      |                                          |
| `import Mod (x, y)`                  | `x`, `y`,`Mod.x`, `Mod.y`                |
| `import qualified Mod`               | `Mod.x`, `Mod.y`, `Mod.z`                |
| `import qualified Mod (x, y)`        | `Mod.x`, `Mod.y`                         |
| `import Mod hiding (x, y)`           | `z`, `Mod.z`                             |
| `import qualified Mod hiding (x, y)` | `Mod.z`                                  |
| `import Mod as Foo`                  | `x`, `y`, `z`, `Foo.x`, `Foo.y`, `Foo.z` |
| `import Mod as Foo (x, y)`           | `x`, `y`,`Foo.x`, `Foo.y`                |
| `import qualified Mod as Foo`        | `Foo.x`, `Foo.y`, `Foo.z`                |
| `import qualified Mod as Foo (x, y)` | `Foo.x`, `Foo.y`                         |