# Hello Haskell

The purpose of this exercise is to learn to create a new project using stack and run it.

1. Open terminal and navigate to location where you want to create a project
2. type `stack new helloHaskell && cd helloHaskell`
3. use `code .` to open `Visual Studio Code`
4. find out where the `Main.hs` and `Lib.hs` are located
5. create a method `sayHello` that takes a string and returns "hello " + provided string
6. update `someFunc` in `Lib.hs` to use `sayHello` method "hello haskell"(don't forget to export `sayHello` method)
7. run `stack run`. It should output "hello haskell" to the console

## How do I add tests

To update any of your dependencies you will need to update `package.yaml` file.
For example to add tests you will need:

1. Create a folder `test`
2. Create main test file `Tests.hs`
3. in `package.yaml` add:

```yaml
tests:
  test:
    main: Tests.hs
    source-dirs: test
    dependencies:
      - helloHaskell
```

4. add `hspec` to have a unit testing framework available in your tests [https://hspec.github.io/](https://hspec.github.io/)
5. update `Tests.hs`

```haskell
import Test.Hspec        (Spec, it, describe, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

-- example import
import Lib (sayHello)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

-- example test
specs :: Spec
specs = describe "Test cases" $ do
        it "says `hello World`" $
          sayHello "World"  `shouldBe` "hello World"
```

6. Run `stack tests --file-watch`, `--file-watch` flag adds continues re-compile after every file change
