Purepur
=======

Generate tests from examples in your purescript-docs.

Given:
```purescript
module Data.Array exposing …

-- | Convert a `Foldable` structure into an `Array`.
-- |
-- | ```purescript run
-- | > fromFoldable (Just 1)
-- | [1]
-- | ```
-- |
fromFoldable :: forall f. Foldable f => f ~> Array
fromFoldable = fromFoldableImpl foldr
```
purepur will generate:
```purescript
module Test.Example.Data.Array where 

…

main :: Spec Unit
main = describe "ArrayEx" $ do 
    it "value spec in docs from:fromFoldable" $ show (fromFoldable (Just 1)) `shouldEqual` "[1]"
```

Usage
------

For purepur to recognize an example as a test, add ` ```purescript run` to a comment in your purescript code, like shown here https://github.com/csicar/purescript-purepur/blob/master/example/src/ArrayEx.purs#L11.
The syntax of the code-fence is identical to that of PSCI. This means that copying the terminal output from PSCI to a docs comment _should_ directly work.

Now generate the test-files:

`purepure -o test/docs --src "src/**/*.purs" --src "./README.md" $(spago sources)`

- `-o` specifies the directory, where the test-files should be written to
- `--src` specified the glob, for which tests should be generated.

Now `test/docs` should look similar to this: https://github.com/csicar/purescript-purepur/blob/master/example/test/docs

In addition to the normal PSCI-Syntax, purepur allows __multiline__ expressions and declarations:


    ```purescript run
    > f x = 
        x + 1
    > f 2
    3
    ```


Building
--------

```bash
git clone …
cd …
stack install
```

Testing
-----

run in `example`

```bash
stack run -- -o test/docs --src "src/**/*.purs" (spago sources)
```


The project is forked from https://github.com/andyarvanitis/purescript-native
