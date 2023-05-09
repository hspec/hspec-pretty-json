# Pretty-print JSON values in Hspec failures

Here is a failing `spec`:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module README where
import           Test.Hspec
import qualified Test.Hspec.PrettyJSON as PrettyJSON
import           Data.Aeson
import           GHC.Generics

data Person = Person {
  name :: String
, age :: Int
} deriving (Eq, Show, Generic, ToJSON)

person :: Person
person = Person "Joe" 23

spec :: Spec
spec = do
  describe "PrettyJSON" $ do
    context "when comparing aeson Values" $ do
      it "pretty-prints JSON values" $ do
        toJSON person `shouldBe` toJSON person { age = 42 }

    context "when comparing Strings" $ do
      it "pretty-prints JSON values" $ do
        encode person `shouldBe` encode person { age = 42 }
```


When you run this `spec` with

```haskell ignore
main :: IO ()
main = hspec spec
```

you will get output of the form:

![without](https://github.com/hspec/hspec/assets/461132/18382eda-f3f0-46d5-bdb6-b7181e9b37be)

By using `PrettyJSON`

```haskell
main :: IO ()
main = hspec $ PrettyJSON.use spec
```
the JSON values will be pretty-printed:

![with](https://github.com/hspec/hspec/assets/461132/167a25de-c4c0-4a54-8c4d-b0ace322d721)
