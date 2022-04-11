{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Test.Hspec.PrettyJSONSpec (spec) where

import           Test.Hspec

import           Control.Arrow
import           Data.List
import           GHC.Generics (Generic)
import           Data.ByteString.Lazy (ByteString)
import           Data.Aeson (ToJSON, toJSON, Value, encode)

import           Test.Hspec.PrettyJSON

data Person = Person {
  name :: String
, age :: Int
} deriving (Eq, Show, Generic, ToJSON)

person :: Person
person = Person "Joe 👋" 23

spec :: Spec
spec = do
  describe "prettyJSON" $ do
    context "with a ByteString representation of JSON" $ do
      let
        json :: ByteString
        json = encode person

      it "pretty-prints expected / actual" $ do
        prettyJSON True (show json) (show json) `shouldBe` (Just . (id &&& id) . intercalate "\n") [
            "{"
          , "  \"age\": 23,"
          , "  \"name\": \"Joe 👋\""
          , "}"
          ]

      it "optionally escapes unicode" $ do
        prettyJSON False (show json) (show json) `shouldBe` (Just . (id &&& id) . intercalate "\n") [
            "{"
          , "  \"age\": 23,"
          , "  \"name\": \"Joe \\ud83d\\udc4b\""
          , "}"
          ]

    context "with a Value representation of JSON" $ do
      let
        json :: Value
        json = toJSON person

      it "pretty-prints expected / actual" $ do
        prettyJSON True (show json) (show json) `shouldBe` (Just . (id &&& id) . intercalate "\n") [
            "{"
          , "  \"age\": 23.0,"
          , "  \"name\": \"Joe 👋\""
          , "}"
          ]

      it "optionally escapes unicode" $ do
        prettyJSON False (show json) (show json) `shouldBe` (Just . (id &&& id) . intercalate "\n") [
            "{"
          , "  \"age\": 23.0,"
          , "  \"name\": \"Joe \\ud83d\\udc4b\""
          , "}"
          ]

  describe "prettyValue" $ do
    it "pretty-prints a Value" $ do
      prettyValue True (toJSON person) `shouldBe` intercalate "\n" [
          "{"
        , "  \"age\": 23,"
        , "  \"name\": \"Joe 👋\""
        , "}"
        ]

    it "optionally escapes unicode" $ do
      prettyValue False (toJSON person) `shouldBe` intercalate "\n" [
          "{"
        , "  \"age\": 23,"
        , "  \"name\": \"Joe \\ud83d\\udc4b\""
        , "}"
        ]

  describe "utf16Escape" $ do
    it "escapes a code point that consist of one code unit" $ do
      utf16Escape '\x03bb' `shouldBe` "\\u03bb"

    it "escapes a code point that consist of two code units" $ do
      utf16Escape '\x1f44b' `shouldBe` "\\ud83d\\udc4b"
