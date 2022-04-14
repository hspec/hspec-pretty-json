{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Test.Hspec.Pretty.JsonSpec (spec) where

import           Test.Hspec

import           Control.Arrow
import           Data.Char
import           Data.List
import           GHC.Generics
import           Data.Aeson hiding (json)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Test.Hspec.Pretty.Json

data Person = Person {
  name :: String
, age :: Int
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

person :: Person
person = Person "Jo\955e" 23

encodeUtf8 :: Text -> ByteString
encodeUtf8 = LB.fromStrict . T.encodeUtf8

quoted :: String -> String
quoted string = "\"" ++ string ++ "\""

shouldPrettyPrint :: Show a => a -> IO ()
shouldPrettyPrint json = do
  let Just (expected, actual) = prettyJson True (show json) (show json)
  expected `shouldBe` actual
  decode (encodeUtf8 $ T.pack expected) `shouldBe` Just person

spec :: Spec
spec = do
  describe "prettyJson" $ do
    it "pretty-prints expected / actual" $ do
      let json = show $ encode person
      prettyJson True json json `shouldBe` (Just . (id &&& id) . intercalate "\n") [
          "{"
        , "  \"age\": 23,"
        , "  \"name\": \"Jo\x03bb\&e\""
        , "}"
        ]

    it "optionally escapes unicode" $ do
      let json = show $ encode person
      prettyJson False json json `shouldBe` (Just . (id &&& id) . intercalate "\n") [
          "{"
        , "  \"age\": 23,"
        , "  \"name\": \"Jo\\u03bbe\""
        , "}"
        ]

    context "with a Value representation of JSON" $ do
      it "pretty-prints expected / actual" $ do
        let
          value :: Value
          value = toJSON person
        shouldPrettyPrint value

    context "with a ByteString representation of JSON (utf-8)" $ do
      it "pretty-prints expected / actual" $ do
        let
          utf8 :: ByteString
          utf8 = encode person
        shouldPrettyPrint utf8

    context "with a Text representation of JSON (utf-32)" $ do
      it "pretty-prints expected / actual" $ do
        let
          utf32 :: Text
          utf32 = T.decodeUtf8 . LB.toStrict $ encode person
        shouldPrettyPrint utf32

    context "with a representation that is both valid utf-8 and utf-32" $ do
      it "gives utf-8 precedence" $ do
        let
          ambiguous :: String
          ambiguous = show ['"', chr 0b110_11111, chr 0b10_111110, '"']
          asUtf32   =      [     chr 0b110_11111, chr 0b10_111110 ]
          asUtf8    =      [        chr 0b_11111___________111110 ]

          utf8 :: ByteString
          utf8 = read ambiguous

          utf32 :: Text
          utf32 = read ambiguous

          expected :: String
          expected = quoted asUtf8

        decode utf8 `shouldBe` Just (toJSON asUtf8)
        decode (encodeUtf8 utf32) `shouldBe` Just (toJSON asUtf32)

        prettyJson True ambiguous ambiguous `shouldBe` Just (expected, expected)

  describe "prettyValue" $ do
    it "pretty-prints a Value" $ do
      prettyValue True (toJSON person) `shouldBe` intercalate "\n" [
          "{"
        , "  \"age\": 23,"
        , "  \"name\": \"Jo\955e\""
        , "}"
        ]

    it "optionally escapes unicode" $ do
      prettyValue False (toJSON person) `shouldBe` intercalate "\n" [
          "{"
        , "  \"age\": 23,"
        , "  \"name\": \"Jo\\u03bbe\""
        , "}"
        ]

  describe "showHex" $ do
    it "shows a number as hexadecimal" $ do
      showHex 955 `shouldBe` "03bb"
