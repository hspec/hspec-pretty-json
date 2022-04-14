{-# LANGUAGE CPP #-}
module Test.Hspec.Pretty.Json (
  usePrettyJson
#ifdef TEST
, prettyJson
, prettyValue
, showHex
#endif
) where

import           Prelude
import           Control.Monad
import           Control.Applicative
import           Data.Bifunctor
import           Data.Char
import           Data.Maybe
import           Text.Read
import qualified Numeric
import           Data.Aeson hiding (encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LB
import           Data.Aeson.Encode.Pretty

import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Runner as Hspec

prettyJson :: Bool -> String -> String -> Maybe (String, String)
prettyJson unicode expected actual = bimap pretty pretty <$> recover expected actual
  where
    pretty = prettyValue unicode

recover :: String -> String -> Maybe (Value, Value)
recover expected actual =
      both readMaybe
  <|> both (readMaybe >=> decode)
  <|> both (readMaybe >=> decodeText)
  where
    both :: (String -> Maybe Value) -> Maybe (Value, Value)
    both f = (,) <$> f expected <*> f actual

    decodeText :: T.Text -> Maybe Value
    decodeText = decode . LB.fromStrict . T.encodeUtf8

prettyValue :: Bool -> Value -> String
prettyValue unicode = (if unicode then id else escapeUnicode) . encode
  where
    encode = T.unpack . T.decodeUtf8 . LB.toStrict . encodePretty' defConfig { confIndent = Spaces 2 }

    escapeUnicode :: String -> String
    escapeUnicode input = case input of
      x : xs | isAscii x -> x : escapeUnicode xs
      x : xs -> '\\' : 'u' : showHex (ord x) ++ escapeUnicode xs
      [] -> []

showHex :: Int -> String
showHex n = replicate (4 - length hex) '0' ++ hex
  where
    hex = Numeric.showHex n ""

usePrettyJson :: SpecWith a
usePrettyJson = modifyConfig $ \ c -> c {
  configPrettyPrintFunction = \ unicode expected actual -> fromMaybe (configPrettyPrintFunction c unicode expected actual) (prettyJson unicode expected actual)
}
