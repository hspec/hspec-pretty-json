{-# LANGUAGE ViewPatterns #-}
module Test.Hspec.PrettyJSON (
  use
, configSetPrettyJSON
, prettyJSON
, recover
, prettyValue
, utf16Escape
) where

import           Control.Applicative
import           Data.Bifunctor
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Word (Word8)
import           Text.Read
import qualified Numeric

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Aeson (Value, decode)
import           Data.Aeson.Encode.Pretty

import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Runner as Hspec

prettyJSON :: Bool -> String -> String -> Maybe (String, String)
prettyJSON unicode expected actual = bimap pretty pretty <$> recover expected actual
  where
    pretty = prettyValue unicode

recover :: String -> String -> Maybe (Value, Value)
recover expected actual =
      both readMaybe
  <|> both (readMaybe >=> decode)
  where
    both :: (String -> Maybe Value) -> Maybe (Value, Value)
    both f = (,) <$> f expected <*> f actual

prettyValue :: Bool -> Value -> String
prettyValue unicode = (if unicode then id else escapeUnicode) . encode
  where
    encode = T.unpack . T.decodeUtf8 . LB.toStrict . encodePretty' defConfig { confIndent = Spaces 2, confCompare = compare }

    escapeUnicode :: String -> String
    escapeUnicode input = case input of
      x : xs | isAscii x -> x : escapeUnicode xs
      x : xs -> utf16Escape x ++ escapeUnicode xs
      [] -> []

utf16Escape :: Char -> String
utf16Escape (return -> input) = case B.unpack . T.encodeUtf16BE $ T.pack input of
  [a, b] -> codeUnit a b
  [a, b, c, d] -> codeUnit a b <> codeUnit c d
  _ -> input
  where
    codeUnit a b = '\\' : 'u' : showHex a <> showHex b

showHex:: Word8 -> String
showHex n = replicate (2 - length hex) '0' ++ hex
  where
    hex = Numeric.showHex n ""

use :: SpecWith a -> SpecWith a
use = (modifyConfig configSetPrettyJSON >>)

configSetPrettyJSON :: Hspec.Config -> Hspec.Config
configSetPrettyJSON c = c {
  configPrettyPrintFunction = prettyPrint
} where
    prettyPrint unicode expected actual = fromMaybe fallback (prettyJSON unicode expected actual)
      where fallback = configPrettyPrintFunction c unicode expected actual
