{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Css3.Selector.Utils
-- Description : A set of utility methods to encode and decode strings.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A module to encode and decode css selector strings. These are used in the parser and renderer to parse and render css selector strings.
module Css3.Selector.Utils
  ( -- * Identifiers
    readIdentifier,
    encodeIdentifier,
    isValidIdentifier,
    toIdentifier,

    -- * Css strings
    readCssString,
    encodeString,
    encodeText,
  )
where

import Control.Arrow (first)
import Data.Char (chr, digitToInt, intToDigit, isAsciiLower, isAsciiUpper, isHexDigit, ord)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.Text (Text, cons, pack, singleton, snoc)
import qualified Data.Text as T

_initLast :: [a] -> Maybe ([a], a)
_initLast [] = Nothing
_initLast (a : as) = Just (go as a)
  where
    go [] x = ([], x)
    go (y : ys) x = first (x :) (go ys y)

_isQuote :: Char -> Bool
_isQuote '"' = True
_isQuote '\'' = True
_isQuote _ = False

-- | Parses a css string literal to a string that ontains the content of that
-- string literal.
readCssString ::
  -- | The string that contains the string literal in the css selector.
  String ->
  -- | A string that contains the content of the string literal.
  String
readCssString (c : xs) | _isQuote c = f
  where
    f
      | Just (vs, c') <- _initLast xs = g c' vs
      | otherwise = "The string literal should contain at least two quotation marks."
      where
        g c' vs
          | c == c' = _readCssString c vs
          | otherwise = "The start and end quotation mark should be the same."
readCssString _ = error "The string should start with an \" or ' and end with the same quotation."

_readCssString :: Char -> String -> String
_readCssString c' = go
  where
    go [] = []
    go ('\\' : '\n' : xs) = go xs
    go ('\\' : ca@(c : xs))
      | c == c' = c : go xs
      | otherwise = let ~(y, ys) = _parseEscape ca in y : go ys
    go (x : xs)
      | x == c' = error "The string can not contain a " ++ show x ++ ", you should escape it."
      | otherwise = x : go xs

-- | Parse a given css identifier to the content of the identifier.
readIdentifier ::
  -- | The given css identifier to parse.
  String ->
  -- | The result of the parsing: the content of the identifier.
  String
readIdentifier = _readCssString '\\'

_notEncode :: Char -> Bool
_notEncode x = isAsciiLower x || isAsciiUpper x

-- | Convert a string to a css selector string literal. This is done by putting
-- quotes around the content, and escaping certain characters.
encodeString ::
  -- | The type of quotes that should be put around the content (should be @'@ or @"@).
  Char ->
  -- | The string that should be converted to a css selector string literal.
  String ->
  -- | The corresponding css selector string literal.
  String
encodeString c' = (c' :) . go
  where
    go [] = [c']
    go (c : cs)
      | _notEncode c = c : go cs
      | otherwise = '\\' : _showHex (ord c) (go cs)

-- | Convert a string to a css selector string literal. This is done by putting
-- quotes around the content, and escaping certain characters.
encodeText ::
  -- | The type of quotes that should be put around the content (should be @'@ or @"@).
  Char ->
  -- | The string that should be converted to a css selector string literal.
  Text ->
  -- | The corresponding css selector string literal.
  Text
encodeText c' t = cons c' (snoc (T.concatMap _encodeCharacter t) c')

_encodeCharacter :: Char -> Text
_encodeCharacter c
  | _notEncode c = singleton c
  | otherwise = cons '\\' (pack (_showHex (ord c) ""))

-- | Encode a given identifier to its css selector equivalent by escaping
-- certain characters.
encodeIdentifier ::
  -- | The identifier to encode.
  Text ->
  -- | The encoded identifier.
  Text
encodeIdentifier = T.concatMap _encodeCharacter

_showHex :: Int -> ShowS
_showHex = go (6 :: Int)
  where
    go 0 _ s = s
    go k n rs = go (k - 1) q (intToDigit r : rs)
      where
        ~(q, r) = quotRem n 16

_parseEscape :: String -> (Char, String)
_parseEscape = go (6 :: Int) 0
  where
    go 0 n cs = yield n cs
    go _ n "" = yield n ""
    go i n ca@(c : cs)
      | isHexDigit c = go (i - 1) (16 * n + digitToInt c) cs
      | otherwise = yield n ca
    yield n s = (chr n, s)

-- | Check if the given identifier is a valid css selector identifier.
isValidIdentifier ::
  -- | The given identifier to check.
  String ->
  -- | 'True' if the given identifier is valid, 'False' otherwise.
  Bool
isValidIdentifier = not . null

-- | Convert the given string to a given object by first checking if it is a
-- valid identifier, and if not raising an error. If it is a valid identifier,
-- the string is packed, and wrapped in the given function.
toIdentifier ::
  -- | The given function to wrap the 'Text' identifier to an object.
  (Text -> a) ->
  -- | The string to validate, and wrap into the given function.
  String ->
  -- | The identifier object to return if the identifier is valid.
  a
toIdentifier f ident
  | isValidIdentifier ident = f (pack ident)
  | otherwise = error ("The identifier \"" <> show ident <> "\" is not a valid identifier.")
