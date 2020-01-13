{-|
Module      : Css.Selector.Utils
Description : A set of utility methods to encode and decode strings.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to encode and decode css selector strings. These are used in the parser and renderer to parse and render css selector strings.
-}
module Css.Selector.Utils (
    -- * Identifiers
    readIdentifier, encodeIdentifier,
    -- * Css strings
    readCssString, encodeString, encodeText
  ) where

import Data.Char(chr, digitToInt, intToDigit, isAscii, isControl, isHexDigit, ord)
import Data.Text(Text, cons, pack, singleton, snoc)
import qualified Data.Text as T

-- | Parses a css string literal to a string that ontains the content of that
-- string literal.
readCssString :: String  -- ^ The string that contains the string literal in the css selector.
    -> String -- ^ A string that contains the content of the string literal.
readCssString ('"':xs) = _readCssString '"' xs
readCssString ('\'':xs) = _readCssString '\'' xs
readCssString _ = error "The string should start with an \" or '."

_readCssString :: Char -> String -> String
_readCssString c' = go
    where go "" = error "The string should end with " ++ c' : "."
          go [c] | c' == c = ""
          go ('\\':'\n':xs) = go xs
          go ('\\':ca@(c:xs)) | c == c' = c : go xs
                              | otherwise = let ~(y,ys) = parseEscape ca in y : go ys
          go (x:xs) | x == c' = error "The string can not contain a " ++ show x ++ ", you should escape it."
                    | otherwise = x : go xs

-- | Parse a given css identifier to the content of the identifier.
readIdentifier :: String -- ^ The given css identifier to parse.
    -> String -- ^ The result of the parsing: the content of the identifier.
readIdentifier = id

_notEncode :: Char -> Bool
_notEncode '\\' = False
_notEncode c = isAscii c && not (isControl c)

-- | Convert a string to a css selector string literal. This is done by putting
-- quotes around the content, and escaping certain characters.
encodeString :: Char -- ^ The type of quotes that should be put around the content (should be @'@ or @"@).
    -> String -- ^ The string that should be converted to a css selector string literal.
    -> String -- ^ The corresponding css selector string literal.
encodeString c' = (c' :) . go
    where go [] = [c']
          go (c:cs) | c == c' = '\\' : c : go cs
                    | _notEncode c = c : go cs
                    | otherwise = '\\' : showHex (ord c) (go cs)

-- | Convert a string to a css selector string literal. This is done by putting
-- quotes around the content, and escaping certain characters.
encodeText :: Char -- ^ The type of quotes that should be put around the content (should be @'@ or @"@).
    -> Text -- ^ The string that should be converted to a css selector string literal.
    -> Text -- ^ The corresponding css selector string literal.
encodeText c' t = cons c' (snoc (T.concatMap (_encodeCharacter c') t) c')

_encodeCharacter :: Char -> Char -> Text
_encodeCharacter c' c
    | c == c' = cons '\\' (singleton c)
    | _notEncode c = singleton c
    | otherwise = cons '\\' (pack (showHex (ord c) ""))

-- | Encode a given identifier to its css selector equivalent by escaping
-- certain characters.
encodeIdentifier :: Text -- ^ The identifier to encode.
    -> Text -- ^ The encoded identifier.
encodeIdentifier = T.concatMap (_encodeCharacter '\\')

showHex :: Int -> ShowS
showHex = go (6 :: Int)
    where go 0 _ s = s
          go k n rs = go (k-1) q (intToDigit r : rs)
              where ~(q, r) = quotRem n 16

parseEscape :: String -> (Char, String)
parseEscape = go (6 :: Int) 0
    where go 0 n cs = yield n cs
          go _ n "" = yield n ""
          go i n ca@(c:cs) | isHexDigit c = go (i-1) (16*n+digitToInt c) cs
                           | otherwise = yield n ca
          yield n s = (chr n, s)
