{-# LANGUAGE TypeApplications #-}

import Codec.Compression.GZip(defaultCompressParams)

import Css3.Selector
import Css3.Selector.Utils(encodeString, readCssString)

import Data.Binary(Binary, encode, decode)
import Data.Function(on)
import Data.Hashable(Hashable(hashWithSalt))
import Data.Text(pack, unpack)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Encode-decode strings" [
        testProperty "Encode-decode identity 1" (encodeDecode '"'),
        testProperty "Encode-decode identity 2" (encodeDecode '\''),
        testProperty "Encode-decode identifier" encodeDecodeId
    ],
    testGroup "Arbitrary css parsing" [
        testProperty "Encode-decode css identity" encodeDecodeCss,
        testProperty "Encode-decode css identity: selector group" (encodeDecodeCss' @SelectorGroup),
        testProperty "Encode-decode css identity: selector" (encodeDecodeCss' @Selector),
        testProperty "Encode-decode css identity: selector sequence" (encodeDecodeCss' @SelectorSequence),
        testProperty "Encode-decode css identity: selector filter" (encodeDecodeCss' @SelectorFilter),
        testProperty "Encode-decode css identity: namespace" (encodeDecodeCss' @Namespace),
        testProperty "Encode-decode css identity: element name" (encodeDecodeCss' @ElementName),
        testProperty "Encode-decode css identity: type selector" (encodeDecodeCss' @TypeSelector),
        testProperty "Encode-decode css identity: attribute" (encodeDecodeCss' @Attrib),
        testProperty "Encode-decode css identity: attribute name" (encodeDecodeCss' @AttributeName),
        testProperty "Encode-decode css identity: class" (encodeDecodeCss' @Class),
        testProperty "Encode-decode css identity: hash" (encodeDecodeCss' @Hash)
    ],
    testGroup "SelectorSequences" [
        testProperty "Adding and removing filters" addRemFilters
    ],
    testGroup "Normalization" [
        testProperty "Normalized variant has the same specificity" normSpec,
        testProperty "Normalization is idempotent" normIdem
    ],
    testGroup "Build an expression or pattern" [
        testProperty "Check build of pattern 1" buildPattern1,
        testProperty "Check build of pattern 2" buildPattern2
    ],
    testGroup "Convert to binary and back" [
        testProperty "Binary identity: selector group" (binaryEquivalent @SelectorGroup),
        testProperty "Binary identity: selector" (binaryEquivalent @Selector),
        testProperty "Binary identity: selector sequence" (binaryEquivalent @SelectorSequence),
        testProperty "Binary identity: selector filter" (binaryEquivalent @SelectorFilter),
        testProperty "Binary identity: namespace" (binaryEquivalent @Namespace),
        testProperty "Binary identity: element name" (binaryEquivalent @ElementName),
        testProperty "Binary identity: type selector" (binaryEquivalent @TypeSelector),
        testProperty "Binary identity: attribute" (binaryEquivalent @Attrib),
        testProperty "Binary identity: attribute name" (binaryEquivalent @AttributeName),
        testProperty "Binary identity: class" (binaryEquivalent @Class),
        testProperty "Binary identity: hash" (binaryEquivalent @Hash)
    ],
    testGroup "Check binary equality" [
        testProperty "Binary uniqness: selector group" (uniqnessEncoding @SelectorGroup),
        testProperty "Binary uniqness: selector" (uniqnessEncoding @Selector),
        testProperty "Binary uniqness: selector sequence" (uniqnessEncoding @SelectorSequence),
        testProperty "Binary uniqness: selector filter" (uniqnessEncoding @SelectorFilter),
        testProperty "Binary uniqness: namespace" (uniqnessEncoding @Namespace),
        testProperty "Binary uniqness: element name" (uniqnessEncoding @ElementName),
        testProperty "Binary uniqness: type selector" (uniqnessEncoding @TypeSelector),
        testProperty "Binary uniqness: attribute" (uniqnessEncoding @Attrib),
        testProperty "Binary uniqness: attribute name" (uniqnessEncoding @AttributeName),
        testProperty "Binary uniqness: class" (uniqnessEncoding @Class),
        testProperty "Binary uniqness: hash" (uniqnessEncoding @Hash)
    ],
    testGroup "Check hash constraint for the Hashable instances" [
        testProperty "Different hash implies different items: selector group" (hashingDifferent @SelectorGroup)
      , testProperty "Different hash implies different items: selector" (hashingDifferent @Selector)
      , testProperty "Different hash implies different items: selector sequence" (hashingDifferent @SelectorSequence)
      , testProperty "Different hash implies different items: selector filter" (hashingDifferent @SelectorFilter)
      , testProperty "Different hash implies different items: namespace" (hashingDifferent @Namespace)
      , testProperty "Different hash implies different items: element name" (hashingDifferent @ElementName)
      , testProperty "Different hash implies different items: type selector" (hashingDifferent @TypeSelector)
      , testProperty "Different hash implies different items: attribute" (hashingDifferent @Attrib)
      , testProperty "Different hash implies different items: attribute name" (hashingDifferent @AttributeName)
      , testProperty "Different hash implies different items: class" (hashingDifferent @Class)
      , testProperty "Different hash implies different items: hash" (hashingDifferent @Hash)
    ],
    testGroup "Encoding without compression" [
        testProperty "Check if encode/decode will not break the data: selector group" (decodeSame @SelectorGroup)
      , testProperty "Check if encode/decode will not break the data: selector" (decodeSame @Selector)
      , testProperty "Check if encode/decode will not break the data: selector sequence" (decodeSame @SelectorSequence)
      , testProperty "Check if encode/decode will not break the data: selector filter" (decodeSame @SelectorFilter)
      , testProperty "Check if encode/decode will not break the data: namespace" (decodeSame @Namespace)
      , testProperty "Check if encode/decode will not break the data: elementname" (decodeSame @ElementName)
      , testProperty "Check if encode/decode will not break the data: typeselector" (decodeSame @TypeSelector)
      , testProperty "Check if encode/decode will not break the data: attrib" (decodeSame @Attrib)
      , testProperty "Check if encode/decode will not break the data: attribute name" (decodeSame @AttributeName)
      , testProperty "Check if encode/decode will not break the data: class" (decodeSame @Class)
      , testProperty "Check if encode/decode will not break the data: hash" (decodeSame @Hash)
      , testProperty "Check if encode/decode will not break the data: selector specificity" (decodeSame @SelectorSpecificity)
    ],
    testGroup "Encoding with compression" [
        testProperty "Check if encode/decode will not break the data: selector group" (decodeCSame @SelectorGroup)
      , testProperty "Check if encode/decode will not break the data: selector" (decodeCSame @Selector)
      , testProperty "Check if encode/decode will not break the data: selector sequence" (decodeCSame @SelectorSequence)
      , testProperty "Check if encode/decode will not break the data: selector filter" (decodeCSame @SelectorFilter)
      , testProperty "Check if encode/decode will not break the data: namespace" (decodeCSame @Namespace)
      , testProperty "Check if encode/decode will not break the data: elementname" (decodeCSame @ElementName)
      , testProperty "Check if encode/decode will not break the data: typeselector" (decodeCSame @TypeSelector)
      , testProperty "Check if encode/decode will not break the data: attrib" (decodeCSame @Attrib)
      , testProperty "Check if encode/decode will not break the data: attribute name" (decodeCSame @AttributeName)
      , testProperty "Check if encode/decode will not break the data: class" (decodeCSame @Class)
      , testProperty "Check if encode/decode will not break the data: hash" (decodeCSame @Hash)
    ],
    testGroup "Encoding with compression with variable compress params" [
        testProperty "Check if encode/decode will not break the data: selector group" (decodePCSame @SelectorGroup)
      , testProperty "Check if encode/decode will not break the data: selector" (decodePCSame @Selector)
      , testProperty "Check if encode/decode will not break the data: selector sequence" (decodePCSame @SelectorSequence)
      , testProperty "Check if encode/decode will not break the data: selector filter" (decodePCSame @SelectorFilter)
      , testProperty "Check if encode/decode will not break the data: namespace" (decodePCSame @Namespace)
      , testProperty "Check if encode/decode will not break the data: elementname" (decodePCSame @ElementName)
      , testProperty "Check if encode/decode will not break the data: typeselector" (decodePCSame @TypeSelector)
      , testProperty "Check if encode/decode will not break the data: attrib" (decodePCSame @Attrib)
      , testProperty "Check if encode/decode will not break the data: attribute name" (decodePCSame @AttributeName)
      , testProperty "Check if encode/decode will not break the data: class" (decodePCSame @Class)
      , testProperty "Check if encode/decode will not break the data: hash" (decodePCSame @Hash)
    ],
    testGroup "Semigroup instances" [
        testProperty "Check if the semigroup is lawful: selector group" (semigroup @SelectorGroup)
      , testProperty "Check if the semigroup is lawful: selector" (semigroup @Selector)
      , testProperty "Check if the semigroup is lawful: namespace" (semigroup @Namespace)
      , testProperty "Check if the semigroup is lawful: elementname" (semigroup @ElementName)
    ],
    testGroup "Semigroup instances" [
        testProperty "Check if the monoid is lawful: namespace" (monoid @Namespace)
      , testProperty "Check if the monoid is lawful: elementname" (monoid @ElementName)
    ]

  ]

encodeDecode :: Char -> String -> Bool
encodeDecode c b = readCssString (encodeString c b) == b

encodeDecodeId :: String -> Bool
encodeDecodeId b = readIdentifier (unpack (encodeIdentifier (pack b))) == b

encodeDecodeCss :: SelectorGroup -> Bool
encodeDecodeCss sg = sg == (parseCss . unpack . toCssSelector) sg

decodeSame :: (Binary a, Eq a) => a -> Bool
decodeSame x = x == decode (encode x)

decodeCSame :: (Binary a, Eq a, ToCssSelector a) => a -> Bool
decodeCSame x = x == decompressDecode (compressEncode x)

decodePCSame :: (Binary a, Eq a, ToCssSelector a) => a -> Bool
decodePCSame x = x == decompressDecode (compressEncodeWith defaultCompressParams x)

binaryEquivalent :: (Binary a, Eq a, ToCssSelector a) => a -> Bool
binaryEquivalent x = decode (encode x) == x

hashingDifferent :: (Hashable a, Eq a) => Int -> a -> a -> Bool
hashingDifferent slt xa xb = (hashWithSalt slt xa == hashWithSalt slt xb) || (xa /= xb)

uniqnessEncoding :: (Binary a, Eq a, ToCssSelector a) => a -> a -> Bool
uniqnessEncoding ca cb = (encode ca == encode cb) == (ca == cb)

encodeDecodeCss' :: ToCssSelector a => a -> Bool
encodeDecodeCss' sg = (parseCss . unpack . toCssSelector . toSelectorGroup) sg == toSelectorGroup sg

buildPattern1 :: SelectorGroup -> Bool
buildPattern1 x = toPattern x == toPattern x -- we use equality checks to force evaluation

buildPattern2 :: SelectorGroup -> SelectorGroup -> Bool
buildPattern2 x y = (x == y) == (toPattern x == toPattern y)

addRemFilters :: TypeSelector -> [SelectorFilter] -> Bool
addRemFilters x fs = filters (addFilters (SimpleSelector x) fs) == fs

semigroup :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroup x y z = (x <> y) <> z == x <> (y <> z)

monoid :: (Eq a, Monoid a) => a -> a -> Bool
monoid x y = x <> mempty == mempty <> x && x == x <> mempty

normSpec :: SelectorGroup -> Bool
normSpec x = specificity' x == specificity' nx && specificity x == specificity nx
    where nx = normalize x

normIdem :: SelectorGroup -> Bool
normIdem x = normalize nx == nx
    where nx = normalize x
