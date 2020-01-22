import Css.Selector
import Css.Selector.Utils(encodeString, readCssString)

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
        testProperty "Encode-decode css identity: selector group" (encodeDecodeCss' :: SelectorGroup -> Bool),
        testProperty "Encode-decode css identity: selector" (encodeDecodeCss' :: Selector -> Bool),
        testProperty "Encode-decode css identity: selector sequence" (encodeDecodeCss' :: SelectorSequence -> Bool),
        testProperty "Encode-decode css identity: selector filter" (encodeDecodeCss' :: SelectorFilter -> Bool),
        testProperty "Encode-decode css identity: namespace" (encodeDecodeCss' :: Namespace -> Bool),
        testProperty "Encode-decode css identity: element name" (encodeDecodeCss' :: ElementName -> Bool),
        testProperty "Encode-decode css identity: type selector" (encodeDecodeCss' :: TypeSelector -> Bool),
        testProperty "Encode-decode css identity: attribute" (encodeDecodeCss' :: Attrib -> Bool),
        testProperty "Encode-decode css identity: attribute name" (encodeDecodeCss' :: AttributeName -> Bool),
        testProperty "Encode-decode css identity: class" (encodeDecodeCss' :: Class -> Bool),
        testProperty "Encode-decode css identity: hash" (encodeDecodeCss' :: Hash -> Bool)
    ],
    testGroup "SelectorSequences" [
        testProperty "Adding and removing filters" addRemFilters
    ],
    testGroup "Normalization" [
        testProperty "Normalized variant has the same specificity" normSpec,
        testProperty "Normalization is idempotent" normIdem
    ],
    testGroup "Build an expression or pattern" [
        testProperty "Check build of expression" buildExpression,
        testProperty "Check build of pattern 1" buildPattern1,
        testProperty "Check build of pattern 2" buildPattern2
    ]
  ]

encodeDecode :: Char -> String -> Bool
encodeDecode c b = readCssString (encodeString c b) == b

encodeDecodeId :: String -> Bool
encodeDecodeId b = readIdentifier (unpack (encodeIdentifier (pack b))) == b

encodeDecodeCss :: SelectorGroup -> Bool
encodeDecodeCss sg = sg == (parseCss . unpack . toCssSelector) sg

encodeDecodeCss' :: ToCssSelector a => a -> Bool
encodeDecodeCss' sg = (parseCss . unpack . toCssSelector . toSelectorGroup) sg == toSelectorGroup sg

-- TODO: complete
buildExpression :: SelectorGroup -> Bool
buildExpression _ = True

buildPattern1 :: SelectorGroup -> Bool
buildPattern1 x = toPattern x == toPattern x -- we use equality checks to force evaluation

buildPattern2 :: SelectorGroup -> SelectorGroup -> Bool
buildPattern2 x y = (x == y) == (toPattern x == toPattern y)

addRemFilters :: TypeSelector -> [SelectorFilter] -> Bool
addRemFilters x fs = filters (addFilters (SimpleSelector x) fs) == fs

normSpec :: SelectorGroup -> Bool
normSpec x = specificity' x == specificity' nx
    where nx = normalize x

normIdem :: SelectorGroup -> Bool
normIdem x = normalize nx == nx
    where nx = normalize x
