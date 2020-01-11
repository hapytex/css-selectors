import Css.Selector
import Css.Selector.Utils(encodeString, readCssString)

import Data.Text(unpack)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Encode-decode strings" [
        testProperty "Encode-decode identity 1" (encodeDecode '"'),
        testProperty "Encode-decode identity 2" (encodeDecode '\'')
    ],
    testGroup "Arbitrary css parsing" [
        testProperty "Encode-decode css identity" encodeDecodeCss
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

encodeDecodeCss :: SelectorGroup -> Bool
encodeDecodeCss sg = sg == (parseCss . unpack . toCssSelector) sg

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
