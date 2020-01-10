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
    testGroup "Build an expression or pattern" [
        testProperty "Check build of expression" buildExpression,
        testProperty "Check build of pattern" buildPattern
    ]
  ]

encodeDecode :: Char -> String -> Bool
encodeDecode c b = readCssString (encodeString c b) == b

encodeDecodeCss :: SelectorGroup -> Bool
encodeDecodeCss sg = sg == (parseCss . unpack . toCssSelector) sg

-- TODO: complete
buildExpression :: SelectorGroup -> Bool
buildExpression _ = True

buildPattern :: SelectorGroup -> Bool
buildPattern x = (toPattern x) == (toPattern x) -- we use equality checks to force evaluation
