{-# LANGUAGE TypeApplications #-}

import Css3.Selector
import Css3.Selector.Utils(encodeString, readCssString)

import Data.Binary(Binary, encode, decode)
import Data.Function(on)
import Data.Hashable(Hashable(hashWithSalt))
import Data.Text(pack, unpack)

import Debug.Trace(trace)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Options
import Test.Framework.Runners.Options

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
        testProperty "Encode-decode css identity" encodeDecodeCss
      , testProperty "Encode-decode css identity: selector group" (encodeDecodeCss' @SelectorGroup)
      , testProperty "Encode-decode css identity: selector" (encodeDecodeCss' @Selector)
      , testProperty "Encode-decode css identity: selector sequence" (encodeDecodeCss' @SelectorSequence)
      , testProperty "Encode-decode css identity: selector filter" (encodeDecodeCss' @SelectorFilter)
      , testProperty "Encode-decode css identity: namespace" (encodeDecodeCss' @Namespace)
      , testProperty "Encode-decode css identity: element name" (encodeDecodeCss' @ElementName)
      , testProperty "Encode-decode css identity: type selector" (encodeDecodeCss' @TypeSelector)
      , testProperty "Encode-decode css identity: attribute" (encodeDecodeCss' @Attrib)
      , testProperty "Encode-decode css identity: attribute name" (encodeDecodeCss' @AttributeName)
      , testProperty "Encode-decode css identity: class" (encodeDecodeCss' @Class)
      , testProperty "Encode-decode css identity: pseudo class" (encodeDecodeCss' @PseudoClass)
      , testProperty "Encode-decode css identity: pseudo element" (encodeDecodeCss' @PseudoElement)
      , testProperty "Encode-decode css identity: pseudo selector sequence" (encodeDecodeCss' @PseudoSelectorSequence)
      , testProperty "Encode-decode css identity: negation" (encodeDecodeCss' @Negation)
    ],
    testGroup "SelectorSequences" [
        testProperty "Adding and removing filters" addRemFilters
    ],
    testGroup "Normalization" [
        testProperty "Normalized variant has the same specificity" normSpec
      , testProperty "Normalization is idempotent" normIdem
    ],
    testGroup "Build an expression or pattern" [
        testProperty "Check build of pattern 1" buildPattern1
      , testProperty "Check build of pattern 2" buildPattern2
    ],
    testGroup "Convert to binary and back" [
        testProperty "Binary identity: selector group" (binaryEquivalent @SelectorGroup)
      , testProperty "Binary identity: selector" (binaryEquivalent @Selector)
      , testProperty "Binary identity: selector sequence" (binaryEquivalent @SelectorSequence)
      , testProperty "Binary identity: selector filter" (binaryEquivalent @SelectorFilter)
      , testProperty "Binary identity: namespace" (binaryEquivalent @Namespace)
      , testProperty "Binary identity: element name" (binaryEquivalent @ElementName)
      , testProperty "Binary identity: type selector" (binaryEquivalent @TypeSelector)
      , testProperty "Binary identity: attribute" (binaryEquivalent @Attrib)
      , testProperty "Binary identity: attribute name" (binaryEquivalent @AttributeName)
      , testProperty "Binary identity: class" (binaryEquivalent @Class)
      , testProperty "Binary identity: hash" (binaryEquivalent @Hash)
      , testProperty "Binary identity: pseudo class" (binaryEquivalent @PseudoClass)
      , testProperty "Binary identity: pseudo element" (binaryEquivalent @PseudoElement)
      , testProperty "Binary identity: pseudo selector sequence" (binaryEquivalent @PseudoSelectorSequence)
      , testProperty "Binary identity: nth" (binaryEquivalent @Nth)
      , testProperty "Binary identity: negation" (binaryEquivalent @Negation)
    ],
    testGroup "Check binary equality" [
        testProperty "Binary uniqness: selector group" (uniqnessEncoding @SelectorGroup)
      , testProperty "Binary uniqness: selector" (uniqnessEncoding @Selector)
      , testProperty "Binary uniqness: selector sequence" (uniqnessEncoding @SelectorSequence)
      , testProperty "Binary uniqness: selector filter" (uniqnessEncoding @SelectorFilter)
      , testProperty "Binary uniqness: namespace" (uniqnessEncoding @Namespace)
      , testProperty "Binary uniqness: element name" (uniqnessEncoding @ElementName)
      , testProperty "Binary uniqness: type selector" (uniqnessEncoding @TypeSelector)
      , testProperty "Binary uniqness: attribute" (uniqnessEncoding @Attrib)
      , testProperty "Binary uniqness: attribute name" (uniqnessEncoding @AttributeName)
      , testProperty "Binary uniqness: class" (uniqnessEncoding @Class)
      , testProperty "Binary uniqness: hash" (uniqnessEncoding @Hash)
      , testProperty "Binary uniqness: pseudo class" (uniqnessEncoding @PseudoClass)
      , testProperty "Binary uniqness: pseudo element" (uniqnessEncoding @PseudoElement)
      , testProperty "Binary uniqness: pseudo selector sequence" (uniqnessEncoding @PseudoSelectorSequence)
      , testProperty "Binary uniqness: nth" (uniqnessEncoding @Nth)
      , testProperty "Binary uniqness: negation" (uniqnessEncoding @Negation)
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
      , testProperty "Different hash implies different items: pseudo class" (hashingDifferent @PseudoClass)
      , testProperty "Different hash implies different items: pseudo element" (hashingDifferent @PseudoElement)
      , testProperty "Different hash implies different items: pseudo selector sequence" (hashingDifferent @PseudoSelectorSequence)
      , testProperty "Different hash implies different items: nth" (hashingDifferent @Nth)
      , testProperty "Different hash implies different items: negation" (hashingDifferent @Negation)
    ],
    testGroup "Nths" [
        testProperty "Produces only (strictly) positive values" positiveNth
      , testProperty "Normalizing produces the same list" normSameNth
      , testProperty "Normalizing is idempotent" normalizeNthIdempotent
      , testProperty "Different normalized Nths have different values" (withMaxSuccess 5000 differentNormalizedNthsAreDifferent)
      , testProperty "nthIsEmpty checks" (withMaxSuccess 5000 checkIfNthIsEmpty)
      , testProperty "nthContainsValue contains all values" nthContainsSameItems
      , testProperty "nth contains same as its normalized counterpart" (withMaxSuccess 5000 normalizeContainsSame)
    ],
    testGroup "Parse examples" (map (\x -> testProperty ("test CSS selector " ++ x) (checkParse x)) validSelectors)
  ]

-- Based on the w3c testkit: https://test.csswg.org/harness/suite/selectors-3_dev/
validSelectors :: [String]
validSelectors = [
    "body > p"
  , "div ol>li p"
  , "*.pastoral"
  , ".pastoral"
  , "h1.pastoral"
  , "p.pastoral.marine"
  , "h1#chapter1"
  , "#chapter1"
  , "*#z98y"
  , "math + p"
  , "h1.opener + h2"
  , "h1 ~ pre"
  , "*"
  , "div :first-child"
  , "div *:first-child"
  , "body > h2:nth-of-type(n+2):nth-last-of-type(n+2)"
  , "body > h2:not(:first-of-type):not(:last-of-type)"
  , "h1, h2, h3"
  , "h1"
  , "foo|h1"
  , "foo|*"
  , "|h1"
  , "*|h1"
  , "*[hreflang|=en]"
  , "[hreflang|=en]"
  , "*.warning"
  , ".warning"
  , "*#myid"
  , "#myid"
  , "ns|*"
  , "*|*"
  , "|*"
  , "*"
  , "[att]"
  , "[att=val]"
  , "[att~=val]"
  , "[att|=val]"
  , "h1[title]"
  , "span[class=\"example\"]"
  , "span[hello=\"Cleveland\"][goodbye=\"Columbus\"]"
  , "a[rel~=\"copyright\"]"
  , "a[href=\"http://www.w3.org/\"]"
  , "a[hreflang=fr]"
  , "a[hreflang|=\"en\"]"
  , "DIALOGUE[character=romeo]"
  , "DIALOGUE[character=juliet]"
  , "[att^=val]"
  , "[att$=val]"
  , "[att*=val]"
  , "object[type^=\"image/\"]"
  , "a[href$=\".html\"]"
  , "p[title*=\"hello\"]"
  , "[foo|att=val]"
  , "[*|att]"
  , "[|att]"
  , "[att]"
  , "EXAMPLE[radix=decimal]"
  , "EXAMPLE[radix=octal]"
  , "EXAMPLE"
  , "*.pastoral"
  , ".pastoral"
  , "H1.pastoral"
  , "p.pastoral.marine"
  , "h1#chapter1"
  , "#chapter1"
  , "*#z98y"
  , "a.external:visited"
  , "a:link"
  , "a:visited"
  , "a:hover"
  , "a:active"
  , "a:focus"
  , "a:focus:hover"
  , "p.note:target"
  , "*:target"
  , "*:target::before"
  , "html:lang(fr-be)"
  , "html:lang(de)"
  , ":lang(fr-be) > q"
  , ":lang(de) > q"
  , "[lang|=fr]"
  , ":lang(fr)"
  , "[lang|=fr]"
  , "tr:nth-child(2n+1)"
  , "tr:nth-child(odd)"
  , "tr:nth-child(2n+0)"
  , "tr:nth-child(even)"
  , "p:nth-child(4n+1)"
  , "p:nth-child(4n+2)"
  , "p:nth-child(4n+3)"
  , "p:nth-child(4n+4)"
  , ":nth-child(10n-1)"
  , ":nth-child(10n+9)"
  , "foo:nth-child(0n+5)"
  , "foo:nth-child(5)"
  , "p"
  , "p::first-letter"
  , "span"
  , "h1 em"
  , "div * p"
  , "div p *[href]"
  , "body > p"
  , "div ol>li p"
  , "match + p"
  , "h1.opener + h2"
  , "h1 ~ pre"
  , "*"
  , "LI"
  , "UL LI"
  , "UL OL+LI"
  , "H1 + *[REL=up]"
  , "UL OL LI.red"
  , "LI.red.level"
  , "#x34y"
  , "#s12:not(FOO)"
  , "*"
  , "E"
  , "E[foo]"
  , "E[foo=\"bar\"]"
  , "E[foo~=\"bar\"]"
  , "E[foo^=\"bar\"]"
  , "E[foo$=\"bar\"]"
  , "E[foo*=\"bar\"]"
  , "E[foo|=\"en\"]"
  , "E:root"
  , "E:nth-child(n)"
  , "E:nth-last-child(n)"
  , "E:nth-of-type(n)"
  , "E:nth-last-of-type(n)"
  , "E:first-child"
  , "E:last-child"
  , "E:first-of-type"
  , "E:last-of-type"
  , "E:only-child"
  , "E:only-of-type"
  , "E:empty"
  , "E:link"
  , "E:visited"
  , "E:active"
  , "E:hover"
  , "E:focus"
  , "E:target"
  , "E:lang(fr)"
  , "E:enabled"
  , "E:disabled"
  , "E:checked"
  , "E::first-line"
  , "E::first-letter"
  , "E::before"
  , "E::after"
  , "E.warning"
  , "E#myid"
  , "E:not(s)"
  , "E F"
  , "E > F"
  , "E + F"
  , "E ~ F"
  , ".class"
  , ".class1.class2"
  , ".class1 .class2"
  , "#id"
  , "*"
  , "element"
  , "element.class"
  , "element,element"
  , "element element"
  , "element>element"
  , "element+element"
  , "element1~element2"
  , "[attribute]"
  , "[attribute=value]"
  , "[attribute~=value]"
  , "[attribute|=value]"
  , "[attribute^=value]"
  , "[attribute$=value]"
  , "[attribute*=value]"
  , ":active"
  , "::after"
  , "::before"
  , ":checked"
  , ":default"
  , ":disabled"
  , ":empty"
  , ":enabled"
  , ":first-child"
  , "::first-letter"
  , "::first-line"
  , ":first-of-type"
  , ":focus"
  , ":fullscreen"
  , ":hover"
  , ":in-range"
  , ":indeterminate"
  , ":invalid"
  , ":lang(language)"
  , ":last-child"
  , ":last-of-type"
  , ":link"
  , "::marker"
  , ":not(selector)"
  , ":nth-child(n)"
  , ":nth-last-child(n)"
  , ":nth-last-of-type(n)"
  , ":nth-of-type(n)"
  , ":only-of-type"
  , ":only-child"
  , ":optional"
  , ":out-of-range"
  , "::placeholder"
  , ":read-only"
  , ":read-write"
  , ":required"
  , ":root"
  , "::selection"
  , ":target"
  , ":valid"
  , ":visited"
  ]

checkParse :: String -> Bool
checkParse x = y == y
  where y = parseCss x

nthContainsSameItems :: Nth -> Bool
nthContainsSameItems nth = all (nthContainsValue nth) (take 5000 (nthValues nth))

normalizeContainsSame :: Nth -> Bool
normalizeContainsSame nth = all (\x -> nthContainsValue nth x == nthContainsValue nnth x) [0 .. 2000]
    where nnth = normalizeNth nth

equivalentNths :: [(Nth, Nth)]
equivalentNths = [
    (Nth 2 1, Odd)
  , (Nth 2 0, Even)
  , (Nth 10 (-1), Nth 10 9)
  ]

checkIfNthIsEmpty :: Nth -> Bool
checkIfNthIsEmpty nth = nthIsEmpty nth == null (nthValues nth)

positiveNth :: Nth -> Bool
positiveNth = all (0 <) . take 5000 . nthValues

normSameNth :: Nth -> Bool
normSameNth n = take 5000 (nthValues n) == take 5000 (nthValues (normalizeNth n))

normalizeNthIdempotent :: Nth -> Bool
normalizeNthIdempotent x = nx == normalizeNth nx where nx = normalizeNth x

differentNormalizedNthsAreDifferent :: Nth -> Nth -> Property
differentNormalizedNthsAreDifferent n1 n2 = within 1000000 (nn1 == nn2 || nthValues nn1 /= nthValues nn2)
    where nn1 = normalizeNth n1
          nn2 = normalizeNth n2

encodeDecode :: Char -> String -> Bool
encodeDecode c b = readCssString (encodeString c b) == b

encodeDecodeId :: String -> Bool
encodeDecodeId b = readIdentifier (unpack (encodeIdentifier (pack b))) == b

encodeDecodeCss :: SelectorGroup -> Bool
encodeDecodeCss sg = sg == (parseCss . unpack . toCssSelector) sg

binaryEquivalent :: (Binary a, Eq a) => a -> Bool
binaryEquivalent x = decode (encode x) == x

hashingDifferent :: (Hashable a, Eq a) => Int -> a -> a -> Bool
hashingDifferent slt xa xb = (hashWithSalt slt xa == hashWithSalt slt xb) || (xa /= xb)

uniqnessEncoding :: (Binary a, Eq a) => a -> a -> Bool
uniqnessEncoding ca cb = (encode ca == encode cb) == (ca == cb)

encodeDecodeCss' :: (Show a, ToCssSelector a) => a -> Bool
encodeDecodeCss' sg = (parseCss . unpack . toCssSelector . toSelectorGroup) sg == toSelectorGroup sg

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
