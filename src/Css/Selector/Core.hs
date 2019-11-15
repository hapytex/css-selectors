{-# LANGUAGE OverloadedStrings #-}

module Css.Selector.Core where

import Data.List.NonEmpty(NonEmpty, toList)
import Data.Function(on)
import Data.Ord(comparing)
import Data.String(IsString(fromString))
import Data.Text(Text, cons, intercalate, pack)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(Gen, frequency)

data SelectorSpecificity = SelectorSpecificity Int Int Int

instance Eq SelectorSpecificity where
    (==) = on (==) specificityValue

instance Ord SelectorSpecificity where
    compare = comparing specificityValue

specificityValue :: SelectorSpecificity -> Int
specificityValue (SelectorSpecificity a b c) = 100*a + 10*b + c

instance Semigroup SelectorSpecificity where
    SelectorSpecificity a1 b1 c1 <> SelectorSpecificity a2 b2 c2 = SelectorSpecificity (a1+a2) (b1+b2) (c1+c2)

instance Monoid SelectorSpecificity where
    mempty = SelectorSpecificity 0 0 0

-- based on https://www.w3.org/TR/2018/REC-selectors-3-20181106/#w3cselgrammar

class ToCssSelector a where
    toCssSelector :: a -> Text
    toSelectorGroup :: a -> SelectorGroup
    specificity' :: a -> SelectorSpecificity
    specificity :: a -> Int
    specificity = specificityValue . specificity'

newtype SelectorGroup = SelectorGroup (NonEmpty Selector)

data Selector =
      SelectorSequence SelectorSequence
    | Descendant SelectorSequence Selector
    | Child SelectorSequence Selector
    | DirectlyPreceded SelectorSequence Selector
    | Preceded SelectorSequence Selector

combine :: (SelectorSequence -> Selector -> Selector) -> Selector -> Selector -> Selector
combine f x0 ys = go x0
    where go (SelectorSequence x) = f x ys
          go (Descendant s1 s2) = Descendant s1 (go s2)
          go (Child s1 s2) = Child s1 (go s2)
          go (DirectlyPreceded s1 s2) = DirectlyPreceded s1 (go s2)
          go (Preceded s1 s2) = Preceded s1 (go s2)

instance Semigroup Selector where
    (<>) = combine Descendant


data SelectorSequence =
      SimpleSelector TypeSelector
    | Filter SelectorSequence SelectorFilter

addFilters :: SelectorSequence -> [SelectorFilter] -> SelectorSequence
addFilters = foldl Filter


data SelectorFilter =
      SHash Hash
    | SClass Class
    | SAttrib Attrib
    | SPseudo Pseudo
    | SNeg Negation


data Attrib = Exist AttributeName | Attrib AttributeName AttributeCombinator Text

attrib :: AttributeCombinator -> AttributeName -> Text -> Attrib
attrib = flip Attrib

(.=) :: AttributeName -> Text -> Attrib
(.=) = attrib Exact

(.~=) :: AttributeName -> Text -> Attrib
(.~=) = attrib Include

(.|=) :: AttributeName -> Text -> Attrib
(.|=) = attrib DashMatch

(.^=) :: AttributeName -> Text -> Attrib
(.^=) = attrib PrefixMatch

(.$=) :: AttributeName -> Text -> Attrib
(.$=) = attrib SuffixMatch

(.*=) :: AttributeName -> Text -> Attrib
(.*=) = attrib SubstringMatch

data Pseudo = Pseudo
data Negation = Negation

instance Semigroup SelectorGroup where
    (SelectorGroup g1) <> (SelectorGroup g2) = SelectorGroup (g1 <> g2)

data Namespace = NAny | NEmpty | Namespace Text
data ElementName = EAny | ElementName Text
data TypeSelector = TypeSelector { selectorNameSpace :: Namespace, elementName :: ElementName }
data AttributeName = AttributeName { attributeNamespace :: Namespace, attributeName :: Text }
data AttributeCombinator = Exact | Include | DashMatch | PrefixMatch | SuffixMatch | SubstringMatch
newtype Class = Class { unClass :: Text }
newtype Hash = Hash { unHash :: Text }

attributeCombinatorText :: AttributeCombinator -> Text
attributeCombinatorText Exact = "="
attributeCombinatorText Include = "~="
attributeCombinatorText DashMatch = "|="
attributeCombinatorText PrefixMatch = "^="
attributeCombinatorText SuffixMatch = "$="
attributeCombinatorText SubstringMatch = "*="

universal :: TypeSelector
universal = TypeSelector NAny EAny

instance IsString Class where
    fromString ('.' : s) = Class (pack s)
    fromString s = Class (pack s)

instance ToCssSelector SelectorGroup where
    toCssSelector (SelectorGroup g) = intercalate " , " (map toCssSelector (toList g))
    toSelectorGroup = id
    specificity' (SelectorGroup g) = foldMap specificity' g

instance ToCssSelector Class where
    toCssSelector = cons '.' . unClass
    toSelectorGroup = toSelectorGroup . SClass
    specificity' = const (SelectorSpecificity 0 1 0)

instance ToCssSelector Attrib where
    toCssSelector (Exist name) = "[" <> toCssSelector name <> "]"
    toCssSelector (Attrib name op val) = "[" <> toCssSelector name <> attributeCombinatorText op <> val <> "]"
    toSelectorGroup = toSelectorGroup . SAttrib
    specificity' = const (SelectorSpecificity 0 1 0)

instance ToCssSelector AttributeName where
    toCssSelector (AttributeName NAny e) = e
    toCssSelector (AttributeName n e) = toCssSelector n <> "|" <> e
    toSelectorGroup = toSelectorGroup . Exist
    specificity' = mempty

instance IsString Hash where
    fromString ('#' : s) = Hash (pack s)
    fromString s = Hash (pack s)

instance IsString Namespace where
    fromString "*" = NAny
    fromString "" = NEmpty
    fromString s = Namespace (pack s)

instance IsString ElementName where
    fromString "*" = EAny
    fromString s = ElementName (pack s)

instance ToCssSelector Hash where
    toCssSelector = cons '#' . unHash
    toSelectorGroup = toSelectorGroup . SHash
    specificity' = const (SelectorSpecificity 1 0 0)

instance ToCssSelector Namespace where
    toCssSelector NAny = "*"
    toCssSelector NEmpty = ""
    toCssSelector (Namespace t) = t
    toSelectorGroup = toSelectorGroup . flip TypeSelector EAny
    specificity' = mempty

instance ToCssSelector SelectorSequence where
    toCssSelector (SimpleSelector s) = toCssSelector s
    toCssSelector (Filter s f) = toCssSelector s <> toCssSelector f
    toSelectorGroup = toSelectorGroup . SelectorSequence
    specificity' (SimpleSelector s) = specificity' s
    specificity' (Filter s f) = specificity' s <> specificity' f

instance ToCssSelector TypeSelector where
    toCssSelector (TypeSelector NAny e) = toCssSelector e
    toCssSelector (TypeSelector n e) = toCssSelector n <> "|" <> toCssSelector e
    toSelectorGroup = toSelectorGroup . SimpleSelector
    specificity' (TypeSelector _ e) = specificity' e

instance ToCssSelector ElementName where
    toCssSelector EAny = "*"
    toCssSelector (ElementName e) = e
    toSelectorGroup = toSelectorGroup . TypeSelector NAny
    specificity' EAny = mempty
    specificity' (ElementName _) = SelectorSpecificity 0 0 1

instance ToCssSelector SelectorFilter where
    toCssSelector (SHash h) = toCssSelector h
    toCssSelector (SClass c) = toCssSelector c
    toCssSelector (SAttrib a) = toCssSelector a
    toCssSelector (SPseudo p) = toCssSelector p
    toCssSelector (SNeg n) = toCssSelector n
    toSelectorGroup = toSelectorGroup . Filter (SimpleSelector universal)
    specificity' (SHash h) = specificity' h
    specificity' (SClass c) = specificity' c
    specificity' (SAttrib a) = specificity' a
    specificity' (SPseudo p) = specificity' p
    specificity' (SNeg n) = specificity' n

instance ToCssSelector Pseudo where

instance ToCssSelector Negation where


_selectorCombine :: Text -> SelectorSequence -> Selector -> Text
_selectorCombine sp sa sb =  toCssSelector sa <> sp <> toCssSelector sb

_specCombine :: SelectorSequence -> Selector -> SelectorSpecificity
_specCombine s1 s2 = specificity' s1 <> specificity' s2

instance ToCssSelector Selector where
    toCssSelector (SelectorSequence s) = toCssSelector s
    toCssSelector (Descendant s1 s2) = _selectorCombine " " s1 s2
    toCssSelector (Child s1 s2) = _selectorCombine " > " s1 s2
    toCssSelector (DirectlyPreceded s1 s2) = _selectorCombine " + " s1 s2
    toCssSelector (Preceded s1 s2) = _selectorCombine " ~ " s1 s2
    toSelectorGroup = toSelectorGroup . SelectorGroup . pure
    specificity' (SelectorSequence s) = specificity' s
    specificity' (Descendant s1 s2) = _specCombine s1 s2
    specificity' (Child s1 s2) = _specCombine s1 s2
    specificity' (DirectlyPreceded s1 s2) = _specCombine s1 s2
    specificity' (Preceded s1 s2) = _specCombine s1 s2

arbitraryText :: Gen Text
arbitraryText = return "a"

instance Arbitrary Namespace where
    arbitrary = frequency [(1, return NAny), (3, Namespace <$> arbitraryText)]
