{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TemplateHaskellQuotes #-}

module Css.Selector.Core where

import Data.Data(Data)
import Data.Function(on)
import Data.List.NonEmpty(NonEmpty((:|)), toList)
import Data.Ord(comparing)
import Data.String(IsString(fromString))
import Data.Text(Text, cons, intercalate, pack)

import Language.Haskell.TH.Lib(appE, conE)
import Language.Haskell.TH.Syntax(Lift(lift), Exp, Name, Q)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(Gen, frequency)

import Text.Blaze(ToMarkup(toMarkup), text)

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

specificity :: ToCssSelector a => a -> Int
specificity = specificityValue . specificity'

newtype SelectorGroup = SelectorGroup (NonEmpty Selector) deriving Data

data Selector =
      SelectorSequence SelectorSequence
    | Combined SelectorSequence SelectorCombinator Selector
    deriving Data


data SelectorCombinator =
      Descendant
    | Child
    | DirectlyPreceded
    | Preceded
    deriving Data

combinatorText :: SelectorCombinator -> Text
combinatorText Descendant = " "
combinatorText Child = " > "
combinatorText DirectlyPreceded = " + "
combinatorText Preceded = " ~ "

combine :: SelectorCombinator -> Selector -> Selector -> Selector
combine c0 x0 ys = go x0
    where go (SelectorSequence x) = Combined x c0 ys
          go (Combined s1 c s2) = Combined s1 c (go s2)

instance Semigroup Selector where
    (<>) = combine Descendant


data SelectorSequence =
      SimpleSelector TypeSelector
    | Filter SelectorSequence SelectorFilter
    deriving Data

addFilters :: SelectorSequence -> [SelectorFilter] -> SelectorSequence
addFilters = foldl Filter


data SelectorFilter =
      SHash Hash
    | SClass Class
    | SAttrib Attrib
    deriving Data

data Attrib =
      Exist AttributeName
    | Attrib AttributeName AttributeCombinator Text
    deriving Data

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

(.#) :: SelectorSequence -> Hash -> SelectorSequence
(.#) = (. SHash) . Filter

(...) :: SelectorSequence -> Class -> SelectorSequence
(...) = (. SClass) . Filter

instance Semigroup SelectorGroup where
    (SelectorGroup g1) <> (SelectorGroup g2) = SelectorGroup (g1 <> g2)

data Namespace = NAny | NEmpty | Namespace Text deriving Data
data ElementName = EAny | ElementName Text deriving Data
data TypeSelector = TypeSelector { selectorNameSpace :: Namespace, elementName :: ElementName } deriving Data
data AttributeName = AttributeName { attributeNamespace :: Namespace, attributeName :: Text } deriving Data
data AttributeCombinator = Exact | Include | DashMatch | PrefixMatch | SuffixMatch | SubstringMatch deriving Data
newtype Class = Class { unClass :: Text } deriving Data
newtype Hash = Hash { unHash :: Text } deriving Data

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
    toSelectorGroup = toSelectorGroup . Filter (SimpleSelector universal)
    specificity' (SHash h) = specificity' h
    specificity' (SClass c) = specificity' c
    specificity' (SAttrib a) = specificity' a

instance ToCssSelector Selector where
    toCssSelector (SelectorSequence s) = toCssSelector s
    toCssSelector (Combined s1 c s2) = toCssSelector s1 <> (combinatorText c) <> toCssSelector s2
    toSelectorGroup = toSelectorGroup . SelectorGroup . pure
    specificity' (SelectorSequence s) = specificity' s
    specificity' (Combined s1 _ s2) = specificity' s1 <> specificity' s2


-- Lift instances
_apply :: Name -> [Q Exp] -> Q Exp
_apply n = foldl appE (conE n)

instance Lift SelectorGroup where
    lift (SelectorGroup sg) = _apply 'SelectorGroup [liftNe sg]
        where liftNe (a :| as) = _apply '(:|) [lift a, lift as]

instance Lift Selector
instance Lift SelectorCombinator
instance Lift SelectorSequence
instance Lift SelectorFilter
instance Lift Attrib

-- ToMarkup instances
instance ToMarkup SelectorGroup where
    toMarkup = text . toCssSelector

instance ToMarkup Selector where
    toMarkup = text . toCssSelector

instance ToMarkup SelectorSequence where
    toMarkup = text . toCssSelector

instance ToMarkup SelectorFilter where
    toMarkup = text . toCssSelector

instance ToMarkup Attrib where
    toMarkup = text . toCssSelector

--- Arbitrary instances
arbitraryText :: Gen Text
arbitraryText = return "a"

instance Arbitrary Namespace where
    arbitrary = frequency [(1, return NAny), (3, Namespace <$> arbitraryText)]
