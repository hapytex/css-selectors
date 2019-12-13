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

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)
import Test.QuickCheck.Gen(Gen, choose, elements, frequency, listOf, oneof)

import Text.Blaze(ToMarkup(toMarkup), text)
import Text.Blaze.Internal(Markup)

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
    deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)

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
data AttributeCombinator = Exact | Include | DashMatch | PrefixMatch | SuffixMatch | SubstringMatch deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)
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
_cssToMarkup :: ToCssSelector a => a -> Markup
_cssToMarkup = text . toCssSelector

instance ToMarkup SelectorGroup where
    toMarkup = _cssToMarkup

instance ToMarkup Selector where
    toMarkup = _cssToMarkup

instance ToMarkup SelectorSequence where
    toMarkup = _cssToMarkup

instance ToMarkup SelectorFilter where
    toMarkup = _cssToMarkup

instance ToMarkup Attrib where
    toMarkup = _cssToMarkup

-- Arbitrary instances
type FreqGen a = (Int, Gen a)

_azGen :: FreqGen Char
_azGen = (52, oneof [choose ('a', 'z'), choose ('A', 'Z')])

_digitGen :: FreqGen Char
_digitGen = (10, choose ('0', '9'))

_symbolGen :: FreqGen Char
_symbolGen = (2, elements "-_")

_arbitraryIdent0 :: Gen Char
_arbitraryIdent0 = snd _azGen

_arbitraryIdentN :: Gen Char
_arbitraryIdentN = frequency [_azGen, _digitGen, _symbolGen]

_arbitraryIdent :: Gen Text
_arbitraryIdent = do
    ident0 <- _arbitraryIdent0
    identn <- listOf _arbitraryIdentN
    postpr <- frequency [(1, return (cons '-')), (3, return id)]
    return (postpr (cons ident0 (pack identn)))

instance Arbitrary Hash where
    arbitrary = Hash <$> _arbitraryIdent

instance Arbitrary Class where
    arbitrary = Class <$> _arbitraryIdent

instance Arbitrary Namespace where
    arbitrary = frequency [(3, return NAny), (1, Namespace <$> _arbitraryIdent)]

instance Arbitrary ElementName where
    arbitrary = frequency [(1, return EAny), (3, ElementName <$> _arbitraryIdent)]

instance Arbitrary TypeSelector where
    arbitrary = TypeSelector <$> arbitrary <*> arbitrary

instance Arbitrary SelectorSequence where
    arbitrary = addFilters . SimpleSelector <$> arbitrary <*> listOf arbitrary

instance Arbitrary SelectorCombinator where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary AttributeCombinator where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SelectorFilter where
    arbitrary = oneof [SHash <$> arbitrary, SClass <$> arbitrary, SAttrib <$> arbitrary]

instance Arbitrary AttributeName where
    arbitrary = AttributeName <$> arbitrary <*> _arbitraryIdent

instance Arbitrary Attrib where
    arbitrary = oneof [Exist <$> arbitrary, Attrib <$> arbitrary <*> arbitrary <*> (pack <$> listOf arbitrary)]

instance Arbitrary SelectorGroup where
    arbitrary = SelectorGroup <$> ((:|) <$> arbitrary <*> arbitrary)

instance Arbitrary Selector where
    arbitrary = SelectorSequence <$> arbitrary
