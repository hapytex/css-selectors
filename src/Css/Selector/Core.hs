{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PatternSynonyms, TemplateHaskellQuotes, TypeFamilies #-}

{-|
Module      : Css.Selector.Core
Description : A module where we define the tree of types to represent and maniplate a css selector.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the tree of types to represent and manipulate a css selector. These data types are members of several typeclasses to make these more useful.
-}
module Css.Selector.Core where

-- based on https://www.w3.org/TR/2018/REC-selectors-3-20181106/#w3cselgrammar

import Css.Selector.Utils(encodeText)

import Data.Data(Data)
import Data.Default(Default(def))
import Data.Function(on)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Ord(comparing)
import Data.String(IsString(fromString))
import Data.Text(Text, cons, intercalate, pack)

import GHC.Exts(IsList(Item, fromList, toList))

import Language.Haskell.TH.Lib(appE, conE)
import Language.Haskell.TH.Syntax(Lift(lift), Exp, Name, Q)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)
import Test.QuickCheck.Gen(Gen, choose, elements, frequency, listOf, oneof)

import Text.Blaze(ToMarkup(toMarkup), text)
import Text.Blaze.Internal(Markup)
import Text.Julius(Javascript, ToJavascript(toJavascript))

-- | A datastructure that specifies the selectivity of a css selector. The
-- specificity is calculated based on three integers: @a@, @b@ and @c@.
--
-- The specificity is calculated with @100*a+10*b+c@ where @a@, @b@ and @c@
-- count certain elements of the css selector.
data SelectorSpecificity = SelectorSpecificity Int Int Int

-- | Calculate the specificity value of the 'SelectorSpecificity'
specificityValue :: SelectorSpecificity -- ^ The 'SelectorSpecificity' to calculate the specificity value from.
    -> Int  -- ^ The specificity level of the 'SelectorSpecificity'. If the value is higher, the rules in the css-selector take precedence.
specificityValue (SelectorSpecificity a b c) = 100*a + 10*b + c

-- | A class that defines that the given type can be converted to a css-selector
-- value, and has a certain specificity.
class ToCssSelector a where
    -- | Convert the given element to a 'Text' object that contains the css
    -- selector.
    toCssSelector :: a -> Text
    -- | Lift the given 'ToCssSelector' type object to a 'SelectorGroup', which
    -- is the "root type" of the css selector hierarchy.
    toSelectorGroup :: a -> SelectorGroup
    -- | Calculate the specificity of the css selector by returing a
    -- 'SelectorSpecificity' object.
    specificity' :: a -> SelectorSpecificity

-- | Calculate the specificity of a 'ToCssSelector' type object. This is done by
-- calculating the 'SelectorSpecificity' object, and then calculating the value
-- of that object.
specificity :: ToCssSelector a => a -- ^ The object for which we evaluate the specificity.
    -> Int -- ^ The specificity level as an 'Int' value.
specificity = specificityValue . specificity'

-- | The root type of a css-selector. This is a comma-separated list of
-- selectors.
newtype SelectorGroup = SelectorGroup { unSelectorGroup :: NonEmpty Selector } deriving (Data, Eq, Show)

-- The type of a single selector. This is a sequence of 'SelectorSequence's that
-- are combined with a 'SelectorCombinator'.
data Selector =
      SelectorSequence SelectorSequence
    | Combined SelectorSequence SelectorCombinator Selector
    deriving (Data, Eq, Show)


-- A type that contains the possible ways to combine 'SelectorSequence's.
data SelectorCombinator =
      Descendant -- ^ The second tag is a descendant of the first one, denoted in css with a space.
    | Child -- ^ The second tag is the (direct) child of the first one, denoted with a @>@ in css.
    | DirectlyPreceded -- ^ The second tag is directly preceded by the first one, denoted with a @+@ in css.
    | Preceded -- ^ The second tag is preceded by the first one, denoted with a @~@ in css.
    deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)

-- | Convert the 'SelectorCombinator' to the equivalent css-selector text. A
-- space for 'Descendant', a @>@ for 'Child', a @+@ for 'DirectlyPreceded', and
-- a @~@ for 'Preceded'
combinatorText :: SelectorCombinator -> Text
combinatorText Descendant = " "
combinatorText Child = " > "
combinatorText DirectlyPreceded = " + "
combinatorText Preceded = " ~ "

-- | Combines two 'Selector's with the given 'SelectorCombinator'.
combine :: SelectorCombinator -> Selector -> Selector -> Selector
combine c0 x0 ys = go x0
    where go (SelectorSequence x) = Combined x c0 ys
          go (Combined s1 c s2) = Combined s1 c (go s2)

data SelectorSequence =
      SimpleSelector TypeSelector
    | Filter SelectorSequence SelectorFilter
    deriving (Data, Eq, Show)

-- | Add a given list of 'SelectorFilter's to the given 'SelectorSequence'. The
-- filters are applied left-to-right.
addFilters :: SelectorSequence -> [SelectorFilter] -> SelectorSequence
addFilters = foldl Filter

-- | A type that sums up the different ways to filter a type selector: with an
-- id (hash), a class, and an attribute.
data SelectorFilter =
      SHash Hash
    | SClass Class
    | SAttrib Attrib
    deriving (Data, Eq, Show)

-- | A css attribute can come in two flavors: either a constraint that the
-- attribute should exists, or a constraint that a certain attribute should have
-- a certain value (prefix, suffix, etc.).
data Attrib =
      Exist AttributeName -- ^ A constraint that the given 'AttributeName' should exist.
    | Attrib AttributeName AttributeCombinator Text -- A constraint about the value associated with the given 'AttributeName'.
    deriving (Data, Eq, Show)

-- | A flipped version of the 'Attrib' data constructor, where one first
-- specifies the conbinator, then the 'AttributeName' and finally the value.
attrib :: AttributeCombinator -> AttributeName -> Text -> Attrib
attrib = flip Attrib

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted to be
-- exactly the given value.
(.=) :: AttributeName -> Text -> Attrib
(.=) = attrib Exact

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted to be
--
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

-- | Construct a 'TypeSelector' with a given 'Namespace' and 'ElementName'.
(.|) :: Namespace -- ^ The 'Namespace' for the 'TypeSelector'.
    -> ElementName -- ^ The 'ElementName' for the 'TypeSelector'.
    -> TypeSelector -- ^ A 'TypeSelector' object constructed with the 'Namespace' and 'ElementName'.
(.|) = TypeSelector

data Namespace = NAny | NEmpty | Namespace Text deriving (Data, Eq, Show)
data ElementName = EAny | ElementName Text deriving (Data, Eq, Show)
data TypeSelector = TypeSelector { selectorNameSpace :: Namespace, elementName :: ElementName } deriving (Data, Eq, Show)
data AttributeName = AttributeName { attributeNamespace :: Namespace, attributeName :: Text } deriving (Data, Eq, Show)
data AttributeCombinator = Exact | Include | DashMatch | PrefixMatch | SuffixMatch | SubstringMatch deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)
newtype Class = Class { unClass :: Text } deriving (Data, Eq, Show)
newtype Hash = Hash { unHash :: Text } deriving (Data, Eq, Show)

-- | Convert the given 'AttributeCombinator' to its css-selector counterpart.
attributeCombinatorText :: AttributeCombinator -> Text
attributeCombinatorText Exact = "="
attributeCombinatorText Include = "~="
attributeCombinatorText DashMatch = "|="
attributeCombinatorText PrefixMatch = "^="
attributeCombinatorText SuffixMatch = "$="
attributeCombinatorText SubstringMatch = "*="

-- | The universal type selector: a selector that matches all types in all
--   namespaces (including the empty namespace)
pattern Universal :: TypeSelector
pattern Universal = TypeSelector NAny EAny

-- Semigroup and Monoid instances
instance Semigroup SelectorSpecificity where
    SelectorSpecificity a1 b1 c1 <> SelectorSpecificity a2 b2 c2 = SelectorSpecificity (a1+a2) (b1+b2) (c1+c2)

instance Semigroup SelectorGroup where
    SelectorGroup g1 <> SelectorGroup g2 = SelectorGroup (g1 <> g2)

instance Semigroup Selector where
    (<>) = combine def

instance Monoid SelectorSpecificity where
    mempty = SelectorSpecificity 0 0 0

-- IsString instances
instance IsString Class where
    fromString ('.' : s) = Class (pack s)
    fromString s = Class (pack s)

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

-- IsList instances
instance IsList SelectorGroup where
    type Item SelectorGroup = Selector
    fromList = SelectorGroup . fromList
    toList (SelectorGroup ss) = toList ss

-- ToCssSelector instances
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
    toCssSelector (Attrib name op val) = "[" <> toCssSelector name <> attributeCombinatorText op <> encodeText '"' val <> "]"
    toSelectorGroup = toSelectorGroup . SAttrib
    specificity' = const (SelectorSpecificity 0 1 0)

instance ToCssSelector AttributeName where
    toCssSelector (AttributeName NAny e) = e
    toCssSelector (AttributeName n e) = toCssSelector n <> "|" <> e
    toSelectorGroup = toSelectorGroup . Exist
    specificity' = mempty


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
    toSelectorGroup = toSelectorGroup . Filter (SimpleSelector Universal)
    specificity' (SHash h) = specificity' h
    specificity' (SClass c) = specificity' c
    specificity' (SAttrib a) = specificity' a

instance ToCssSelector Selector where
    toCssSelector (SelectorSequence s) = toCssSelector s
    toCssSelector (Combined s1 c s2) = toCssSelector s1 <> combinatorText c <> toCssSelector s2
    toSelectorGroup = toSelectorGroup . SelectorGroup . pure
    specificity' (SelectorSequence s) = specificity' s
    specificity' (Combined s1 _ s2) = specificity' s1 <> specificity' s2

-- Custom Eq and Ord instances
instance Eq SelectorSpecificity where
    (==) = on (==) specificityValue

instance Ord SelectorSpecificity where
    compare = comparing specificityValue

-- Default instances
instance Default SelectorGroup where
    def = SelectorGroup (pure def)

instance Default Selector where
    def = SelectorSequence def

instance Default SelectorSequence where
    def = SimpleSelector def

instance Default TypeSelector where
    def = Universal

instance Default SelectorSpecificity where
    def = mempty

instance Default Namespace where
    def = NAny

instance Default ElementName where
    def = EAny

instance Default SelectorCombinator where
    def = Descendant

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

-- ToJavaScript instances
_cssToJavascript :: ToCssSelector a => a -> Javascript
_cssToJavascript = toJavascript . toCssSelector

instance ToJavascript SelectorGroup where
    toJavascript = _cssToJavascript

instance ToJavascript Selector where
    toJavascript = _cssToJavascript

instance ToJavascript SelectorSequence where
    toJavascript = _cssToJavascript

instance ToJavascript SelectorFilter where
    toJavascript = _cssToJavascript

instance ToJavascript Attrib where
    toJavascript = _cssToJavascript


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
    arbitrary = frequency [(3, SelectorSequence <$> arbitrary), (1, Combined <$> arbitrary <*> arbitrary <*> arbitrary) ]
