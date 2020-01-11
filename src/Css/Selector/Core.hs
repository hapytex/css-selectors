{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PatternSynonyms, TemplateHaskellQuotes, TypeFamilies #-}

{-|
Module      : Css.Selector.Core
Description : A module where we define the tree of types to represent and maniplate a css selector.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the tree of types to represent and manipulate a css selector. These data types are members of several typeclasses to make these more useful.
-}
module Css.Selector.Core (
    -- * ToCssSelector typeclass
    ToCssSelector(..)
    -- * Selectors and combinators
    , Selector(..)
    , SelectorCombinator(..), SelectorFilter(..), SelectorGroup(..)
    , SelectorSequence(..)
    , filters, addFilters, combinatorText, combine
    -- * Namespaces
    , Namespace(..), pattern NEmpty
    -- * Type selectors
    , ElementName(..), TypeSelector(..), pattern Universal, (.|)
    -- * Attributes
    , Attrib(..), AttributeCombinator(..), AttributeName(..), AttributeValue
    , (.=), (.~=), (.|=), (.^=), (.$=), (.*=)
    , attrib, attributeCombinatorText
    -- * Classes
    , Class(..), (...)
    -- * Hashes
    , Hash(..), (.#)
    -- * Specificity
    , SelectorSpecificity(..), specificity, specificityValue
  ) where

-- based on https://www.w3.org/TR/2018/REC-selectors-3-20181106/#w3cselgrammar

import Css.Selector.Utils(encodeText, toIdentifier)

import Data.Aeson(Value(String), ToJSON(toJSON))
import Data.Data(Data)
import Data.Default(Default(def))
import Data.Function(on)
import Data.List(unfoldr)
import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.List.NonEmpty
import Data.Ord(comparing)
import Data.String(IsString(fromString))
import Data.Text(Text, cons, intercalate, pack, unpack)

import GHC.Exts(IsList(Item, fromList, toList))

import Language.Haskell.TH.Lib(appE, conE)
import Language.Haskell.TH.Syntax(Lift(lift), Exp(AppE, ConE, LitE), Lit(StringL), Name, Pat(ConP, ListP, ViewP), Q)

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
data SelectorSpecificity =
      SelectorSpecificity Int Int Int -- ^ Create a 'SelectorSpecificity' object with a given value for @a@, @b@, and @c@.
    deriving (Data, Show)

-- | Calculate the specificity value of the 'SelectorSpecificity'
specificityValue :: SelectorSpecificity -- ^ The 'SelectorSpecificity' to calculate the specificity value from.
    -> Int  -- ^ The specificity level of the 'SelectorSpecificity'. If the value is higher, the rules in the css-selector take precedence.
specificityValue (SelectorSpecificity a b c) = 100*a + 10*b + c

-- | A class that defines that the given type can be converted to a css-selector
-- value, and has a certain specificity.
class ToCssSelector a where
    -- | Convert the given element to a 'Text' object that contains the css
    -- selector.
    toCssSelector :: a -- ^ The given object for which we calculate the css selector.
        -> Text -- ^ The css selector text for the given object.

    -- | Lift the given 'ToCssSelector' type object to a 'SelectorGroup', which
    -- is the "root type" of the css selector hierarchy.
    toSelectorGroup :: a -- ^ The item to lift to a 'SelectorGroup'
        -> SelectorGroup -- ^ The value of a 'SelectorGroup' of which the object is the selective part.

    -- | Calculate the specificity of the css selector by returing a
    -- 'SelectorSpecificity' object.
    specificity' :: a -- ^ The item for which we calculate the specificity level.
        -> SelectorSpecificity -- ^ The specificity level of the given item.
    -- Convert the given 'ToCssSelector' item to a 'Pat' pattern, such that we
    -- can use it in functions.
    toPattern :: a -- ^ The item to convert to a 'Pat'.
        -> Pat -- ^ The pattern that is generated that will match only items equal to the given object.
    -- Convert the given 'ToCssSelector' item to an item in a more normalized
    -- form. A normalization is /idempotent/: applying this multiple times will
    -- have the same effect as applying it once.
    normalize :: a -- ^ The item to normalize.
        -> a -- ^ A normalized variant of the given item. This will filter the same objects, and have the same specificity.
    normalize = id

-- | Calculate the specificity of a 'ToCssSelector' type object. This is done by
-- calculating the 'SelectorSpecificity' object, and then calculating the value
-- of that object.
specificity :: ToCssSelector a => a -- ^ The object for which we evaluate the specificity.
    -> Int -- ^ The specificity level as an 'Int' value.
specificity = specificityValue . specificity'

-- | The root type of a css-selector. This is a comma-separated list of
-- selectors.
newtype SelectorGroup = SelectorGroup {
    unSelectorGroup :: NonEmpty Selector -- ^ Unwrap the given 'NonEmpty' list of 'Selector's from the 'SelectorGroup' object.
  } deriving (Data, Eq, Ord, Show)

-- | The type of a single selector. This is a sequence of 'SelectorSequence's that
-- are combined with a 'SelectorCombinator'.
data Selector =
      Selector SelectorSequence -- ^ Convert a given 'SelectorSequence' to a 'Selector'.
    | Combined SelectorSequence SelectorCombinator Selector -- ^ Create a combined selector where we have a 'SelectorSequence' that is combined with a given 'SelectorCombinator' to a 'Selector'.
    deriving (Data, Eq, Ord, Show)


-- | A type that contains the possible ways to combine 'SelectorSequence's.
data SelectorCombinator =
      Descendant -- ^ The second tag is a descendant of the first one, denoted in css with a space.
    | Child -- ^ The second tag is the (direct) child of the first one, denoted with a @>@ in css.
    | DirectlyPreceded -- ^ The second tag is directly preceded by the first one, denoted with a @+@ in css.
    | Preceded -- ^ The second tag is preceded by the first one, denoted with a @~@ in css.
    deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)

-- | Convert the 'SelectorCombinator' to the equivalent css-selector text. A
-- space for 'Descendant', a @>@ for 'Child', a @+@ for 'DirectlyPreceded', and
-- a @~@ for 'Preceded'
combinatorText :: SelectorCombinator -- ^ The given 'SelectorCombinator' to retrieve the css token for.
    -> Text -- ^ The css selector token that is used for the given 'SelectorCombinator'.
combinatorText Descendant = " "
combinatorText Child = " > "
combinatorText DirectlyPreceded = " + "
combinatorText Preceded = " ~ "

-- | Combines two 'Selector's with the given 'SelectorCombinator'.
combine :: SelectorCombinator -- ^ The 'SelectorCombinator' that is applied between the two 'Selector's.
    -> Selector -- ^ The left 'Selector'.
    -> Selector -- ^ The right 'Selector'.
    -> Selector -- ^ A 'Selector' that is a combination of the left 'Selector' and the right 'Selector' with the given 'SelectorCombinator'.
combine c0 x0 ys = go x0
    where go (Selector x) = Combined x c0 ys
          go (Combined s1 c s2) = Combined s1 c (go s2)

-- | A 'SelectorSequence' is a 'TypeSelector' (that can be 'Universal') followed
-- by zero, one or more 'SelectorFilter's these filter the selector further, for
-- example with a 'Hash', a 'Class', or an 'Attrib'.
data SelectorSequence =
      SimpleSelector TypeSelector -- ^ Convert a 'TypeSelector' into a 'SimpleSelector'.
    | Filter SelectorSequence SelectorFilter -- ^ Apply an additional 'SelectorFilter' to the 'SelectorSequence'.
    deriving (Data, Eq, Ord, Show)

-- | Add a given list of 'SelectorFilter's to the given 'SelectorSequence'. The
-- filters are applied left-to-right.
addFilters :: SelectorSequence -- ^ The 'SelectorSequence' to apply the filter on.
    -> [SelectorFilter] -- ^ The list of 'SelectorFilter's to apply on the 'SelectorSequence'.
    -> SelectorSequence -- ^ A modified 'SelectorSequence' where we applied the list of 'SelectorFilter's.
addFilters = foldl Filter

-- | Obtain the list of filters that are applied in the given 'SelectorSequence'
filters :: SelectorSequence -- ^ The given 'SelectorSequence' to analyze.
    -> [SelectorFilter] -- ^ The given list of 'SelectorFilter's applied, this can be empty.
filters = reverse . unfoldr go
    where go (Filter s f) = Just (f, s)
          go (SimpleSelector _) = Nothing

-- | A type that sums up the different ways to filter a type selector: with an
-- id (hash), a class, and an attribute.
data SelectorFilter =
      SHash Hash -- ^ A 'Hash' object as filter.
    | SClass Class -- ^ A 'Class' object as filter.
    | SAttrib Attrib -- ^ An 'Attrib' object as filter.
    deriving (Data, Eq, Ord, Show)

-- | A css attribute can come in two flavors: either a constraint that the
-- attribute should exists, or a constraint that a certain attribute should have
-- a certain value (prefix, suffix, etc.).
data Attrib =
      Exist AttributeName -- ^ A constraint that the given 'AttributeName' should exist.
    | Attrib AttributeName AttributeCombinator AttributeValue -- ^ A constraint about the value associated with the given 'AttributeName'.
    deriving (Data, Eq, Ord, Show)

-- | A flipped version of the 'Attrib' data constructor, where one first
-- specifies the conbinator, then the 'AttributeName' and finally the value.
attrib :: AttributeCombinator -- ^ The 'AttributeCombinator' that specifies the required relation between the attribute and a value.
    -> AttributeName -- ^ The name of an attribute to filter.
    -> AttributeValue -- ^ The value of the attribute to filter.
    -> Attrib -- ^ The result is an 'Attrib' object that will filter the given 'AttributeName' with the given 'AttributeCombinator'.
attrib = flip Attrib

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted to be
-- exactly the given value.
(.=) :: AttributeName -- ^ The name of the attribute to constraint.
    -> AttributeValue -- ^ The value that constraints the attribute.
    -> Attrib -- ^ The 'Attrib' object we construct with the given name and value.
(.=) = attrib Exact

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted such
-- that the attribute is a whitespace seperated list of items, and the value is
-- one of these items.
(.~=) :: AttributeName -- ^ The name of the attribute to constraint.
    -> AttributeValue -- ^ The value that constraints the attribute.
    -> Attrib -- ^ The 'Attrib' object we construct with the given name and value.
(.~=) = attrib Include

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted such
-- that the attribute is a dash seperated list of items, and the value is
-- the first of these items.
(.|=) :: AttributeName -- ^ The name of the attribute to constraint.
    -> AttributeValue -- ^ The value that constraints the attribute.
    -> Attrib -- ^ The 'Attrib' object we construct with the given name and value.
(.|=) = attrib DashMatch

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted such
-- that the attribute has as prefix the given 'AttributeValue'.
(.^=) :: AttributeName -- ^ The name of the attribute to constraint.
    -> AttributeValue -- ^ The value that constraints the attribute.
    -> Attrib -- ^ The 'Attrib' object we construct with the given name and value.
(.^=) = attrib PrefixMatch

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted such
-- that the attribute has as suffix the given 'AttributeValue'.
(.$=) :: AttributeName -- ^ The name of the attribute to constraint.
    -> AttributeValue -- ^ The value that constraints the attribute.
    -> Attrib -- ^ The 'Attrib' object we construct with the given name and value.
(.$=) = attrib SuffixMatch

-- | Create an 'Attrib' where the given 'AttributeName' is constrainted such
-- that the attribute has as substring the given 'AttributeValue'.
(.*=) :: AttributeName -- ^ The name of the attribute to constraint.
    -> AttributeValue -- ^ The value that constraints the attribute.
    -> Attrib -- ^ The 'Attrib' object we construct with the given name and value.
(.*=) = attrib SubstringMatch

-- | Filter a given 'SelectorSequence' with a given 'Hash'.
(.#) :: SelectorSequence -- ^ The given 'SelectorSequence' to filter.
    -> Hash -- ^ The given 'Hash' to filter the 'SelectorSequence' further.
    -> SelectorSequence -- ^ A 'SelectorSequence' that is filtered additionally with the given 'Hash'.
(.#) = (. SHash) . Filter

-- | Filter a given 'SelectorSequence' with a given 'Class'.
(...) :: SelectorSequence -- ^ The given 'SelectorSequence to filter.
    -> Class -- ^ The given 'Class' to filter the 'SelectorSequence' further.
    -> SelectorSequence -- ^ A 'SelectorSequence' that is filtered additionally with the given 'Class'.
(...) = (. SClass) . Filter

-- | Construct a 'TypeSelector' with a given 'Namespace' and 'ElementName'.
(.|) :: Namespace -- ^ The 'Namespace' for the 'TypeSelector'.
    -> ElementName -- ^ The 'ElementName' for the 'TypeSelector'.
    -> TypeSelector -- ^ A 'TypeSelector' object constructed with the 'Namespace' and 'ElementName'.
(.|) = TypeSelector

-- | The namespace of a css selector tag. The namespace can be 'NAny' (all
-- possible namespaces), or a namespace with a given text (this text can be
-- empty).
data Namespace =
      NAny -- ^ A typeselector part that specifies that we accept all namespaces, in css denoted with @*@.
    | Namespace Text -- ^ A typselector part that specifies that we accept a certain namespace name.
    deriving (Data, Eq, Ord, Show)

-- | The empty namespace. This is /not/ the wildcard namespace (@*@). This is a
-- bidirectional namespace and can thus be used in expressions as well.
pattern NEmpty :: Namespace
pattern NEmpty = Namespace ""

-- | The element name of a css selector tag. The element name can be 'EAny' (all
-- possible tag names), or an element name with a given text.
data ElementName =
      EAny -- ^ A typeselector part that specifies that we accept all element names, in css denoted with @*@.
    | ElementName Text -- ^ A typeselector part that specifies that we accept a certain element name.
    deriving (Data, Eq, Ord, Show)

-- | A typeselector is a combination of a selector for a namespace, and a
-- selector for an element name. One, or both can be a wildcard.
data TypeSelector = TypeSelector {
    selectorNamespace :: Namespace, -- ^ The selector for the namespace.
    elementName :: ElementName -- ^ The selector for the element name.
  } deriving (Data, Eq, Ord, Show)

-- | An attribute name is a name that optionally has a namespace, and the name
-- of the attribute.
data AttributeName = AttributeName {
    attributeNamespace :: Namespace, -- ^ The namespace to which the attribute name belongs. This can be 'NAny' as well.
    attributeName :: Text  -- ^ The name of the attribute over which we make a claim.
  } deriving (Data, Eq, Ord, Show)

-- | We use 'Text' as the type to store an attribute value.
type AttributeValue = Text

-- | The possible ways to match an attribute with a given value in a css
-- selector.
data AttributeCombinator =
      Exact -- ^ The attribute has exactly the value of the value, denoted with @=@ in css.
    | Include -- ^ The attribute has a whitespace separated list of items, one of these items is the value, denoted with @~=@ in css.
    | DashMatch -- ^ The attribute has a hyphen separated list of items, the first item is the value, denoted with @|=@ in css.
    | PrefixMatch -- ^ The value is a prefix of the value in the attribute, denoted with @^=@ in css.
    | SuffixMatch -- ^ The value is a suffix of the value in the attribute, denoted with @$=@ in css.
    | SubstringMatch -- ^The value is a substring of the value in the attribute, denoted with @*=@ in css.
    deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)

-- | A css class, this is wrapped in a data type. The type only wraps the class
-- name, not the dot prefix.
newtype Class = Class {
    unClass :: Text -- ^ Obtain the name from the class.
  } deriving (Data, Eq, Ord, Show)

-- | A css hash (used to match an element with a given id). The type only wraps
-- the hash name, not the hash (@#@) prefix.
newtype Hash = Hash {
    unHash :: Text -- ^ Obtain the name from the hash.
  } deriving (Data, Eq, Ord, Show)

-- | Convert the given 'AttributeCombinator' to its css-selector counterpart.
attributeCombinatorText :: AttributeCombinator -- ^ The 'AttributeCombinator' for which we obtain the corresponding css selector text.
    -> AttributeValue -- ^ The css selector text for the given 'AttributeCombinator'.
attributeCombinatorText Exact = "="
attributeCombinatorText Include = "~="
attributeCombinatorText DashMatch = "|="
attributeCombinatorText PrefixMatch = "^="
attributeCombinatorText SuffixMatch = "$="
attributeCombinatorText SubstringMatch = "*="

-- | The universal type selector: a selector that matches all types in all
--   namespaces (including the empty namespace). This pattern is bidirectional
--   and thus can be used in expressions as well.
pattern Universal :: TypeSelector
pattern Universal = TypeSelector NAny EAny

-- Semigroup and Monoid instances
instance Semigroup SelectorSpecificity where
    SelectorSpecificity a1 b1 c1 <> SelectorSpecificity a2 b2 c2 = SelectorSpecificity (a1+a2) (b1+b2) (c1+c2)

instance Semigroup SelectorGroup where
    SelectorGroup g1 <> SelectorGroup g2 = SelectorGroup (g1 <> g2)

instance Semigroup Selector where
    (<>) = combine def

instance Semigroup Namespace where
    (<>) NAny = id
    (<>) x = const x

instance Semigroup ElementName where
    (<>) EAny = id
    (<>) x = const x

instance Monoid SelectorSpecificity where
    mempty = SelectorSpecificity 0 0 0

instance Monoid Namespace where
    mempty = NAny

instance Monoid ElementName where
    mempty = EAny

-- IsString instances
instance IsString Class where
    fromString ('.' : s) = toIdentifier (Class . pack) s
    fromString s = toIdentifier (Class . pack) s

instance IsString Hash where
    fromString ('#' : s) = toIdentifier (Hash . pack) s
    fromString s = toIdentifier (Hash . pack) s

instance IsString Namespace where
    fromString "*" = NAny
    fromString s = toIdentifier (Namespace . pack) s

instance IsString ElementName where
    fromString "*" = EAny
    fromString s = toIdentifier (ElementName . pack) s

-- IsList instances
instance IsList SelectorGroup where
    type Item SelectorGroup = Selector
    fromList = SelectorGroup . fromList
    toList (SelectorGroup ss) = toList ss

-- ToCssSelector instances
_textToPattern :: Text -> Pat
_textToPattern t = ViewP (AppE (ConE '(==)) (AppE (ConE 'pack) (LitE (StringL (unpack t))))) (_constantP 'True)

_constantP :: Name -> Pat
_constantP = flip ConP []

instance ToCssSelector SelectorGroup where
    toCssSelector (SelectorGroup g) = intercalate " , " (map toCssSelector (toList g))
    toSelectorGroup = id
    specificity' (SelectorGroup g) = foldMap specificity' g
    toPattern (SelectorGroup g) = ConP 'SelectorGroup [go g]
        where go (x :| xs) = ConP '(:|) [toPattern x, ListP (map toPattern xs)]
    normalize (SelectorGroup g) = (SelectorGroup (Data.List.NonEmpty.sort g))

instance ToCssSelector Class where
    toCssSelector = cons '.' . unClass
    toSelectorGroup = toSelectorGroup . SClass
    specificity' = const (SelectorSpecificity 0 1 0)
    toPattern (Class c) = ConP 'Class [_textToPattern c]

instance ToCssSelector Attrib where
    toCssSelector (Exist name) = "[" <> toCssSelector name <> "]"
    toCssSelector (Attrib name op val) = "[" <> toCssSelector name <> attributeCombinatorText op <> encodeText '"' val <> "]"
    toSelectorGroup = toSelectorGroup . SAttrib
    specificity' = const (SelectorSpecificity 0 1 0)
    toPattern (Exist name) = ConP 'Exist [toPattern name]
    toPattern (Attrib name op val) = ConP 'Attrib [toPattern name, _constantP (go op), _textToPattern val]
        where go Exact = 'Exact
              go Include = 'Include
              go DashMatch = 'DashMatch
              go PrefixMatch = 'PrefixMatch
              go SuffixMatch = 'SuffixMatch
              go SubstringMatch = 'SubstringMatch

instance ToCssSelector AttributeName where
    toCssSelector (AttributeName NAny e) = e
    toCssSelector (AttributeName n e) = toCssSelector n <> "|" <> e
    toSelectorGroup = toSelectorGroup . Exist
    specificity' = mempty
    toPattern (AttributeName n a) = ConP 'AttributeName [toPattern n, _textToPattern a]

instance ToCssSelector Hash where
    toCssSelector = cons '#' . unHash
    toSelectorGroup = toSelectorGroup . SHash
    specificity' = const (SelectorSpecificity 1 0 0)
    toPattern (Hash h) = ConP 'Hash [_textToPattern h]

instance ToCssSelector Namespace where
    toCssSelector NAny = "*"
    toCssSelector (Namespace t) = t
    toSelectorGroup = toSelectorGroup . flip TypeSelector EAny
    specificity' = mempty
    toPattern NAny = _constantP 'NAny
    -- used to make patterns more readable
    toPattern NEmpty = _constantP 'NEmpty
    toPattern (Namespace t) = ConP 'Namespace [_textToPattern t]

instance ToCssSelector SelectorSequence where
    toCssSelector (SimpleSelector s) = toCssSelector s
    toCssSelector (Filter s f) = toCssSelector s <> toCssSelector f
    toSelectorGroup = toSelectorGroup . Selector
    specificity' (SimpleSelector s) = specificity' s
    specificity' (Filter s f) = specificity' s <> specificity' f
    toPattern (SimpleSelector s) = ConP 'SimpleSelector [toPattern s]
    toPattern (Filter s f) = ConP 'Filter [toPattern s, toPattern f]

instance ToCssSelector TypeSelector where
    toCssSelector (TypeSelector NAny e) = toCssSelector e
    toCssSelector (TypeSelector n e) = toCssSelector n <> "|" <> toCssSelector e
    toSelectorGroup = toSelectorGroup . SimpleSelector
    specificity' (TypeSelector _ e) = specificity' e
    -- we use Universal, to make the generated pattern more convenient to read.
    toPattern Universal = _constantP 'Universal
    toPattern (TypeSelector n t) = ConP 'TypeSelector [toPattern n, toPattern t]

instance ToCssSelector ElementName where
    toCssSelector EAny = "*"
    toCssSelector (ElementName e) = e
    toSelectorGroup = toSelectorGroup . TypeSelector NAny
    specificity' EAny = mempty
    specificity' (ElementName _) = SelectorSpecificity 0 0 1
    toPattern EAny = _constantP 'EAny
    toPattern (ElementName e) = ConP 'ElementName [_textToPattern e]

instance ToCssSelector SelectorFilter where
    toCssSelector (SHash h) = toCssSelector h
    toCssSelector (SClass c) = toCssSelector c
    toCssSelector (SAttrib a) = toCssSelector a
    toSelectorGroup = toSelectorGroup . Filter (SimpleSelector Universal)
    specificity' (SHash h) = specificity' h
    specificity' (SClass c) = specificity' c
    specificity' (SAttrib a) = specificity' a
    toPattern (SHash h) = ConP 'SHash [toPattern h]
    toPattern (SClass c) = ConP 'SClass [toPattern c]
    toPattern (SAttrib a) = ConP 'SAttrib [toPattern a]

instance ToCssSelector Selector where
    toCssSelector (Selector s) = toCssSelector s
    toCssSelector (Combined s1 c s2) = toCssSelector s1 <> combinatorText c <> toCssSelector s2
    toSelectorGroup = toSelectorGroup . SelectorGroup . pure
    specificity' (Selector s) = specificity' s
    specificity' (Combined s1 _ s2) = specificity' s1 <> specificity' s2
    toPattern (Selector s) = ConP 'Selector [toPattern s]
    toPattern (Combined s1 c s2) = ConP 'Combined [toPattern s1, _constantP (go c), toPattern s2]
        where go Descendant = 'Descendant
              go Child = 'Child
              go DirectlyPreceded = 'DirectlyPreceded
              go Preceded = 'Preceded

-- Custom Eq and Ord instances
instance Eq SelectorSpecificity where
    (==) = on (==) specificityValue

instance Ord SelectorSpecificity where
    compare = comparing specificityValue

-- Default instances
instance Default SelectorGroup where
    def = SelectorGroup (pure def)

instance Default Selector where
    def = Selector def

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

instance Default AttributeCombinator where
    def = Exact

-- Lift instances
_apply :: Name -> [Q Exp] -> Q Exp
_apply = foldl appE . conE

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

-- ToJavaScript and ToJson instances
_cssToJavascript :: ToCssSelector a => a -> Javascript
_cssToJavascript = toJavascript . toCssSelector

_cssToJson :: ToCssSelector a => a -> Value
_cssToJson = String . toCssSelector

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

instance ToJSON SelectorGroup where
    toJSON = _cssToJson

instance ToJSON Selector where
    toJSON = _cssToJson

instance ToJSON SelectorSequence where
    toJSON = _cssToJson

instance ToJSON SelectorFilter where
    toJSON = _cssToJson

instance ToJSON Attrib where
    toJSON = _cssToJson


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
    arbitrary = frequency [(3, Selector <$> arbitrary), (1, Combined <$> arbitrary <*> arbitrary <*> arbitrary) ]
