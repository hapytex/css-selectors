{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, OverloadedStrings, PatternSynonyms, TemplateHaskellQuotes, TypeFamilies #-}

{-|
Module      : Css3.Selector.Core
Description : A module where we define the tree of types to represent and maniplate a css selector.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the tree of types to represent and manipulate a css selector. These data types are members of several typeclasses to make these more useful.
-}
module Css3.Selector.Core (
    -- * ToCssSelector typeclass
    ToCssSelector(toCssSelector, toSelectorGroup, specificity', toPattern, normalize)
    -- * Selectors and combinators
    , Selector(..)
    , SelectorCombinator(..), SelectorGroup(..)
    , PseudoElement(After, Before, FirstLetter, FirstLine, Marker, Selection), PseudoSelectorSequence(SelectorSequence, (:.::)), (.::)
    , PseudoClass(
          Active, Checked, Disabled, Empty, Enabled, Focus, Hover, InRange, Invalid, Link, NthChild, NthLastChild, NthLastOfType
        , NthOfType, OnlyOfType, OnlyChild, Optional, OutOfRange, ReadOnly , ReadWrite, Required, Root, Target, Valid, Visited
        ), (.:), pattern FirstChild, pattern FirstOfType, pattern LastChild, pattern LastOfType
    , SelectorSequence(..)
    , combinatorText, combine
    , (.>), (.+), (.~)
    -- * Filters
    , SelectorFilter(SHash, SClass, SAttrib, SPseudo), filters, filters', addFilters, (.@)
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
    -- * Nth items
    , Nth(Nth, linear, constant), pattern Even, pattern Odd, nthValues, nthValues0, nthValues1, normalizeNth, nthContainsValue, intersectNth
    -- * Specificity
    , SelectorSpecificity(..), specificity, specificityValue
    -- * Read and write binary content
    , encode, decode, compressEncode, compressEncodeWith, decompressDecode
  ) where

-- based on https://www.w3.org/TR/2018/REC-selectors-3-20181106/#w3cselgrammar

import Codec.Compression.GZip(CompressParams, compress, compressWith, decompress)

import Control.Applicative(liftA2)
import Control.DeepSeq(NFData)

import Css3.Selector.Utils(encodeIdentifier, encodeText, toIdentifier)

import Data.Aeson(Value(String), ToJSON(toJSON))
import Data.Binary(Binary(put, get), Get, Put, decode, encode, getWord8, putWord8)
import Data.ByteString.Lazy(ByteString)
import Data.Char(toLower)
import Data.Data(Data)
import Data.Default(Default(def))
import Data.Function(on)
import Data.Hashable(Hashable)
import Data.List(sort, unfoldr)
import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.List.NonEmpty
import Data.Ord(comparing)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup(Semigroup((<>)))
#endif
import Data.String(IsString(fromString))
import qualified Data.Text as T
import Data.Text(Text, cons, inits, intercalate, pack, snoc, tails, unpack)

import GHC.Exts(IsList(Item, fromList, toList))
import GHC.Generics(Generic)

import Language.Haskell.TH.Lib(appE, conE)
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax(Lift(lift, liftTyped), Exp(AppE, ConE, LitE), Lit(StringL), Name, Pat(ConP, ListP, ViewP), Q, unsafeCodeCoerce)
#elif MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Syntax(Lift(lift, liftTyped), Exp(AppE, ConE, LitE), Lit(StringL), Name, Pat(ConP, ListP, ViewP), Q, unsafeTExpCoerce)
#else
import Language.Haskell.TH.Syntax(Lift(lift), Exp(AppE, ConE, LitE), Lit(IntegerL, StringL), Name, Pat(ConP, ListP, LitP, ViewP), Q)
#endif

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary, shrink), arbitraryBoundedEnum)
import Test.QuickCheck.Gen(Gen, frequency, listOf, listOf1, oneof)

import Text.Blaze(ToMarkup(toMarkup), text)
import Text.Blaze.Internal(Markup)
import Text.Julius(Javascript, ToJavascript(toJavascript))

-- | A datastructure that specifies the selectivity of a css selector. The
-- specificity is calculated based on three integers: @a@, @b@ and @c@.
--
-- The specificity is calculated with @100*a+10*b+c@ where @a@, @b@ and @c@
-- count certain elements of the css selector.
data SelectorSpecificity
    = SelectorSpecificity Int Int Int -- ^ Create a 'SelectorSpecificity' object with a given value for @a@, @b@, and @c@.
    deriving (Data, Generic, Show)

instance Hashable SelectorSpecificity

instance NFData SelectorSpecificity

-- | Calculate the specificity value of the 'SelectorSpecificity'
specificityValue :: SelectorSpecificity -- ^ The 'SelectorSpecificity' to calculate the specificity value from.
    -> Int  -- ^ The specificity level of the 'SelectorSpecificity'. If the value is higher, the rules in the css selector take precedence.
specificityValue (SelectorSpecificity a b c) = 100*a + 10*b + c

data Nth = Nth { linear :: Int, constant :: Int } deriving (Data, Eq, Generic, Ord, Read, Show)

instance Hashable Nth

instance NFData Nth

-- | Normalize the given 'Nth' object to a normalized one. If and only if the
-- normalized variants are the same of two 'Nth' objects, then these will produce
-- the same list of values. Normalization is idempotent: calling 'normalizeNth'
-- on a normalized 'Nth' will produce the same 'Nth'.
normalizeNth
  :: Nth -- ^ The given 'Nth' item to normalize.
  -> Nth -- ^ The normalized variant of the given 'Nth' object.
normalizeNth nth@(Nth n c)
  | n <= 0 && c + n <= 0 = Nth 0 (max 0 c)
  | n > 0 && c < 0 = let cn = c `mod` n in if cn /= 0 then Nth n cn else Nth n n
  | n > 0 && c == 0 = Nth n n
  | otherwise = nth

-- | Obtain the one-based indices that match the given 'Nth' object. The CSS3 selectors
-- are one-based: the first child has index 1.
nthValues
  :: Nth  -- The 'Nth' object that specifies the given range.
  -> [Int]  -- ^ A list of one-based indexes that contain the items selected by the 'Nth' object. The list can be infinite.
nthValues (Nth n c)
  | n > 0 && c <= 0 = let {c' = c `mod` n; cn' = c' + n} in (if c' /= 0 then (c':) else id) [cn', cn' + n ..]
  | n > 0 = [c, c+n ..]
  | n < 0 = [ c, c+n .. 1 ]
  | otherwise = [c | c > 0]

nthContainsValue :: Nth -> Int -> Bool
nthContainsValue (Nth 0 c) i = c == i && i > 0
nthContainsValue (Nth n c) i = i > 0 && (i - c) `div` n >= 0 && (i - c) `mod` n == 0

-- | Obtain the one-based indices that match the given 'Nth' object. The CSS3 selectors
-- are one-based: the first child has index 1. This is an alias of the 'nthValues' function.
nthValues1
  :: Nth  -- The 'Nth' object that specifies the given range.
  -> [Int]  -- ^ A list of zero-based indexes that contain the items selected by the 'Nth' object. The list can be infinite.
nthValues1 = nthValues

-- | Obtain the zero-based indices that match the given 'Nth' object. One can use this for list/vector processing since
-- the CSS3 selectors start with index 1. The 'nthValues1' can be used for one-based indexes.
nthValues0
  :: Nth  -- The 'Nth' object that specifies the given range.
  -> [Int]  -- ^ A list of zero-based indexes that contain the items selected by the 'Nth' object. The list can be infinite.
nthValues0 = map (subtract 1) . nthValues

intersectNth
  :: Nth
  -> Nth
  -> Nth
intersectNth (Nth 0 a) (Nth 0 b) = if a == b then Nth 0 a else Nth 0 (-1)
intersectNth (Nth na ca) (Nth nb cb)
  | na > 0 && nb > 0 = Nth (lcm na nb) (ca+cb)
  | otherwise = undefined

pattern Even :: Nth
pattern Even = Nth 2 0

pattern Odd :: Nth
pattern Odd = Nth 2 1

pattern One :: Nth
pattern One = Nth 0 1

nthToText :: Nth -> Text
nthToText Even = "even"
nthToText Odd = "odd"
nthToText (Nth n 0) = snoc (pack (show n)) 'n'
nthToText (Nth 0 b) = pack (show b)
nthToText (Nth n b)
  | b <= 0 = pack (show n ++ 'n' : show b)
  | otherwise = pack (show n ++ 'n' : '+' : show b)

-- | A class that defines that the given type can be converted to a css selector
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
    {-# MINIMAL toCssSelector, toSelectorGroup, specificity', toPattern #-}

-- | Convert the given item to a compressed 'ByteString'. This can be used to write to and read from a file for example.
-- The econding format is not an official format: it is constructed based on the structure of the Haskell types. That
-- stream is then passed through a gzip implementation.
compressEncode :: (Binary a, ToCssSelector a)
  => a -- ^ The object to turn into a compressed 'ByteString'.
  -> ByteString -- ^ A compressed binary representation of the given object.
compressEncode = compress . encode

-- | Convert the given item to a compressed 'ByteString'. This can be used to write to and read from a file for example.
-- The econding format is not an official format: it is constructed based on the structure of the Haskell types. That
-- stream is then passed through a gzip implementation.
compressEncodeWith :: (Binary a, ToCssSelector a)
  => CompressParams -- ^ The parameters that determine how to compress the 'ByteString'.
  -> a -- ^ The object to turn into a compressed 'ByteString'.
  -> ByteString -- ^ A compressed binary representation of the given object.
compressEncodeWith level = compressWith level . encode

-- | Convert the given item to a compressed 'ByteString'. This can be used to write to and read from a file for example.
-- The econding format is not an official format: it is constructed based on the structure of the Haskell types. That
-- stream is then passed through a gzip implementation.
decompressDecode :: (Binary a, ToCssSelector a)
  => ByteString -- ^ A compressed binary representation of a 'ToCssSelector' type.
  -> a -- ^ The corresponding decompressed and decoded logic.
decompressDecode = decode . decompress


-- | Calculate the specificity of a 'ToCssSelector' type object. This is done by
-- calculating the 'SelectorSpecificity' object, and then calculating the value
-- of that object.
specificity :: ToCssSelector a => a -- ^ The object for which we evaluate the specificity.
    -> Int -- ^ The specificity level as an 'Int' value.
specificity = specificityValue . specificity'

-- | The root type of a css selector. This is a comma-separated list of
-- selectors.
newtype SelectorGroup = SelectorGroup {
    unSelectorGroup :: NonEmpty Selector -- ^ Unwrap the given 'NonEmpty' list of 'Selector's from the 'SelectorGroup' object.
  } deriving (Data, Eq, Generic, Ord, Show)

instance Hashable SelectorGroup

instance NFData SelectorGroup

-- | The type of a single selector. This is a sequence of 'SelectorSequence's that
-- are combined with a 'SelectorCombinator'.
data Selector =
      Selector PseudoSelectorSequence -- ^ Convert a given 'SelectorSequence' to a 'Selector'.
    | Combined PseudoSelectorSequence SelectorCombinator Selector -- ^ Create a combined selector where we have a 'SelectorSequence' that is combined with a given 'SelectorCombinator' to a 'Selector'.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable Selector

instance NFData Selector

-- | A type that contains the possible ways to combine 'SelectorSequence's.
data SelectorCombinator =
      Descendant -- ^ The second tag is a descendant of the first one, denoted in css with a space.
    | Child -- ^ The second tag is the (direct) child of the first one, denoted with a @>@ in css.
    | DirectlyPreceded -- ^ The second tag is directly preceded by the first one, denoted with a @+@ in css.
    | Preceded -- ^ The second tag is preceded by the first one, denoted with a @~@ in css.
    deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable SelectorCombinator

instance NFData SelectorCombinator

-- | Convert the 'SelectorCombinator' to the equivalent css selector text. A
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

-- | Combines two 'Selector's with the 'Child' combinator.
(.>) :: Selector -- ^ The left 'Selector'.
    -> Selector -- ^ The right 'Selector'.
    -> Selector -- ^ A selector that is the combination of the left 'Selector' and the right 'Selector' through 'Child'.
(.>) = combine Child

-- | Combines two 'Selector's with the 'DirectlyPreceded' combinator.
(.+) :: Selector -- ^ The left 'Selector'.
    -> Selector -- ^ The right 'Selector'.
    -> Selector -- ^ A selector that is the combination of the left 'Selector' and the right 'Selector' through 'DirectlyPreceded'.
(.+) = combine DirectlyPreceded

-- | Combines two 'Selector's with the 'Preceded' combinator.
(.~) :: Selector -- ^ The left 'Selector'.
    -> Selector -- ^ The right 'Selector'.
    -> Selector -- ^ A selector that is the combination of the left 'Selector' and the right 'Selector' through 'Preceded'.
(.~) = combine Preceded

-- | A 'SelectorSequence' is a 'TypeSelector' (that can be 'Universal') followed
-- by zero, one or more 'SelectorFilter's these filter the selector further, for
-- example with a 'Hash', a 'Class', or an 'Attrib'.
data SelectorSequence =
      SimpleSelector TypeSelector -- ^ Convert a 'TypeSelector' into a 'SimpleSelector'.
    | Filter SelectorSequence SelectorFilter -- ^ Apply an additional 'SelectorFilter' to the 'SelectorSequence'.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable SelectorSequence

instance NFData SelectorSequence

data PseudoSelectorSequence
    = SelectorSequence SelectorSequence
    | SelectorSequence :.:: PseudoElement
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable PseudoSelectorSequence

instance NFData PseudoSelectorSequence

(.::) :: SelectorSequence -> PseudoElement -> PseudoSelectorSequence
(.::) = (:.::)

-- | Add a given list of 'SelectorFilter's to the given 'SelectorSequence'. The
-- filters are applied left-to-right.
addFilters :: SelectorSequence -- ^ The 'SelectorSequence' to apply the filter on.
    -> [SelectorFilter] -- ^ The list of 'SelectorFilter's to apply on the 'SelectorSequence'.
    -> SelectorSequence -- ^ A modified 'SelectorSequence' where we applied the list of 'SelectorFilter's.
addFilters = foldl Filter

-- | An infix variant of the 'addFilters' function.
(.@) :: SelectorSequence -- ^ The 'SelectorSequence' to apply the filter on.
    -> [SelectorFilter] -- ^ The list of 'SelectorFilter's to apply on the 'SelectorSequence'.
    -> SelectorSequence -- ^ A modified 'SelectorSequence' where we applied the list of 'SelectorFilter's.
(.@) = addFilters

-- | Obtain the list of filters that are applied in the given 'SelectorSequence'
-- in /reversed/ order.
filters' :: SelectorSequence -- ^ The given 'SelectorSequence' to analyze.
    -> [SelectorFilter] -- ^ The given list of 'SelectorFilter's applied in /reversed/ order, this can be empty.
filters' = unfoldr go
    where go (Filter s f) = Just (f, s)
          go (SimpleSelector _) = Nothing

-- | Obtain the list of filters that are applied in the given
-- 'SelectorSequence'.
filters :: SelectorSequence -- ^ The given 'SelectorSequence' to analyze.
    -> [SelectorFilter] -- ^ The given list of 'SelectorFilter's applied, this can be empty.
filters = reverse . filters'

-- | A type that sums up the different ways to filter a type selector: with an
-- id (hash), a class, and an attribute.
data SelectorFilter
    = SHash Hash -- ^ A 'Hash' object as filter.
    | SClass Class -- ^ A 'Class' object as filter.
    | SAttrib Attrib -- ^ An 'Attrib' object as filter.
    | SPseudo PseudoClass -- ^ A 'PseudoClass' object as filter.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable SelectorFilter

instance NFData SelectorFilter

-- | A css attribute can come in two flavors: either a constraint that the
-- attribute should exists, or a constraint that a certain attribute should have
-- a certain value (prefix, suffix, etc.).
data Attrib =
      Exist AttributeName -- ^ A constraint that the given 'AttributeName' should exist.
    | Attrib AttributeName AttributeCombinator AttributeValue -- ^ A constraint about the value associated with the given 'AttributeName'.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable Attrib

instance NFData Attrib

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


-- | Filter a given 'SelectorSequence' with a given 'PseudoClass'.
(.:) :: SelectorSequence -- ^ The given 'SelectorSequence' to filter.
    -> PseudoClass -- ^ The given 'PseudoClass' to filter the 'SelectorSequence' further.
    -> SelectorSequence -- ^ A 'SelectorSequence' that is filtered additionally with the given 'PseudoClass'.
(.:) = (. SPseudo) . Filter

-- | The namespace of a css selector tag. The namespace can be 'NAny' (all
-- possible namespaces), or a namespace with a given text (this text can be
-- empty).
data Namespace =
      NAny -- ^ A typeselector part that specifies that we accept all namespaces, in css denoted with @*@.
    | Namespace Text -- ^ A typselector part that specifies that we accept a certain namespace name.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable Namespace

instance NFData Namespace

-- | The empty namespace. This is /not/ the wildcard namespace (@*@). This is a
-- bidirectional namespace and can thus be used in expressions as well.
pattern NEmpty :: Namespace
pattern NEmpty = Namespace ""

-- | The element name of a css selector tag. The element name can be 'EAny' (all
-- possible tag names), or an element name with a given text.
data ElementName =
      EAny -- ^ A typeselector part that specifies that we accept all element names, in css denoted with @*@.
    | ElementName Text -- ^ A typeselector part that specifies that we accept a certain element name.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable ElementName

instance NFData ElementName

-- | A typeselector is a combination of a selector for a namespace, and a
-- selector for an element name. One, or both can be a wildcard.
data TypeSelector = TypeSelector {
    selectorNamespace :: Namespace, -- ^ The selector for the namespace.
    elementName :: ElementName -- ^ The selector for the element name.
  } deriving (Data, Eq, Generic, Ord, Show)

instance Hashable TypeSelector

instance NFData TypeSelector

-- | An attribute name is a name that optionally has a namespace, and the name
-- of the attribute.
data AttributeName = AttributeName {
    attributeNamespace :: Namespace, -- ^ The namespace to which the attribute name belongs. This can be 'NAny' as well.
    attributeName :: Text  -- ^ The name of the attribute over which we make a claim.
  } deriving (Data, Eq, Generic, Ord, Show)

instance Hashable AttributeName

instance NFData AttributeName

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
    deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable AttributeCombinator

instance NFData AttributeCombinator

-- https://www.w3schools.com/css/css_pseudo_classes.asp
data PseudoClass
  = Active
  | Checked
  | Disabled
  | Empty
  | Enabled
  -- | FirstChild
  -- | FirstOfType
  | Focus
  | Hover
  | InRange
  | Invalid
  -- TODO: Lang
  -- | LastChild
  -- | LastOfType
  | Link
  -- TODO: Not
  | NthChild Nth
  | NthLastChild Nth
  | NthLastOfType Nth
  | NthOfType Nth
  | OnlyOfType
  | OnlyChild
  | Optional
  | OutOfRange
  | ReadOnly
  | ReadWrite
  | Required
  | Root
  | Target
  | Valid
  | Visited
  deriving (Data, Eq, Generic, Ord, Read, Show)

instance Hashable PseudoClass

instance NFData PseudoClass

pattern FirstChild :: PseudoClass
pattern FirstChild = NthChild One

pattern FirstOfType :: PseudoClass
pattern FirstOfType = NthOfType One

pattern LastChild :: PseudoClass
pattern LastChild = NthLastChild One

pattern LastOfType :: PseudoClass
pattern LastOfType = NthLastOfType One

data PseudoElement
  = After
  | Before
  | FirstLetter
  | FirstLine
  | Marker
  | Selection
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable PseudoElement

instance NFData PseudoElement

-- | A css class, this is wrapped in a data type. The type only wraps the class
-- name, not the dot prefix.
newtype Class = Class {
    unClass :: Text -- ^ Obtain the name from the class.
  } deriving (Data, Eq, Generic, Ord, Show)

instance Hashable Class

instance NFData Class

-- | A css hash (used to match an element with a given id). The type only wraps
-- the hash name, not the hash (@#@) prefix.
newtype Hash = Hash {
    unHash :: Text -- ^ Obtain the name from the hash.
  } deriving (Data, Eq, Generic, Ord, Show)

instance Hashable Hash

instance NFData Hash

-- | Convert the given 'AttributeCombinator' to its css selector counterpart.
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
#if __GLASGOW_HASKELL__ < 803
    mappend = (<>)
#endif

instance Monoid Namespace where
    mempty = NAny
#if __GLASGOW_HASKELL__ < 803
    mappend = (<>)
#endif

instance Monoid ElementName where
    mempty = EAny
#if __GLASGOW_HASKELL__ < 803
    mappend = (<>)
#endif

-- IsString instances
instance IsString Class where
    fromString = toIdentifier Class

instance IsString Hash where
    fromString = toIdentifier Hash

instance IsString Namespace where
    fromString = toIdentifier Namespace

instance IsString ElementName where
    fromString = toIdentifier ElementName

instance IsString AttributeName where
    fromString = toIdentifier (AttributeName NAny)

instance IsString Attrib where
    fromString = Exist . fromString

instance IsString PseudoClass where
    fromString = go . map toLower
      where go "active" = Active
            go "checked" = Checked
            go "disabled" = Disabled
            go "empty" = Empty
            go "enabled" = Enabled
            go "first-child" = FirstChild
            go "first-of-type" = FirstOfType
            go "focus" = Focus
            go "hover" = Hover
            go "in-range" = InRange
            go "invalid" = Invalid
            go "last-child" = LastChild
            go "last-of-type" = LastOfType
            go "link" = Link
            -- TODO: items with an Nth
            go "only-of-type" = OnlyOfType
            go "only-child" = OnlyChild
            go "optional" = Optional
            go "out-of-range" = OutOfRange
            go "read-only"= ReadOnly
            go "read-write" = ReadWrite
            go "required" = Required
            go "root" = Root
            go "target" = Target
            go "valid" = Valid
            go "visited" = Visited
            go x = error ("The pseudo class \"" ++ x ++ "\" is not a valid pseudo class.")

instance IsString PseudoElement where
    fromString = go . map toLower
      where go "after" = After
            go "before" = Before
            go "first-letter" = FirstLetter
            go "first-line" = FirstLine
            go "selection" = Selection
            go x = error ("The pseudo element \"" ++ x ++ "\" is not a valid pseudo element.")


-- IsList instances
instance IsList SelectorGroup where
    type Item SelectorGroup = Selector
    fromList = SelectorGroup . fromList
    toList (SelectorGroup ss) = toList ss

-- ToCssSelector instances
_textToPattern :: Text -> Pat
_textToPattern t = ViewP (AppE (ConE '(==)) (AppE (ConE 'pack) (LitE (StringL (unpack t))))) (_constantP 'True)

_constantP :: Name -> Pat
_constantP = (`ConP` [])

instance ToCssSelector SelectorGroup where
    toCssSelector (SelectorGroup g) = intercalate " , " (map toCssSelector (toList g))
    toSelectorGroup = id
    specificity' (SelectorGroup g) = foldMap specificity' g
    toPattern (SelectorGroup g) = ConP 'SelectorGroup [go g]
        where go (x :| xs) = ConP '(:|) [toPattern x, ListP (map toPattern xs)]
    normalize (SelectorGroup g) = SelectorGroup (Data.List.NonEmpty.sort (normalize <$> g))

instance ToCssSelector Class where
    toCssSelector = cons '.' . encodeIdentifier . unClass
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
    toCssSelector (AttributeName NAny e) = encodeIdentifier e
    toCssSelector (AttributeName n e) = toCssSelector n <> "|" <> encodeIdentifier e
    toSelectorGroup = toSelectorGroup . Exist
    specificity' = mempty
    toPattern (AttributeName n a) = ConP 'AttributeName [toPattern n, _textToPattern a]

instance ToCssSelector Hash where
    toCssSelector = cons '#' . encodeIdentifier . unHash
    toSelectorGroup = toSelectorGroup . SHash
    specificity' = const (SelectorSpecificity 1 0 0)
    toPattern (Hash h) = ConP 'Hash [_textToPattern h]

instance ToCssSelector Namespace where
    toCssSelector NAny = "*"
    toCssSelector (Namespace t) = encodeIdentifier t
    toSelectorGroup = toSelectorGroup . flip TypeSelector EAny
    specificity' = mempty
    toPattern NAny = _constantP 'NAny
    -- used to make patterns more readable
    toPattern NEmpty = _constantP 'NEmpty
    toPattern (Namespace t) = ConP 'Namespace [_textToPattern t]

instance ToCssSelector SelectorSequence where
    toCssSelector (SimpleSelector s) = toCssSelector s
    toCssSelector (Filter s f) = toCssSelector s <> toCssSelector f
    toSelectorGroup = toSelectorGroup . SelectorSequence
    specificity' (SimpleSelector s) = specificity' s
    specificity' (Filter s f) = specificity' s <> specificity' f
    toPattern (SimpleSelector s) = ConP 'SimpleSelector [toPattern s]
    toPattern (Filter s f) = ConP 'Filter [toPattern s, toPattern f]
    normalize = flip go []
        where go (Filter s f) = go s . (normalize f:)
              go (SimpleSelector s) = addFilters (SimpleSelector (normalize s)) . sort

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
    toCssSelector (ElementName e) = encodeIdentifier e
    toSelectorGroup = toSelectorGroup . TypeSelector NAny
    specificity' EAny = mempty
    specificity' (ElementName _) = SelectorSpecificity 0 0 1
    toPattern EAny = _constantP 'EAny
    toPattern (ElementName e) = ConP 'ElementName [_textToPattern e]

instance ToCssSelector SelectorFilter where
    toCssSelector (SHash h) = toCssSelector h
    toCssSelector (SClass c) = toCssSelector c
    toCssSelector (SAttrib a) = toCssSelector a
    toCssSelector (SPseudo p) = toCssSelector p
    toSelectorGroup = toSelectorGroup . Filter (SimpleSelector Universal)
    specificity' (SHash h) = specificity' h
    specificity' (SClass c) = specificity' c
    specificity' (SAttrib a) = specificity' a
    specificity' (SPseudo p) = specificity' p
    toPattern (SHash h) = ConP 'SHash [toPattern h]
    toPattern (SClass c) = ConP 'SClass [toPattern c]
    toPattern (SAttrib a) = ConP 'SAttrib [toPattern a]
    toPattern (SPseudo p) = ConP 'SPseudo [toPattern p]

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
    normalize (Selector s) = Selector (normalize s)
    normalize (Combined s1 c s2) = Combined (normalize s1) c (normalize s2)

instance ToCssSelector PseudoSelectorSequence where
    toCssSelector (SelectorSequence ss) = toCssSelector ss
    toCssSelector (ss :.:: pe)
      | def == ss = toCssSelector pe
      | otherwise = toCssSelector ss <> toCssSelector pe
    toSelectorGroup = toSelectorGroup . Selector
    specificity' (SelectorSequence ss) = specificity' ss
    specificity' (ss :.:: pe) = specificity' ss <> specificity' pe
    toPattern (SelectorSequence ss) = ConP 'SelectorSequence [toPattern ss]
    toPattern (ss :.:: pe) = ConP '(:.::) [toPattern ss, toPattern pe]
    normalize (SelectorSequence ss) = SelectorSequence (normalize ss)
    normalize (ss :.:: pe) = normalize ss :.:: normalize pe

_nthToPat :: Nth -> Pat
_nthToPat (Nth n b) = ConP 'Nth [f n, f b]
    where f = LitP . IntegerL . fromIntegral

instance ToCssSelector PseudoClass where
    toCssSelector = cons ':' . go
      where go Active = "active"
            go Checked = "checked"
            go Disabled = "disabled"
            go Empty = "empty"
            go Enabled = "enabled"
            go Focus = "focus"
            go Hover = "hover"
            go InRange = "in-range"
            go Invalid = "invalid"
            go Link = "link"
            go FirstChild = "first-child"
            go (NthChild nth) = "nth-child(" <> nthToText nth <> ")"
            go LastChild = "last-child"
            go (NthLastChild nth) = "nth-last-child(" <> nthToText nth <> ")"
            go LastOfType = "last-of-type"
            go (NthLastOfType nth) = "nth-last-of-type(" <> nthToText nth <> ")"
            go FirstOfType = "first-of-type"
            go (NthOfType nth) = "nth-of-type(" <> nthToText nth <> ")"
            go OnlyOfType = "only-of-type"
            go OnlyChild = "only-child"
            go Optional = "optional"
            go OutOfRange = "out-of-range"
            go ReadOnly = "read-only"
            go ReadWrite = "read-write"
            go Required = "required"
            go Root = "root"
            go Target = "target"
            go Valid = "valid"
            go Visited = "visited"

    specificity' = const (SelectorSpecificity 0 1 0)  -- TODO: add items in the not(...) function
    toSelectorGroup = toSelectorGroup . SPseudo
    toPattern Active = _constantP 'Active
    toPattern Checked = _constantP 'Checked
    toPattern Disabled = _constantP 'Disabled
    toPattern Empty = _constantP 'Empty
    toPattern Enabled = _constantP 'Enabled
    toPattern Focus = _constantP 'Focus
    toPattern Hover = _constantP 'Hover
    toPattern InRange = _constantP 'InRange
    toPattern Invalid = _constantP 'Invalid
    toPattern Link = _constantP 'Link
    toPattern (NthChild nth) = ConP 'NthChild [_nthToPat nth]
    toPattern (NthLastChild nth) = ConP 'NthLastChild [_nthToPat nth]
    toPattern (NthLastOfType nth) = ConP 'NthLastOfType [_nthToPat nth]
    toPattern (NthOfType nth) = ConP 'NthOfType [_nthToPat nth]
    toPattern OnlyOfType = _constantP 'OnlyOfType
    toPattern OnlyChild = _constantP 'OnlyChild
    toPattern Optional = _constantP 'Optional
    toPattern OutOfRange = _constantP 'OutOfRange
    toPattern ReadOnly = _constantP 'ReadOnly
    toPattern ReadWrite = _constantP 'ReadWrite
    toPattern Required = _constantP 'Required
    toPattern Root = _constantP 'Root
    toPattern Target = _constantP 'Target
    toPattern Valid = _constantP 'Valid
    toPattern Visited = _constantP 'Visited
    normalize (NthChild nth) = NthChild (normalizeNth nth)
    normalize (NthLastChild nth) = NthLastChild (normalizeNth nth)
    normalize (NthLastOfType nth) = NthLastOfType (normalizeNth nth)
    normalize (NthOfType nth) = NthOfType (normalizeNth nth)
    normalize pc = pc  -- TODO: normalize item in the not(...), etc. function(s).

instance ToCssSelector PseudoElement where
    toCssSelector = pack . (':' :) . (':' :) . go
      where go After = "after"
            go Before = "before"
            go FirstLetter = "first-letter"
            go FirstLine = "first-line"
            go Marker = "marker"
            go Selection = "selection"
    specificity' = const (SelectorSpecificity 0 0 1)
    toSelectorGroup = toSelectorGroup . (def :.::)
    toPattern = _constantP . go
      where go After = 'After
            go Before = 'Before
            go FirstLetter = 'FirstLetter
            go FirstLine = 'FirstLine
            go Marker = 'Marker
            go Selection = 'Selection

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

instance Default PseudoSelectorSequence where
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

instance Default AttributeCombinator where
    def = Exact

-- | The default of the Nth instance is @n@, where all childs are selected.
instance Default Nth where
    def = Nth 1 0

-- Binary instance
_putEnum :: Enum a => a -> Put
_putEnum = putWord8 . fromIntegral . fromEnum

_getEnum :: Enum a => Get a
_getEnum = toEnum . fromIntegral <$> getWord8

instance Binary Nth where
  put (Nth n b) = put n >> put b
  get = Nth <$> get <*> get

instance Binary SelectorSpecificity where
  put (SelectorSpecificity a b c) = put a >> put b >> put c
  get = SelectorSpecificity <$> get <*> get <*> get

instance Binary Selector where
  put (Selector c) = putWord8 0 >> put c
  put (Combined c sc cs) = putWord8 1 >> put c >> put sc >> put cs
  get = do
    w <- getWord8
    case w of
      0 -> Selector <$> get
      1 -> Combined <$> get <*> get <*> get
      _ -> fail "An error occured while deserializing a Selector object."

instance Binary PseudoSelectorSequence where
  put (SelectorSequence ss) = putWord8 0 >> put ss
  put (ss :.:: pe) = putWord8 1 >> put ss >> put pe
  get = do
    w <- getWord8
    case w of
      0 -> SelectorSequence <$> get
      1 -> (:.::) <$> get <*> get
      _ -> fail "An error occured while deserializing a PseudoSelectorSequence."

instance Binary PseudoClass where
  put Active = putWord8 0
  put Checked = putWord8 1
  put Disabled = putWord8 2
  put Empty = putWord8 3
  put Enabled = putWord8 4
  put Focus = putWord8 5
  put Hover = putWord8 6
  put InRange = putWord8 7
  put Invalid = putWord8 8
  -- put  -- TODO: Lang
  put Link = putWord8 10
  -- put  -- TODO: Not
  put (NthChild nth) = putWord8 12 >> put nth
  put (NthLastChild nth) = putWord8 13 >> put nth
  put (NthLastOfType nth) = putWord8 14 >> put nth
  put (NthOfType nth) = putWord8 15 >> put nth
  put OnlyOfType = putWord8 16
  put OnlyChild = putWord8 17
  put Optional = putWord8 18
  put OutOfRange = putWord8 19
  put ReadOnly = putWord8 20
  put ReadWrite = putWord8 21
  put Required = putWord8 22
  put Root = putWord8 23
  put Target = putWord8 24
  put Valid = putWord8 25
  put Visited = putWord8 26

  get = do
    w <- getWord8
    case w of
      0 -> pure Active
      1 -> pure Checked
      2 -> pure Disabled
      3 -> pure Empty
      4 -> pure Enabled
      5 -> pure Focus
      6 -> pure Hover
      7 -> pure InRange
      8 -> pure Invalid
      -- 11  -- TODO: Lang
      10 -> pure Link
      -- 15  -- TODO: Not
      12 -> NthChild <$> get
      13 -> NthLastChild <$> get
      14 -> NthLastOfType <$> get
      15 -> NthOfType <$> get
      16 -> pure OnlyOfType
      17 -> pure OnlyChild
      18 -> pure Optional
      19 -> pure OutOfRange
      20 -> pure ReadOnly
      21 -> pure ReadWrite
      22 -> pure Required
      23 -> pure Root
      24 -> pure Target
      25 -> pure Valid
      26 -> pure Visited
      _ -> fail "An error occured while deserialzing a PseudoClass object."


instance Binary PseudoElement where
  put = _putEnum
  get = _getEnum

instance Binary SelectorCombinator where
  put = _putEnum
  get = _getEnum

instance Binary SelectorSequence where
  put (SimpleSelector ts) = putWord8 0 >> put ts
  put (Filter ss sf) = putWord8 1 >> put ss >> put sf
  get = do
    w <- getWord8
    case w of
      0 -> SimpleSelector <$> get
      1 -> Filter <$> get <*> get
      _ -> fail "An error occured while deserializing a Selector object."

instance Binary SelectorFilter where
  put (SHash h) = putWord8 0 >> put h
  put (SClass c) = putWord8 1 >> put c
  put (SAttrib a) = putWord8 2 >> put a
  put (SPseudo p) = putWord8 3 >> put p
  get = do
    w <- getWord8
    case w of
      0 -> SHash <$> get
      1 -> SClass <$> get
      2 -> SAttrib <$> get
      3 -> SPseudo <$> get
      _ -> fail "An error occurred when deserializing a SelectorFilter object."

instance Binary Attrib where
  put (Exist e) = putWord8 0 >> put e
  put (Attrib an ac av) = putWord8 1 >> put an >> put ac >> put av
  get = do
    w <- getWord8
    case w of
      0 -> Exist <$> get
      1 -> Attrib <$> get <*> get <*> get
      _ -> fail "An error occured when deserializing an Attrib object."

instance Binary Namespace where
  put NAny = putWord8 0
  put (Namespace t) = putWord8 1 >> put t
  get = do
    w <- getWord8
    case w of
      0 -> pure NAny
      1 -> Namespace <$> get
      _ -> fail "An error occurred when deserializing a Namespace object."

instance Binary ElementName where
  put EAny = putWord8 0
  put (ElementName t) = putWord8 1 >> put t
  get = do
    w <- getWord8
    case w of
      0 -> pure EAny
      1 -> ElementName <$> get
      _ -> fail "An error occurred when deserializing an ElementName."

instance Binary TypeSelector where
  put (TypeSelector ns en) = put ns >> put en
  get = TypeSelector <$> get <*> get

instance Binary AttributeName where
  put (AttributeName ns n) = put ns >> put n
  get = AttributeName <$> get <*> get

instance Binary AttributeCombinator where
  put = _putEnum
  get = _getEnum

instance Binary Hash where
  put (Hash h) = put h
  get = Hash <$> get

instance Binary Class where
  put (Class h) = put h
  get = Class <$> get

instance Binary SelectorGroup where
  put (SelectorGroup g) = put g
  get = SelectorGroup <$> get


-- Lift instances
_apply :: Name -> [Q Exp] -> Q Exp
_apply = foldl appE . conE

instance Lift SelectorGroup where
    lift (SelectorGroup sg) = _apply 'SelectorGroup [liftNe sg]
        where liftNe (a :| as) = _apply '(:|) [lift a, lift as]
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif


instance Lift Selector where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift SelectorCombinator where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift SelectorSequence where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift SelectorFilter where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift Attrib where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift PseudoClass where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift PseudoElement where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

-- ToMarkup instances
_cssToMarkup :: ToCssSelector a => a -> Markup
_cssToMarkup = text . toCssSelector

instance ToMarkup SelectorGroup where
    toMarkup = _cssToMarkup

instance ToMarkup Selector where
    toMarkup = _cssToMarkup

instance ToMarkup SelectorSequence where
    toMarkup = _cssToMarkup

instance ToMarkup PseudoSelectorSequence where
    toMarkup = _cssToMarkup

instance ToMarkup PseudoClass where
    toMarkup = _cssToMarkup

instance ToMarkup PseudoElement where
    toMarkup = _cssToMarkup

instance ToMarkup SelectorFilter where
    toMarkup = _cssToMarkup

instance ToMarkup Attrib where
    toMarkup = _cssToMarkup

-- ToJavaScript and ToJson instances
_cssToJavascript :: ToCssSelector a => a -> Javascript
#if __GLASGOW_HASKELL__ < 803
_cssToJavascript = toJavascript . toJSON . toCssSelector
#else
_cssToJavascript = toJavascript . toCssSelector
#endif

_cssToJson :: ToCssSelector a => a -> Value
_cssToJson = String . toCssSelector

instance ToJavascript SelectorGroup where
    toJavascript = _cssToJavascript

instance ToJavascript Selector where
    toJavascript = _cssToJavascript

instance ToJavascript SelectorSequence where
    toJavascript = _cssToJavascript

instance ToJavascript PseudoSelectorSequence where
    toJavascript = _cssToJavascript

instance ToJavascript PseudoClass where
    toJavascript = _cssToJavascript

instance ToJavascript PseudoElement where
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

instance ToJSON PseudoSelectorSequence where
    toJSON = _cssToJson

instance ToJSON PseudoClass where
    toJSON = _cssToJson

instance ToJSON PseudoElement where
    toJSON = _cssToJson

instance ToJSON Attrib where
    toJSON = _cssToJson


-- Arbitrary instances
_arbitraryIdent :: Gen Text
_arbitraryIdent = pack <$> listOf1 arbitrary

_shrinkText :: Text -> [Text]
_shrinkText = liftA2 (zipWith (<>)) inits (tails . T.drop 1)

_shrinkIdent :: Text -> [Text]
_shrinkIdent t
    | T.length t < 2 = []
    | otherwise = _shrinkText t

instance Arbitrary Hash where
    arbitrary = Hash <$> _arbitraryIdent
    shrink (Hash a) = Hash <$> _shrinkIdent a

instance Arbitrary Class where
    arbitrary = Class <$> _arbitraryIdent
    shrink (Class a) = Class <$> _shrinkIdent a

instance Arbitrary Nth where
    -- arbitrary = oneof [pure Even, pure Odd]
    arbitrary = Nth <$> arbitrary <*> arbitrary
    shrink nth
      | nth == nnth = []
      | otherwise = [nnth]
      where nnth = normalizeNth nth

instance Arbitrary Namespace where
    arbitrary = frequency [(3, pure NAny), (1, Namespace <$> _arbitraryIdent)]
    shrink NAny = []
    shrink (Namespace a) = Namespace <$> _shrinkIdent a

instance Arbitrary ElementName where
    arbitrary = frequency [(1, pure EAny), (3, ElementName <$> _arbitraryIdent)]
    shrink EAny = []
    shrink (ElementName a) = ElementName <$> _shrinkIdent a

instance Arbitrary TypeSelector where
    arbitrary = TypeSelector <$> arbitrary <*> arbitrary
    shrink (TypeSelector x y) = (TypeSelector x <$> shrink y) ++ ((`TypeSelector` y) <$> shrink x)

instance Arbitrary SelectorSequence where
    arbitrary = addFilters . SimpleSelector <$> arbitrary <*> listOf arbitrary
    shrink (SimpleSelector ss) = SimpleSelector <$> shrink ss
    shrink (Filter ss sf) = ss : ((`Filter` sf) <$> shrink ss) ++ (Filter ss <$> shrink sf)

instance Arbitrary PseudoSelectorSequence where
    arbitrary = frequency [(3, SelectorSequence <$> arbitrary), (1, (:.::) <$> arbitrary <*> arbitrary)]
    shrink (SelectorSequence ss) = SelectorSequence <$> shrink ss
    shrink (ss :.:: pe) = SelectorSequence ss : ((ss :.::) <$> shrink pe) ++ ((:.:: pe) <$> shrink ss)

instance Arbitrary SelectorCombinator where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary AttributeCombinator where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SelectorFilter where
    arbitrary = oneof [SHash <$> arbitrary, SClass <$> arbitrary, SAttrib <$> arbitrary, SPseudo <$> arbitrary]
    shrink (SHash x) = SHash <$> shrink x
    shrink (SClass x) = SClass <$> shrink x
    shrink (SAttrib x) = SAttrib <$> shrink x
    shrink (SPseudo x) = SPseudo <$> shrink x

instance Arbitrary AttributeName where
    arbitrary = AttributeName <$> arbitrary <*> _arbitraryIdent
    shrink (AttributeName x y) = (AttributeName x <$> _shrinkIdent y) ++ ((`AttributeName` y) <$> shrink x)

instance Arbitrary Attrib where
    arbitrary = oneof [Exist <$> arbitrary, Attrib <$> arbitrary <*> arbitrary <*> (pack <$> listOf arbitrary)]
    shrink (Exist x) = Exist <$> shrink x
    shrink (Attrib x y z) = (Attrib x y <$> _shrinkText z) ++ ((\sx -> Attrib sx y z) <$> shrink x)

instance Arbitrary SelectorGroup where
    arbitrary = SelectorGroup <$> ((:|) <$> arbitrary <*> arbitrary)
    shrink (SelectorGroup (x :| xs)) = go xs (SelectorGroup . (x :|) <$> shrink xs)
      where go [] = id
            go (y:ys) = (SelectorGroup (y :| ys) :)

instance Arbitrary Selector where
    arbitrary = frequency [(3, Selector <$> arbitrary), (1, Combined <$> arbitrary <*> arbitrary <*> arbitrary) ]
    shrink (Selector x) = Selector <$> shrink x
    shrink (Combined x y z) = z : (Combined x y <$> shrink z) ++ ((\sx -> Combined sx y z) <$> shrink x)

instance Arbitrary PseudoClass where
    arbitrary = oneof (map pure [
        Active, Checked, Disabled, Empty, Enabled, Focus, Hover, InRange, Invalid, Link, OnlyOfType, OnlyChild
      , Optional, OutOfRange, ReadOnly, ReadWrite, Required, Root, Target, Valid, Visited
      ] ++ map (<$> arbitrary) [NthChild, NthLastChild, NthLastOfType, NthOfType])


instance Arbitrary PseudoElement where
    arbitrary = arbitraryBoundedEnum
