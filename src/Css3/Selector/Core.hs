{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, OverloadedStrings, PatternSynonyms, TemplateHaskellQuotes, TypeFamilies #-}

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
    , PseudoElement(After, Before, FirstLetter, FirstLine, Marker, Placeholder, Selection), PseudoSelectorSequence(Sequence, (:.::)), (.::)
    , PseudoClass(
          Active, Checked, Default, Disabled, Empty, Enabled, Focus, Fullscreen, Hover, Indeterminate, InRange, Invalid, Lang
        , Link, NthChild, NthLastChild, NthLastOfType, NthOfType, OnlyOfType, OnlyChild, Optional, OutOfRange, ReadOnly
        , ReadWrite, Required, Root, Target, Valid, Visited
        ), (.:), pattern FirstChild, pattern FirstOfType, pattern LastChild, pattern LastOfType, Language
    , SelectorSequence(..)
    , combinatorText, combine
    , (.>), (.+), (.~)
    -- * Filters
    , SelectorFilter(SHash, SClass, SAttrib, SPseudo, SNot), filters, filters', addFilters, (.@)
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
    -- * Negation
    , Negation(NTypeSelector, NHash, NClass, NAttrib, NPseudo, NPseudoElement)
    -- * Nth items
    , Nth(Nth, linear, constant), pattern Even, pattern Odd, pattern One, nthValues, nthIsEmpty, nthValues0, nthValues1, normalizeNth, nthContainsValue
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
import Data.Default.Class(Default(def))
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
import Language.Haskell.TH.Syntax(Lift(lift, liftTyped), Quote, Exp(AppE, ConE, LitE), Lit(IntegerL, StringL), Name, Pat(ConP, ListP, LitP, ViewP), unsafeCodeCoerce)
#elif MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Syntax(Lift(lift, liftTyped), Exp(AppE, ConE, LitE), Lit(IntegerL, StringL), Name, Pat(ConP, ListP, LitP, ViewP), Q, unsafeTExpCoerce)
#else
import Language.Haskell.TH.Syntax(Lift(lift), Exp(AppE, ConE, LitE), Lit(IntegerL, StringL), Name, Pat(ConP, ListP, LitP, ViewP), Q)
#endif

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary, shrink), arbitraryBoundedEnum)
import Test.QuickCheck.Gen(Gen, elements, frequency, listOf, listOf1, oneof)

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

-- | A data type that is used to select children and elements of type with the @:nth-child@, @:nth-last-child@, @:nth-last-of-type@ and @:nth-of-type@.
-- if the 'One' is used as argument, then the pseudo classes are @:first-child@, @:first-of-type@, @:last-child@, and @:last-of-type@.
data Nth
  = Nth {
    linear :: Int  -- ^ The linear part of the 'Nth' object: the integral number before the @n@.
  , constant :: Int  -- ^ The constant part of the 'Nth' object.
  } deriving (Data, Eq, Generic, Ord, Read, Show)

instance Hashable Nth

instance NFData Nth

-- | Check if the given 'Nth' object contains /no/ items.
nthIsEmpty
  :: Nth  -- ^ The given 'Nth' object object to check.
  -> Bool  -- ^ 'True' if the given 'Nth' object does /not/ contain any items; 'False' otherwise.
nthIsEmpty (Nth n c) = n <= 0 && c <= 0

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

-- | Check if the given 'Nth' object contains a given value.
nthContainsValue
  :: Nth -- ^ The given 'Nth' object that specifies a sequence.
  -> Int  -- ^ The given index for which we check if it is contained in the given 'Nth' object.
  -> Bool  -- ^ This function returns 'True' if the given item is a member of the given 'Nth' sequence; 'False' otherwise.
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

-- | A pattern synonym that is used in CSS to specify a sequence that starts with two and each time increases with two.
pattern Even :: Nth
pattern Even = Nth 2 0

-- | A pattern synonym that is used in CSS to specify a sequence that starts with one and each time increases with two.
pattern Odd :: Nth
pattern Odd = Nth 2 1

-- | An 'Nth' item that spans a collection with only @1@ as value. This is used to transform @:nth-child@ to @:first-child@ for example.
pattern One :: Nth
pattern One = Nth 0 1

-- | Convert the given 'Nth' object to text used by the CSS selector.
nthToText
  :: Nth  -- ^ The 'Nth' object for which we determine the textual presentation.
  -> Text -- ^ The textual presentation of the 'Nth' object in a CSS selector.
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

-- | A 'SelectorSequence' with an optional 'PseudoElement' at the end. Each /element/ of a 'Selector' can
-- have /at most/ one 'PseudoElement'.
data PseudoSelectorSequence
    = Sequence SelectorSequence  -- ^ A data constructor where there is no optional 'PseudoElement' involved.
    | SelectorSequence :.:: PseudoElement  -- ^ A data constructor for a 'SelectorSequence' with a 'PseudoElement'.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable PseudoSelectorSequence

instance NFData PseudoSelectorSequence

-- | Add a given 'PseudoElement' to the given 'SelectorSequence' to produce a 'PseudoSelectorSequence'. Since
-- a 'PseudoElement' is an instance of 'IsString', this can thus be used to combine string literals.
(.::)
  :: SelectorSequence  -- ^ The given 'SelectorSequence' to which we add the pseudo element.
  -> PseudoElement  -- ^ The given 'PseudoElement' to add to the 'SelectorSequence'.
  -> PseudoSelectorSequence  -- ^ The corresponding 'PseudoSelectorSequence'.
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
    | SNot Negation  -- ^ A @:not(…)@ clause that contains a simple selector to negate.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable SelectorFilter

instance NFData SelectorFilter

-- | A data type that contains all possible items that can be used in a @:not(…)@ clause.
-- Since a @:not(…)@ cannot be nested in another @:not(…)@, we see an 'SNot' as a special
-- case, and not as a 'PseudoClass'.
data Negation
    = NTypeSelector TypeSelector  -- ^ A 'TypeSelector' for the @:not(…)@ clause.
    | NHash Hash  -- ^ A 'Hash' for the @:not(…)@ clause.
    | NClass Class  -- ^ A 'Class' for the @:not(…)@ clause.
    | NAttrib Attrib  -- ^ An 'Attrib' for the @:not(…)@ clause.
    | NPseudo PseudoClass  -- ^ A 'PseudoClass' for the @:not(…)@ clause.
    | NPseudoElement PseudoElement  -- ^ A 'PseudoElement' for the @:not(…)@ clause.
    deriving (Data, Eq, Generic, Ord, Show)

instance Hashable Negation

instance NFData Negation

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

-- | We use 'Text' to specify the language in the @:lang(…)@ pseudo class.
type Language = Text

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

-- | A data type that contains the possible pseudo classes. In a CSS selector
-- the pseudo classes are specified with a single colon, for example @:active@.
-- These filter on the /state/ of the items. A full list of pseudo classes
-- is available <https://www.w3schools.com/css/css_pseudo_classes.asp here>.
data PseudoClass
  = Active  -- ^ The @:active@ pseudo class.
  | Checked  -- ^ The @:checked@ pseudo class.
  | Default  -- ^ The @:default@ pseudo class.
  | Disabled  -- ^ The @:disabled@ pseudo class.
  | Empty  -- ^ The @:empty@ pseudo class.
  | Enabled  -- ^ The @:enabled@ pseudo class.
  | Focus  -- ^ The @:focus@ pseudo class.
  | Fullscreen  -- ^ The @:fullscreen@ pseudo class.
  | Hover  -- ^ The @:hover@ pseudo class.
  | Indeterminate  -- ^ The @:indeterminate@ pseudo class.
  | InRange  -- ^ The @:in-range@ pseudo class.
  | Invalid  -- ^ The @:invalid@ pseudo class.
  | Lang Language  -- ^ The @:lang(…)@ pseudo class, the language parameter is at the moment a 'Text' object, but only uppercase, lowercase and hyphens are characters that can be parsed.
  | Link  -- ^ The @:link@ pseudo class.
  | NthChild Nth  -- ^ The @:nth-child(…)@ pseudo class, if the 'Nth' parameter is 'One', then it is equivalent to @:first-child@.
  | NthLastChild Nth  -- ^ The @:nth-last-child(…)@ pseudo class, if the 'Nth' parameter is 'One', then it is equivalent to @:last-child@.
  | NthLastOfType Nth  -- ^ The @:nth-last-of-type(…)@ pseudo class, if the 'Nth' parameter is 'One', then it is equivalent to @:last-of-type@.
  | NthOfType Nth  -- ^ The @:nth-of-type(…)@ pseudo class, if the 'Nth' parameter is 'One', then it is equivalent to @:first-of-type@.
  | OnlyOfType  -- ^ The @:only-of-type@ pseudo class.
  | OnlyChild  -- ^ The @:only-child@ pseudo class.
  | Optional  -- ^ The @:optional@ pseudo class.
  | OutOfRange  -- ^ The @:out-of-range@ pseudo class.
  | ReadOnly  -- ^ The @:read-only@ pseudo class.
  | ReadWrite  -- ^ The @:rad-write@ pseudo class.
  | Required  -- ^ The @:required@ pseudo class.
  | Root  -- ^ The @:root@ pseudo class.
  | Target  -- ^ The @:target@ pseudo class.
  | Valid  -- ^ The @:valid@ pseudo class.
  | Visited  -- ^ The @:visited@ pseudo class.
  deriving (Data, Eq, Generic, Ord, Read, Show)

instance Hashable PseudoClass

instance NFData PseudoClass

-- | A pattern synonym for @:nth-child(1)@. If @NthChild (Nth 0 1)@ is used, then
-- this will render as @:first-child@.
pattern FirstChild :: PseudoClass
pattern FirstChild = NthChild One

-- | A pattern synonym for @:nth-of-type(1)@. If @NthOfType (Nth 0 1)@ is used, then
-- this will render as @:first-of-type@.
pattern FirstOfType :: PseudoClass
pattern FirstOfType = NthOfType One

-- | A pattern synonym for @:nth-last-child(1)@. If @NthLastChild (Nth 0 1)@ is used, then
-- this will render as @:last-child@.
pattern LastChild :: PseudoClass
pattern LastChild = NthLastChild One

-- | A pattern synonym for @:nth-last-of-type(1)@. If @NthLastOfType (Nth 0 1)@ is used, then
-- this will render as @:last-of-type@.
pattern LastOfType :: PseudoClass
pattern LastOfType = NthLastOfType One

-- | An enum type that contains the possible /pseudo elements/. A pseudo
-- element is specified by two colon characters (@::@), followed by the name of
-- the pseudo element. The 'After', 'Before', 'FirstLine' and 'FirstLetter'
-- can be written with a single colon for backwards compatibility with
-- CSS 1 and CSS 2.
data PseudoElement
  = After  -- ^ The @::after@ pseudo-elements can be used to describe generated content after an element’s content.
  | Before  -- ^ The @::before@ pseudo-element can be used to describe generated content before an element’s content.
  | FirstLetter  -- ^ The @::first-line@ pseudo-element describes the contents of the first formatted line of an element.
  | FirstLine  -- ^ The @::first-letter@ pseudo-element represents the first letter of an element, if it is not preceded by any other content (such as images or inline tables) on its line.
  | Marker -- ^ The @::marker@ pseudo-element selects the markers of list items.
  | Placeholder -- ^ The @::placeholder@ pseudo-element selects form elements with placeholder text, and let you style the placeholder text.
  | Selection -- ^ The @::selection@ pseudo-element matches the portion of an element that is selected by a user.
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
            go "default" = Default
            go "disabled" = Disabled
            go "empty" = Empty
            go "enabled" = Enabled
            go "first-child" = FirstChild
            go "first-of-type" = FirstOfType
            go "focus" = Focus
            go "fullscreen" = Fullscreen
            go "hover" = Hover
            go "indeterminate" = Indeterminate
            go "in-range" = InRange
            go "invalid" = Invalid
            go "last-child" = LastChild
            go "last-of-type" = LastOfType
            go "link" = Link
            --  items with :lang(...) and :...(nth)
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

instance IsString ((->) Nth PseudoClass) where
  fromString = go . map toLower
    where go "nth-child" = NthChild
          go "nth-last-child" = NthLastChild
          go "nth-last-of-type" = NthLastOfType
          go "nth-of-type" = NthLastOfType
          go x = error ("There is no pseudo class \"" ++ x ++ "\" that takes an Nth object as parameter.")

instance IsString PseudoElement where
    fromString = go . map toLower
      where go "after" = After
            go "before" = Before
            go "first-letter" = FirstLetter
            go "first-line" = FirstLine
            go "marker" = Marker
            go "placeholder" = Placeholder
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

#if MIN_VERSION_template_haskell(2,18,0)
_constantP :: Name -> Pat
_constantP = flip (`ConP` []) []
#else
_constantP :: Name -> Pat
_constantP = (`ConP` [])
#endif

#if MIN_VERSION_template_haskell(2,18,0)
_conP :: Name -> [Pat] -> Pat
_conP = (`ConP` [])
#else
_conP :: Name -> [Pat] -> Pat
_conP = ConP
#endif


instance ToCssSelector SelectorGroup where
    toCssSelector (SelectorGroup g) = intercalate " , " (map toCssSelector (toList g))
    toSelectorGroup = id
    specificity' (SelectorGroup g) = foldMap specificity' g
    toPattern (SelectorGroup g) = _conP 'SelectorGroup [go g]
        where go (x :| xs) = _conP '(:|) [toPattern x, ListP (map toPattern xs)]
    normalize (SelectorGroup g) = SelectorGroup (Data.List.NonEmpty.sort (normalize <$> g))

instance ToCssSelector Class where
    toCssSelector = cons '.' . encodeIdentifier . unClass
    toSelectorGroup = toSelectorGroup . SClass
    specificity' = const (SelectorSpecificity 0 1 0)
    toPattern (Class c) = _conP 'Class [_textToPattern c]

instance ToCssSelector Attrib where
    toCssSelector (Exist name) = "[" <> toCssSelector name <> "]"
    toCssSelector (Attrib name op val) = "[" <> toCssSelector name <> attributeCombinatorText op <> encodeText '"' val <> "]"
    toSelectorGroup = toSelectorGroup . SAttrib
    specificity' = const (SelectorSpecificity 0 1 0)
    toPattern (Exist name) = _conP 'Exist [toPattern name]
    toPattern (Attrib name op val) = _conP 'Attrib [toPattern name, _constantP (go op), _textToPattern val]
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
    toPattern (AttributeName n a) = _conP 'AttributeName [toPattern n, _textToPattern a]

instance ToCssSelector Hash where
    toCssSelector = cons '#' . encodeIdentifier . unHash
    toSelectorGroup = toSelectorGroup . SHash
    specificity' = const (SelectorSpecificity 1 0 0)
    toPattern (Hash h) = _conP 'Hash [_textToPattern h]

instance ToCssSelector Namespace where
    toCssSelector NAny = "*"
    toCssSelector (Namespace t) = encodeIdentifier t
    toSelectorGroup = toSelectorGroup . flip TypeSelector EAny
    specificity' = mempty
    toPattern NAny = _constantP 'NAny
    -- used to make patterns more readable
    toPattern NEmpty = _constantP 'NEmpty
    toPattern (Namespace t) = _conP 'Namespace [_textToPattern t]

instance ToCssSelector SelectorSequence where
    toCssSelector (SimpleSelector s) = toCssSelector s
    toCssSelector (Filter s f) = toCssSelector s <> toCssSelector f
    toSelectorGroup = toSelectorGroup . Sequence
    specificity' (SimpleSelector s) = specificity' s
    specificity' (Filter s f) = specificity' s <> specificity' f
    toPattern (SimpleSelector s) = _conP 'SimpleSelector [toPattern s]
    toPattern (Filter s f) = _conP 'Filter [toPattern s, toPattern f]
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
    toPattern (TypeSelector n t) = _conP 'TypeSelector [toPattern n, toPattern t]

instance ToCssSelector ElementName where
    toCssSelector EAny = "*"
    toCssSelector (ElementName e) = encodeIdentifier e
    toSelectorGroup = toSelectorGroup . TypeSelector NAny
    specificity' EAny = mempty
    specificity' (ElementName _) = SelectorSpecificity 0 0 1
    toPattern EAny = _constantP 'EAny
    toPattern (ElementName e) = _conP 'ElementName [_textToPattern e]

instance ToCssSelector SelectorFilter where
    toCssSelector (SHash h) = toCssSelector h
    toCssSelector (SClass c) = toCssSelector c
    toCssSelector (SAttrib a) = toCssSelector a
    toCssSelector (SPseudo p) = toCssSelector p
    toCssSelector (SNot n) = toCssSelector n
    toSelectorGroup = toSelectorGroup . Filter (SimpleSelector Universal)
    specificity' (SHash h) = specificity' h
    specificity' (SClass c) = specificity' c
    specificity' (SAttrib a) = specificity' a
    specificity' (SPseudo p) = specificity' p
    specificity' (SNot n) = specificity' n  -- Selectors inside the negation pseudo-class are counted like any other, but the negation itself does not count as a pseudo-class.
    toPattern (SHash h) = _conP 'SHash [toPattern h]
    toPattern (SClass c) = _conP 'SClass [toPattern c]
    toPattern (SAttrib a) = _conP 'SAttrib [toPattern a]
    toPattern (SPseudo p) = _conP 'SPseudo [toPattern p]
    toPattern (SNot n) = _conP 'SNot [toPattern n]

instance ToCssSelector Selector where
    toCssSelector (Selector s) = toCssSelector s
    toCssSelector (Combined s1 c s2) = toCssSelector s1 <> combinatorText c <> toCssSelector s2
    toSelectorGroup = toSelectorGroup . SelectorGroup . pure
    specificity' (Selector s) = specificity' s
    specificity' (Combined s1 _ s2) = specificity' s1 <> specificity' s2
    toPattern (Selector s) = _conP 'Selector [toPattern s]
    toPattern (Combined s1 c s2) = _conP 'Combined [toPattern s1, _constantP (go c), toPattern s2]
        where go Descendant = 'Descendant
              go Child = 'Child
              go DirectlyPreceded = 'DirectlyPreceded
              go Preceded = 'Preceded
    normalize (Selector s) = Selector (normalize s)
    normalize (Combined s1 c s2) = Combined (normalize s1) c (normalize s2)

instance ToCssSelector PseudoSelectorSequence where
    toCssSelector (Sequence ss) = toCssSelector ss
    toCssSelector (ss :.:: pe)
      | def == ss = toCssSelector pe
      | otherwise = toCssSelector ss <> toCssSelector pe
    toSelectorGroup = toSelectorGroup . Selector
    specificity' (Sequence ss) = specificity' ss
    specificity' (ss :.:: pe) = specificity' ss <> specificity' pe
    toPattern (Sequence ss) = _conP 'Sequence [toPattern ss]
    toPattern (ss :.:: pe) = _conP '(:.::) [toPattern ss, toPattern pe]
    normalize (Sequence ss) = Sequence (normalize ss)
    normalize (ss :.:: pe) = normalize ss :.:: normalize pe

_nthToPat :: Nth -> Pat
_nthToPat (Nth n b) = _conP 'Nth [f n, f b]
    where f = LitP . IntegerL . fromIntegral

instance ToCssSelector PseudoClass where
    toCssSelector = cons ':' . go
      where go Active = "active"
            go Checked = "checked"
            go Default = "default"
            go Disabled = "disabled"
            go Empty = "empty"
            go Enabled = "enabled"
            go Focus = "focus"
            go Fullscreen = "fullscreen"
            go Hover = "hover"
            go Indeterminate = "indeterminate"
            go InRange = "in-range"
            go Invalid = "invalid"
            go Link = "link"
            go (Lang l) = "lang(" <> l <> ")"
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

    specificity' = const (SelectorSpecificity 0 1 0)
    toSelectorGroup = toSelectorGroup . SPseudo
    toPattern Active = _constantP 'Active
    toPattern Checked = _constantP 'Checked
    toPattern Default = _constantP 'Default
    toPattern Disabled = _constantP 'Disabled
    toPattern Empty = _constantP 'Empty
    toPattern Enabled = _constantP 'Enabled
    toPattern Focus = _constantP 'Focus
    toPattern Fullscreen = _constantP 'Fullscreen
    toPattern Hover = _constantP 'Hover
    toPattern Indeterminate = _constantP 'Indeterminate
    toPattern InRange = _constantP 'InRange
    toPattern Invalid = _constantP 'Invalid
    toPattern Link = _constantP 'Link
    toPattern (Lang l) = _conP 'Lang [_textToPattern l]
    toPattern (NthChild nth) = _conP 'NthChild [_nthToPat nth]
    toPattern (NthLastChild nth) = _conP 'NthLastChild [_nthToPat nth]
    toPattern (NthLastOfType nth) = _conP 'NthLastOfType [_nthToPat nth]
    toPattern (NthOfType nth) = _conP 'NthOfType [_nthToPat nth]
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
    normalize pc = pc

instance ToCssSelector Negation where
    toCssSelector n = ":not("<> go n <> ")"
      where go (NTypeSelector t) = toCssSelector t
            go (NHash h) = toCssSelector h
            go (NClass c) = toCssSelector c
            go (NAttrib a) = toCssSelector a
            go (NPseudo p) = toCssSelector p
            go (NPseudoElement p) = toCssSelector p
    toSelectorGroup = toSelectorGroup . SNot
    specificity' (NTypeSelector t) = specificity' t
    specificity' (NHash h) = specificity' h
    specificity' (NClass c) = specificity' c
    specificity' (NAttrib a) = specificity' a
    specificity' (NPseudo p) = specificity' p
    specificity' (NPseudoElement p) = specificity' p
    toPattern (NTypeSelector t) = _conP 'NTypeSelector [toPattern t]
    toPattern (NHash h) = _conP 'NHash [toPattern h]
    toPattern (NClass c) = _conP 'NClass [toPattern c]
    toPattern (NAttrib a) = _conP 'NAttrib [toPattern a]
    toPattern (NPseudo p) = _conP 'NPseudo [toPattern p]
    toPattern (NPseudoElement p) = _conP 'NPseudoElement [toPattern p]

instance ToCssSelector PseudoElement where
    toCssSelector = pack . (':' :) . (':' :) . go
      where go After = "after"
            go Before = "before"
            go FirstLetter = "first-letter"
            go FirstLine = "first-line"
            go Marker = "marker"
            go Placeholder = "placeholder"
            go Selection = "selection"
    specificity' = const (SelectorSpecificity 0 0 1)
    toSelectorGroup = toSelectorGroup . (def :.::)
    toPattern = _constantP . go
      where go After = 'After
            go Before = 'Before
            go FirstLetter = 'FirstLetter
            go FirstLine = 'FirstLine
            go Marker = 'Marker
            go Placeholder = 'Placeholder
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
    def = Sequence def

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
  put (Sequence ss) = putWord8 0 >> put ss
  put (ss :.:: pe) = putWord8 1 >> put ss >> put pe
  get = do
    w <- getWord8
    case w of
      0 -> Sequence <$> get
      1 -> (:.::) <$> get <*> get
      _ -> fail "An error occured while deserializing a PseudoSelectorSequence."

instance Binary PseudoClass where
  put Active = putWord8 0
  put Checked = putWord8 1
  put Default = putWord8 2
  put Disabled = putWord8 3
  put Empty = putWord8 4
  put Enabled = putWord8 5
  put Focus = putWord8 6
  put Fullscreen = putWord8 7
  put Hover = putWord8 8
  put Indeterminate = putWord8 9
  put InRange = putWord8 10
  put Invalid = putWord8 11
  put (Lang l) = putWord8 12 >> put l
  put Link = putWord8 13
  put (NthChild nth) = putWord8 14 >> put nth
  put (NthLastChild nth) = putWord8 15 >> put nth
  put (NthLastOfType nth) = putWord8 16 >> put nth
  put (NthOfType nth) = putWord8 17 >> put nth
  put OnlyOfType = putWord8 18
  put OnlyChild = putWord8 19
  put Optional = putWord8 20
  put OutOfRange = putWord8 21
  put ReadOnly = putWord8 22
  put ReadWrite = putWord8 23
  put Required = putWord8 24
  put Root = putWord8 25
  put Target = putWord8 26
  put Valid = putWord8 27
  put Visited = putWord8 28

  get = do
    w <- getWord8
    case w of
      0 -> pure Active
      1 -> pure Checked
      2 -> pure Default
      3 -> pure Disabled
      4 -> pure Empty
      5 -> pure Enabled
      6 -> pure Focus
      7 -> pure Fullscreen
      8 -> pure Hover
      9 -> pure Indeterminate
      10 -> pure InRange
      11 -> pure Invalid
      12 -> Lang <$> get
      13 -> pure Link
      14 -> NthChild <$> get
      15 -> NthLastChild <$> get
      16 -> NthLastOfType <$> get
      17 -> NthOfType <$> get
      18 -> pure OnlyOfType
      19 -> pure OnlyChild
      20 -> pure Optional
      21 -> pure OutOfRange
      22 -> pure ReadOnly
      23 -> pure ReadWrite
      24 -> pure Required
      25 -> pure Root
      26 -> pure Target
      27 -> pure Valid
      28 -> pure Visited
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
  put (SNot n) = putWord8 4 >> put n
  get = do
    w <- getWord8
    case w of
      0 -> SHash <$> get
      1 -> SClass <$> get
      2 -> SAttrib <$> get
      3 -> SPseudo <$> get
      4 -> SNot <$> get
      _ -> fail "An error occurred when deserializing a SelectorFilter object."

instance Binary Negation where
  put (NTypeSelector t) = putWord8 0 >> put t
  put (NHash h) = putWord8 1 >> put h
  put (NClass c) = putWord8 2 >> put c
  put (NAttrib a) = putWord8 3 >> put a
  put (NPseudo p) = putWord8 4 >> put p
  put (NPseudoElement p) = putWord8 5 >> put p
  get = do
    w <- getWord8
    case w of
      0 -> NTypeSelector <$> get
      1 -> NHash <$> get
      2 -> NClass <$> get
      3 -> NAttrib <$> get
      4 -> NPseudo <$> get
      5 -> NPseudoElement <$> get
      _ -> fail "An error occurred when deserializing a Negation object."


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
#if MIN_VERSION_template_haskell(2,17,0)
_apply :: Quote m => Name -> [m Exp] -> m Exp
_apply = foldl appE . conE
#else
_apply :: Name -> [Q Exp] -> Q Exp
_apply = foldl appE . conE
#endif

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

instance Lift PseudoSelectorSequence where
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

instance Lift Nth where
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift Negation where
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

instance ToMarkup Negation where
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

instance ToJavascript Negation where
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

instance ToJSON Negation where
    toJSON = _cssToJson


-- Arbitrary instances
_arbitraryIdent :: Gen Text
_arbitraryIdent = pack <$> listOf1 arbitrary

_arbitraryLanguages :: [Text]
_arbitraryLanguages = ["af", "af-ZA", "ar", "ar-AE", "ar-BH", "ar-DZ", "ar-EG", "ar-IQ", "ar-JO", "ar-KW", "ar-LB", "ar-LY", "ar-MA", "ar-OM", "ar-QA", "ar-SA", "ar-SY", "ar-TN", "ar-YE", "az", "az-AZ", "az-AZ", "be", "be-BY", "bg", "bg-BG", "bs-BA", "ca", "ca-ES", "cs", "cs-CZ", "cy", "cy-GB", "da", "da-DK", "de", "de-AT", "de-CH", "de-DE", "de-LI", "de-LU", "dv", "dv-MV", "el", "el-GR", "en", "en-AU", "en-BZ", "en-CA", "en-CB", "en-GB", "en-IE", "en-JM", "en-NZ", "en-PH", "en-TT", "en-US", "en-ZA", "en-ZW", "eo", "es", "es-AR", "es-BO", "es-CL", "es-CO", "es-CR", "es-DO", "es-EC", "es-ES", "es-ES", "es-GT", "es-HN", "es-MX", "es-NI", "es-PA", "es-PE", "es-PR", "es-PY", "es-SV", "es-UY", "es-VE", "et", "et-EE", "eu", "eu-ES", "fa", "fa-IR", "fi", "fi-FI", "fo", "fo-FO", "fr", "fr-BE", "fr-CA", "fr-CH", "fr-FR", "fr-LU", "fr-MC", "gl", "gl-ES", "gu", "gu-IN", "he", "he-IL", "hi", "hi-IN", "hr", "hr-BA", "hr-HR", "hu", "hu-HU", "hy", "hy-AM", "id", "id-ID", "is", "is-IS", "it", "it-CH", "it-IT", "ja", "ja-JP", "ka", "ka-GE", "kk", "kk-KZ", "kn", "kn-IN", "ko", "ko-KR", "kok", "kok-IN", "ky", "ky-KG", "lt", "lt-LT", "lv", "lv-LV", "mi", "mi-NZ", "mk", "mk-MK", "mn", "mn-MN", "mr", "mr-IN", "ms", "ms-BN", "ms-MY", "mt", "mt-MT", "nb", "nb-NO", "nl", "nl-BE", "nl-NL", "nn-NO", "ns", "ns-ZA", "pa", "pa-IN", "pl", "pl-PL", "ps", "ps-AR", "pt", "pt-BR", "pt-PT", "qu", "qu-BO", "qu-EC", "qu-PE", "ro", "ro-RO", "ru", "ru-RU", "sa", "sa-IN", "se", "se-FI", "se-FI", "se-FI", "se-NO", "se-NO", "se-NO", "se-SE", "se-SE", "se-SE", "sk", "sk-SK", "sl", "sl-SI", "sq", "sq-AL", "sr-BA", "sr-BA", "sr-SP", "sr-SP", "sv", "sv-FI", "sv-SE", "sw", "sw-KE", "syr", "syr-SY", "ta", "ta-IN", "te", "te-IN", "th", "th-TH", "tl", "tl-PH", "tn", "tn-ZA", "tr", "tr-TR", "tt", "tt-RU", "ts", "uk", "uk-UA", "ur", "ur-PK", "uz", "uz-UZ", "uz-UZ", "vi", "vi-VN", "xh", "xh-ZA", "zh", "zh-CN", "zh-HK", "zh-MO", "zh-SG", "zh-TW", "zu", "zu-ZA"]

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
    arbitrary = Nth . (1+) . abs <$> arbitrary <*> arbitrary
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
    arbitrary = frequency [(3, Sequence <$> arbitrary), (1, (:.::) <$> arbitrary <*> arbitrary)]
    shrink (Sequence ss) = Sequence <$> shrink ss
    shrink (ss :.:: pe) = Sequence ss : ((ss :.::) <$> shrink pe) ++ ((:.:: pe) <$> shrink ss)

instance Arbitrary SelectorCombinator where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary AttributeCombinator where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SelectorFilter where
    arbitrary = frequency [(4, SHash <$> arbitrary), (4, SClass <$> arbitrary), (4, SAttrib <$> arbitrary), (4, SPseudo <$> arbitrary), (1, SNot <$> arbitrary)]
    shrink (SHash x) = SHash <$> shrink x
    shrink (SClass x) = SClass <$> shrink x
    shrink (SAttrib x) = SAttrib <$> shrink x
    shrink (SPseudo x) = SPseudo <$> shrink x
    shrink (SNot x) = SNot <$> shrink x

instance Arbitrary Negation where
    arbitrary = oneof [NTypeSelector <$> arbitrary, NHash <$> arbitrary, NClass <$> arbitrary, NAttrib <$> arbitrary, NPseudo <$> arbitrary, NPseudoElement <$> arbitrary]
    shrink (NTypeSelector x) = NTypeSelector <$> shrink x
    shrink (NHash x) = NHash <$> shrink x
    shrink (NClass x) = NClass <$> shrink x
    shrink (NAttrib x) = NAttrib <$> shrink x
    shrink (NPseudo x) = NPseudo <$> shrink x
    shrink (NPseudoElement x) = NPseudoElement <$> shrink x

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
    arbitrary = oneof ((Lang <$> elements _arbitraryLanguages) : map pure [
        Active, Checked, Default, Disabled, Empty, Enabled, Focus, Fullscreen, Hover, Indeterminate, InRange, Invalid, Link, OnlyOfType, OnlyChild
      , Optional, OutOfRange, ReadOnly, ReadWrite, Required, Root, Target, Valid, Visited
      ] ++ map (<$> arbitrary) [NthChild, NthLastChild, NthLastOfType, NthOfType])

instance Arbitrary PseudoElement where
    arbitrary = arbitraryBoundedEnum
