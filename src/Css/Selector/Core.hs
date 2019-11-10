{-# LANGUAGE OverloadedStrings #-}

module Css.Selector.Core where

import Data.List.NonEmpty(NonEmpty, toList)
import Data.String(IsString(fromString))
import Data.Text(Text, cons, intercalate, pack)

data SelectorSpecificity = SelectorSpecificity { a :: Int, b :: Int, c :: Int }

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
      SelectorFilter SelectorFilter
    | SimpleSelector SimpleSelector
    | Filter SelectorSequence SelectorFilter

addFilters :: SelectorSequence -> [SelectorFilter] -> SelectorSequence
addFilters = foldl Filter


data SimpleSelector = Universal
                    | Type TypeSelector

data SelectorFilter =
      SHash Hash
    | SClass Class


instance Semigroup SelectorGroup where
    (SelectorGroup g1) <> (SelectorGroup g2) = SelectorGroup (g1 <> g2)

data Namespace = NAny | Namespace Text
data TypeSelector = TypeSelector { selectorNameSpace :: Namespace, elementName :: ElementName }
newtype ElementName = ElementName { unElementName :: Text }
newtype Class = Class { unClass :: Text }
newtype Hash = Hash { unHash :: Text }

instance IsString Class where
    fromString = Class . pack

instance ToCssSelector SelectorGroup where
    toCssSelector (SelectorGroup g) = intercalate " , " (map toCssSelector (toList g))
    toSelectorGroup = id
    specificity' (SelectorGroup g) = foldMap specificity' g

instance ToCssSelector Class where
    toCssSelector = cons '.' . unClass
    toSelectorGroup = toSelectorGroup . SClass
    specificity' = const (SelectorSpecificity 0 1 0)

instance IsString Hash where
    fromString = Hash . pack

instance ToCssSelector Hash where
    toCssSelector = cons '#' . unHash
    toSelectorGroup = toSelectorGroup . SHash
    specificity' = const (SelectorSpecificity 1 0 0)

instance ToCssSelector Namespace where
    toCssSelector NAny = "*"
    toCssSelector (Namespace t) = t

instance ToCssSelector SelectorSequence where
    toCssSelector (SimpleSelector s) = toCssSelector s
    toCssSelector (SelectorFilter f) = toCssSelector f
    toCssSelector (Filter s f) = toCssSelector s <> toCssSelector f
    specificity' (SimpleSelector s) = specificity' s
    specificity' (SelectorFilter f) = specificity' f
    specificity' (Filter s f) = specificity' s <> specificity' f

instance ToCssSelector TypeSelector where
    toCssSelector (TypeSelector NAny e) = toCssSelector e
    toCssSelector (TypeSelector n e) = toCssSelector n <> "|" <> toCssSelector e
    specificity' = const (SelectorSpecificity 0 0 1)

instance ToCssSelector ElementName where
    toCssSelector (ElementName e) = e

instance ToCssSelector SimpleSelector where
    toCssSelector Universal = "*"
    toCssSelector (Type t) = toCssSelector t
    toSelectorGroup = toSelectorGroup . SimpleSelector
    specificity' Universal = mempty
    specificity' (Type t) = specificity' t

instance ToCssSelector SelectorFilter where
    toCssSelector (SHash h) = toCssSelector h
    toCssSelector (SClass c) = toCssSelector c
    toSelectorGroup = toSelectorGroup . SelectorFilter
    specificity' (SHash h) = specificity' h
    specificity' (SClass c) = specificity' c

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

{-
data SelectorModifier
    = MClass Text
    | MId Text
    | 

data AttributeFilter
    = AExists Attribute
    | AEquals Attribute Value
    | AStarts Attribute Value
    | AHyphenStarts Attribute Value
    | 

type Attribute = Text
type Value = Texta

(.=) :: Attribute -> a
-}
