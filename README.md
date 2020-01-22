# css-selectors

[![Build Status of the package by Travis](https://travis-ci.com/hapytex/css-selectors.svg?branch=master)](https://travis-ci.com/hapytex/css-selectors)

A library for parsing, manipulating, and rendering css selectors (not css files,
just the selectors).

It has a *quasiquoter* to enable Haskell to validate the css selector at compile
time.

Currently the css grammar is implemented *without* the pseudo-classes,
pseudo-elements and negations. One can furthermore calculate the specificity of
a css-selector, and thus perform an analysis over what css-selector will take
precedence.

## Selector structure

A css selector has the following structure:

 1. a `SelectorGroup` is a group of one or more `Selector`s, these are
    *comma-separated*;
 2. A `Selector` is a custom linked list implementation where the "cons" (the
    `Combined` data constructor) contains a besides a reference to a
    `SelectorSequence` (head) and a `Selector` (tail), it specifies what
    `SelectorCombinator` is used. A `Selector` has at least one
    `SelectorSequence`, this is constructoed with the `Selector` data
    constructor;
  3. A `SelectorSequence` contains a `TypeSelector` (in case the `TypeSelector`
     is `Universal`, this does not need to be part of the css-selector
     expression); and a set of zero or more `SelectorFilter`s;
  4. A `SelectorFilter` is a `Hash`, a `Class`, or an `Attrib`;
  5. Both a `TypeSelector` and an `AttributeName` have a namespace. A namespace
     can be any (`*`), empty, or a namespace (which should be a valid
     identifier);
  6. A `Hash` is a valid identifier prepended with a number sign (`#`);
  7. A `Class` is a valid identifier prepended with a dot (`.`);
  8. An `Attribute` can be an `Exist` object that imposes a constraint that the
     attribute should exist for the given tag, or an `Attrib` that specifies
     that the attribute exists, and that the value for this attribute satisfies
     a given constraint. This constraint is determined by the
     `AttributeCombinator` and the value of the `Attrib` object.

## Quasiquoter

The main use of this package is a *quasiquoter*, that can be used both for
*expressions* and *patterns*. We thus can construct a `SelectorGroup` in an
expression with:

```haskell
myCssSelector :: SelectorGroup
myCssSelector = [csssel|* html .pun .inbox, * html .pun #bdrdmain, * html .pun .infldset|]
```

A less common use case is using the quasiquoter in a pattern to check if a given
`SelectorGroup` matches *exactly* with a given css selector. For example:

```haskell
isMyCssSelector :: SelectorGroup -> Bool
isMyCssSelector [csssel|* html .pun .unbox|] = True
isMyCssSelector _ = False
```

The quasiquoter can be used in a type signature as well, but will always,
regardless of the content, return the type for `SelectorGroup`. If you use the
quasiquoter as a declaration, it will simply not generate any declarations. It
will raise a warning (not an error) about this.

Perhaps in the (far) future, we will make more sensical implementations for the
type and declaration part of the quasiquoter.

Note that you need to enable the `-XQuasiQuotes` pragma when you compile.

## Selector normalization

One can turn equivalent css selectors in a "normalized" form. This is done by
sorting the `Selector`s in a `Selector` group, and sorting the `SelectorFilter`s
of a certain `SelectorSequence`.

The order is determined by the default instances of `Ord` of the sequences. This
is thus not an "inherent" ordering of the css selector, but just an order that
the program constructed to convert multiple css selectors that are equivalent
same to a normal form in which these are equal.

We here do *not* optimize the css selector, for example by removing duplicate
filters, since that can have impact on the specificity of the selector.

## Selector specificity

The specificity of a selector is defined by three numbers *a*, *b* and *c*.
Later, one calculates the specificity level with *100 a + 10 b + c*. The higher
the specificity level, the more it takes precedence. If there are thus two
selectors and the former selector has *14* as specificity level, and the latter
has *42* as specificity level, then rules defined in the latter, will "overrule"
the rules defined in the former, given these rules "clash".

One can calculate the specificity of a item with as type a member of the `ToCssSelector`
class with:

```haskell
specificity :: ToCssSelector a => a -> Int
```

or you can obtain a more detailed result with:

```haskell
specificity' :: ToCssSelector a => a -> SelectorSpecificity
```

## `ToMarkup`, `ToJSON`, and `ToJavascript` instances

The types that are members of the `ToCssSelector` are members of the `ToMarkup`,
`ToJSON`, and `ToJavascript` type classes as well, such that we can conveniently
use these in blaze HTML and for example in *Hamlet*.

The `ToMarkup` instance will render the css selector as raw content. So if you
add this as an attribute, the css selector will appear, unescaped, in the
rendered page. Note that it will be escaped, so `foo > bar` will be generated as
`foo &gt; bar`.

The `ToJSON` instance will convert the given object in a JSON string that
contains the css selector.

The `ToJavascript` will render the content to a javascript *string*. So if you
use this in hamlet, you generate a string that contains the css-selector. This
is often useful, since javascript itself has no syntax for css selectors, and
often strings are used to represent these.

## `Arbitrary` css selectors

One can generate arbitrary CSS selectors (and their subcomponents). It is
however not advisable to use this for anything other than for validation
purposes (like with `QuickCheck`).

# `css-selectors` is not *safe* Haskell

There are not extensions that are used that make the library *itself*
unsafe, but it makes use of `aeson`, `blaze-markup`, etc. and the packages are
not safe. Hence this package is not *safe Haskell*.

# Future plans

We want to implement an extra quasiquoter with the ability to specify variables,
that can then be used in expressions, or in patterns.

# Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/css-selectors).

You can contact the package maintainer on
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

---

This package is dedicated in loving memory to my mother, Veerle Dumon
(1958-2019), in the hope that eventually it will be as stylish as she was.
