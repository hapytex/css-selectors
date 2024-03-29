# css-selectors

[![Build Status of the package by GitHub actions](https://github.com/hapytex/css-selectors/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/css-selectors/actions/workflows/build-ci.yml)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/css-selectors/badge)](https://matrix.hackage.haskell.org/#/package/css-selectors)
[![Hackage version badge](https://img.shields.io/hackage/v/css-selectors.svg)](https://hackage.haskell.org/package/css-selectors)

A library for parsing, manipulating, and rendering css selectors (not css files,
just the selectors).

It has a *quasiquoter* to enable Haskell to validate the css selector at compile
time.

One can furthermore calculate the specificity of a css-selector, and thus
perform an analysis over what css-selector will take precedence.

The package documentation can be found on the [GitHub pages](https://hapytex.github.io/css-selectors/).

## Selector structure

A css selector has the following structure:

  1. a `SelectorGroup` is a group of one or more `Selector`s, these are
     *comma-separated*;
  2. A `Selector` is a custom linked list implementation where the "cons" (the
     `Combined` data constructor) contains a besides a reference to a
     `PseudoSelectorSequence` (head) and a `Selector` (tail), it specifies what
     `PseudoSelectorCombinator` is used. A `Selector` has at least one
     `PseudoSelectorSequence`, this is constructoed with the `Selector` data
     constructor;
  3. A `PseudoSelectorSequence` is a `Selector` sequence with an *optional* `PseudoElement`, that
     pseudo element is written at the end of the `PseudoSelectorSequence` when specified in the CSS
     selector;
  4. A `SelectorSequence` contains a `TypeSelector` (in case the `TypeSelector`
     is `Universal`, this does not need to be part of the css-selector
     expression); and a set of zero or more `SelectorFilter`s;
  5. A `SelectorFilter` is a `Hash`, a `Class`, `Attrib`, `PseudoClass`, or a `Negation`;
  6. Both a `TypeSelector` and an `AttributeName` have a namespace. A namespace
     can be any (`*`), empty, or a namespace (which should be a valid
     identifier);
  7. A `Hash` is a valid identifier prepended with a number sign (`#`);
  8. A `Class` is a valid identifier prepended with a dot (`.`);
  9. An `Attribute` can be an `Exist` object that imposes a constraint that the
     attribute should exist for the given tag, or an `Attrib` that specifies
     that the attribute exists, and that the value for this attribute satisfies
     a given constraint. This constraint is determined by the
     `AttributeCombinator` and the value of the `Attrib` object;
  10. A `Negation` is written in a css selector with `:not(…)`. It can contain a `TypeSelector`, `Hash`,
      `Class` or `PseudoClass`, it can *not* contain a nested `:not(…)`.
  11. A `PseudoClass` is an identifier after a single colon (`:`). Some pseudo classes
     are functions that are then called with a parameter. For the `:nth-child(…)`, `:nth-last-child(…)`,
     `:nth-last-of-type(…)`, and `:nth-of-type(…)` these functions take an `Nth` as parameter. This
     parameter specifies which childs will be selected. The `:lang(…)` pseudo class
     takes the name of a language, for example `en-US`;
  12. An `Nth` describes what childs are selected, for example `4n+2`, this is used as a function
      parameter for the `NthChild`, `NthLastChild`, `NthLastOfType` and `NthOfType` pseudo classes; and
  13. A `PseudoElement` is an optional item at the end of a `SelectorSequence`. Usually pseudo elements are
      written with two colons in front, for example `::before` and `::after`. For backwards compatibility,
      these can also be written as `:before` and `:after`.

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

## `Binary` and `Hashable` instances

The css-elements are all members of the `Binary` and `Hashable` typeclasses,
The `Binary` typeclass converts the css selector to a compact binary format.
This is *not* standard format. This is more to write a css-selector to a
binary format and back.

css-elements are an instance of `Hashable` as well, for example to use as
keys in a `HashMap`.

## `css-selectors` is not *safe* Haskell

There are not extensions that are used that make the library *itself*
unsafe, but it makes use of `aeson`, `blaze-markup`, etc. and the packages are
not safe. Hence this package is not *safe Haskell*.

## Future plans

We want to implement an extra quasiquoter with the ability to specify variables,
that can then be used in expressions, or in patterns.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/css-selectors).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

---

This package is dedicated in loving memory to my mother, *Veerle Dumon*
(1958-2019), in the hope that eventually it will be as stylish as she was.
