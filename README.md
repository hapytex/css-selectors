# css-selectors

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
 2. ...

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

## `ToMarkup`, `ToJSON` and `ToJavascript` instances

The types that are members of the `ToCssSelector` are members of the `ToMarkup`,
`ToJSON`, and `ToJavascript` type classes as well, such that we can conveniently
use these in blaze HTML and for example in *Hamlet*.

The `ToMarkup` instance will render the css selector as raw content. So if you
add this as an attribute, the css selector will appear, unescaped, in the
rendered page.

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

# Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/css-selectors).

You can contact the package maintainer on
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

---

This package is dedicated in loving memory to my mother, Veerle Dumon
(1958-2019), in the hope that eventually it will be as stylish as she was.
