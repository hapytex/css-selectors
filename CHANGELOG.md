# `css-selectors` Changelog

For a full list of changes, see the history on [GitHub](https://github.com/hapytex/css-selectors).

## Version 0.5.0.0

Added pseudo-elements and pseudo-classes. The `Binary` instances are updated and *not* backedwards compatible.

## Version 0.4.0.0

All css items are now a member of the `Generic` and the `Hashable` typeclass.

## Version 0.3.0.0

The css items are now an instance of `Binary` and can be serialized and deserialized; and compressed. One
can use this to store the css selector to a file. The format is *not* a standardized one: it is just defined
to store the selector effectively.

## Version 0.2.1.0

Move from modules from `Css.*` to `Css3.*`, and implementations of `shrink` for the css selectors data types.

## Version 0.2.0.0

The first version that is deployed on Hackage.
