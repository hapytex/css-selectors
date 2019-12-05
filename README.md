# css-selectors

A library for parsing, manipulating, and rendering css selectors (not css files,
just the selectors).

It has a *quasiquoter* to enable Haskell to validate the css selector at compile
time.

Currently the css grammar is implemented *without* the pseudo-classes,
pseudo-elements and negations. One can furthermore calculate the specificity of
a css-selector, and thus perform an analysis over what css-selector will take
precedence.
