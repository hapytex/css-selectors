{-|
Module      : Css.Selector
Description : Css 3 selectors in Haskell.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module for backwards compatibility that re-exports 'Css3.Selector'. This module is deprecated and eventually will be removed.
-}
module Css.Selector {-# DEPRECATED "Use Css3.Selector instead" #-} (
    module Css3.Selector,
  ) where

import Css3.Selector
