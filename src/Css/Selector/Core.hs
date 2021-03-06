{-|
Module      : Css.Selector.Core
Description : A module where we define the tree of types to represent and maniplate a css selector.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module for backwards compatibility that re-exports 'Css3.Selector.Core'. This module is deprecated and eventually will be removed.
-}
module Css.Selector.Core {-# DEPRECATED "Use Css3.Selector.Core instead" #-} (
    module Css3.Selector.Core
  ) where

import Css3.Selector.Core
