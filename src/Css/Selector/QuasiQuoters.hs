{-|
Module      : Css.Selector.QuasiQuoters
Description : A module that defines a quasiquoter to parse a string to a css selector.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module for backwards compatibility that re-exports 'Css3.Selector.QuasiQuoters'. This module is deprecated and eventually will be removed.
-}
module Css.Selector.QuasiQuoters {-# DEPRECATED "Use Css3.Selector.QuasiQuoters instead" #-} (
    module Css3.Selector.QuasiQuoters
  ) where

import Css3.Selector.QuasiQuoters
