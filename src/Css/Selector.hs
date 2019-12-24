{-|
Module      : Css.Selector
Description : Css selectors in Haskell.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to define css selectors by making use of a quasiquoter, and manipulating these css-selectors.
-}
module Css.Selector (
    module Css.Selector.Core,
    module Css.Selector.QuasiQuoters,
    module Css.Selector.Utils
  ) where

import Css.Selector.Core
import Css.Selector.QuasiQuoters
import Css.Selector.Utils
