{-|
Module      : Css3.Selector
Description : Css 3 selectors in Haskell.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to define css selectors by making use of a quasiquoter, and manipulating these css selectors.
-}
module Css3.Selector (
    module Css3.Selector.Core,
    module Css3.Selector.QuasiQuoters,
    module Css3.Selector.Utils
  ) where

import Css3.Selector.Core
import Css3.Selector.QuasiQuoters
import Css3.Selector.Utils
