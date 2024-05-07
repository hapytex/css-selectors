-- |
-- Module      : Css4.Selector
-- Description : Css 3 selectors in Haskell.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A module to define css selectors by making use of a quasiquoter, and manipulating these css selectors.
module Css4.Selector
  ( module Css4.Selector.Core,
    module Css4.Selector.QuasiQuoters,
    module Css4.Selector.Utils,
  )
where

import Css4.Selector.Core
import Css4.Selector.QuasiQuoters
import Css4.Selector.Utils
