{-# LANGUAGE OverloadedStrings #-}

module Css.Selector.Arbitrary where

import Css.Selector.Core
import Data.Text(Text)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(Gen, frequency)


arbitraryText :: Gen Text
arbitraryText = return "a"

instance Arbitrary Namespace where
    arbitrary = frequency [(1, return NAny), (3, Namespace <$> arbitraryText)]
