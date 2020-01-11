{-# LANGUAGE ScopedTypeVariables, TemplateHaskellQuotes #-}

{-|
Module      : Css.Selector.QuasiQuoters
Description : A module that defines a quasiquoter to parse a string to a css selector.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines a quasiquoter to parse a string to a css selector.
-}
module Css.Selector.QuasiQuoters (
    csssel, parseCss
  ) where

import Css.Selector.Core(SelectorGroup(..), toPattern)
import Css.Selector.Lexer(alexScanTokens)
import Css.Selector.Parser(cssselector)

import Data.Data(Data, cast)
import Data.Text(pack, unpack)

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec))
import Language.Haskell.TH.Syntax(Exp(AppE, VarE), Q, dataToExpQ, lift)

-- | Parse the string to a 'SelectorGroup'.
parseCss :: String -- ^ The string to be parsed to a 'SelectorGroup'
    -> SelectorGroup -- ^ The selectorgroup that is the equivalent of the given 'String'.
parseCss = cssselector . alexScanTokens . filter ('\r' /=)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ ((((AppE (VarE 'pack) <$>) . lift . unpack) <$>) . cast)

-- | A quasiquoter that can be used to construct a 'SelectorGroup' for the given
-- css selector. In case the css selector is invalid. A compiler error will be
-- thrown (at compile time).
csssel :: QuasiQuoter
csssel = QuasiQuoter {
    quoteExp = liftDataWithText . parseCss,
    quotePat = return . toPattern . parseCss,
    quoteType = fail "This quasiquoter does not generate a type.",
    quoteDec = fail "This quasiquoter does not generate declarations."
  }
