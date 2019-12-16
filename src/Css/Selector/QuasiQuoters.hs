{-# LANGUAGE TemplateHaskellQuotes #-}

module Css.Selector.QuasiQuoters (
    csssel, parseCss
  ) where

import Data.Data(Data, cast)
import Data.Text(pack, unpack)

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec))
import Language.Haskell.TH.Syntax(Exp(AppE, VarE), Q, dataToExpQ, lift)

import Css.Selector.Core(SelectorGroup)
import Css.Selector.Lexer(alexScanTokens)
import Css.Selector.Parser(cssselector)

-- | Parse the string to a 'SelectorGroup'.
parseCss :: String -- ^ The string to be parsed to a 'SelectorGroup'
    -> SelectorGroup -- ^ The selectorgroup that is the equivalent of the given 'String'.
parseCss = cssselector . alexScanTokens

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ ((((AppE (VarE 'pack) <$>) . lift . unpack) <$>) . cast)

-- | A quasiquoter that can be used to construct a 'SelectorGroup' for the given
-- css selector. In case the css selector is invalid. A compiler error will be
-- thrown (at compile time).
csssel :: QuasiQuoter
csssel = QuasiQuoter {
    quoteExp = liftDataWithText . parseCss,
    -- TODO: maybe later generate pattern to match CSS selectors
    quotePat = error "This quasiquoter does not generate patterns.",
    quoteType = error "This quasiquoter does not generate a type.",
    quoteDec = error "This quasiquoter does not generate declarations."
  }
