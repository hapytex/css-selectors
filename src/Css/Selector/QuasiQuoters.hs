{-# LANGUAGE TemplateHaskellQuotes #-}

module Css.Selector.QuasiQuoters (
    csssel, parseCss
  ) where

import Data.Data(Data, cast)
import Data.Text(Text, pack, unpack)

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec))
import Language.Haskell.TH.Syntax(Exp(AppE, VarE), Q, dataToExpQ, lift)

import Css.Selector.Core(SelectorGroup)
import Css.Selector.Lexer(alexScanTokens)
import Css.Selector.Parser(cssselector)

parseCss :: String -> SelectorGroup
parseCss = cssselector . alexScanTokens

liftText :: Text -> Q Exp
liftText = (AppE (VarE 'pack) <$>) . lift . unpack

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ ((liftText <$>) . cast)

csssel :: QuasiQuoter
csssel = QuasiQuoter {
    quoteExp = liftDataWithText . parseCss,
    -- TODO: maybe later generate pattern to match CSS selectors
    quotePat = error "This quasiquoter does not generate patterns.",
    quoteType = error "This quasiquoter does not generate a type.",
    quoteDec = error "This quasiquoter does not generate declarations."
  }
