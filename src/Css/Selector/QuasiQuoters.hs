module Css.Selector.QuasiQuoters where

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec))
import Css.Selector.Core(SelectorGroup)
import Css.Selector.Lexer(alexScanTokens)
import Css.Selector.Parser(cssselector)

parseCss :: String -> SelectorGroup
parseCss = cssselector . alexScanTokens

csssel :: QuasiQuoter
csssel = QuasiQuoter {
    quoteExp = undefined,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
  }
