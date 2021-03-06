{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Module      : Css3.Selector.QuasiQuoters
Description : A module that defines a quasiquoter to parse a string to a css selector.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines a quasiquoter to parse a string to a css selector.
-}
module Css3.Selector.QuasiQuoters (
    csssel, cssselFile, parseCss
  ) where

import Css3.Selector.Core(SelectorGroup, toPattern)
import Css3.Selector.Lexer(alexScanTokens)
import Css3.Selector.Parser(cssselector)

import Data.Data(Data, cast)
import Data.Text(pack, unpack)

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec), quoteFile)
import Language.Haskell.TH.Syntax(Exp(AppE, VarE), Q, Type(ConT), dataToExpQ, lift, reportWarning)

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
    quotePat = pure . toPattern . parseCss,
    quoteType = const (reportWarning "The type of the quasiquoter will always use the SelectorGroup type." >> pure (ConT ''SelectorGroup)),
    quoteDec = const (reportWarning "The use of this quasiquoter will not make any declarations." >> pure [])
  }

-- | A quasiquoter that takes the content from the file, and then runs the
-- content of that file as a 'csssel' quasiquote.
cssselFile :: QuasiQuoter
cssselFile = quoteFile csssel
