-- vim:ft=haskell
{
module Css3.Selector.Nth.Parser where

import Css3.Selector.Core(Nth(Nth));
import Css3.Selector.Nth.Lexer(AlexPosn(..), Token(..), TokenLoc(..))

import Data.List.NonEmpty(NonEmpty((:|)), (<|))

import Data.Default(def)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.Text(pack)
}

%name nth
%tokentype { TokenLoc }
%error { happyError }

%token
    nth    { TokenLoc (TNth $$) _ _ }
    pm     { TokenLoc (TPM $$) _ _ }
    int    { TokenLoc (TInt $$) _ _}
    n      { TokenLoc TN _ _ }
    ' '    { TokenLoc TSpace _ _ }

%%

Nth
    : nth                                        { $1 }
    | pmint                                      { Nth 0 $1 }
    | pmint n tail                               { $3 (Nth $1) }
    | pm n tail                                  { $3 (Nth ($1 1)) }
    ;

pmint
    : int                                         { $1 }
    | pm int                                      { $1 $2 }
    ;

optspace
    : ' '                                         { ' ' }
    |                                             { ' ' }
    ;

tail
    :                                             { $ 0 }
    | optspace pm optspace int                    { $ ($2 $4) }
    ;

{

happyError :: [TokenLoc] -> a
happyError [] = error "Unexpected end of string when parsing an nth object."

}
