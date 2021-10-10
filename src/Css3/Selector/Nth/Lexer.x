--vim:ft=haskell
{
{-# LANGUAGE PatternSynonyms #-}

module Css3.Selector.Nth.Lexer(AlexPosn(..), Token(..), TokenLoc(..), alexScanTokens) where

import Css3.Selector.Core(Nth, PseudoClass(NthChild, NthLastChild, NthLastOfType, NthOfType), pattern Even, pattern Odd)
}

%wrapper "posn"

$nonascii = [^\0-\xff]
$w        = [\ \t\r\n\f]
$nostar   = [^\*]
$nostars  = [^\/\*]
$tl       = [\~]
$pm       = [\-\+]

@nl       = \r|\n|\r\n|\f
@unicode  = \\[0-9a-fA-F]{1,6}(\r\n|[ \n\r\t\f])?
@escape   = @unicode | \\[^\n\r\f0-9a-fA-F]
@wo = $w*
@nonaesc = $nonascii | @escape
@nmstart = [_a-zA-Z] | @nonaesc
@nmchar  = [_\-a-zA-Z0-9] | @nonaesc
@ident   = [\-]? @nmstart @nmchar*
@name    = @nmchar+
@int     = [0-9]+
@float   = [0-9]*[.][0-9]+
@string1 = \'([^\n\r\f\\\'] | \\@nl | @nonaesc )*\'   -- strings with single quote
@string2 = \"([^\n\r\f\\\"] | \\@nl | @nonaesc )*\"   -- strings with double quotes
@string  = @string1 | @string2

@d       = d|D|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
@e       = e|E|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
@n       = n|N|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
@o       = o|O|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
@t       = t|T|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
@v       = v|V|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\v
@cmo     = \/\*
@cmc     = \*\/
@psc     = [:]
@pse     = [:][:]
@psb     = [:][:]?
@even    = @e@v@e@n
@odd     = @o@d@d
@nth1    = $pm@int
@nth2    = $pm?@int?@n($pm@wo@int)?
@nth     = @wo(@even|@odd|@nth2)@wo
@nthb    = @wo\(@nth\)

tokens :-
  @wo@even@wo             { constoken (TNth Even) }
  @wo@odd@wo              { constoken (TNth Odd) }
  @psc "nth-child"        { constoken (TPseudo NthChild) }
  @psc "nth-last-child"   { constoken (TPseudo NthLastChild) }
  @psc "nth-last-of-type" { constoken (TPseudo NthLastOfType) }
  @psc "nth-of-type"      { constoken (TPseudo NthOfType) }
  @int                    { tokenize (TInt . read) }
  @n                      { constoken TN }
  "+"                     { constoken (TPM id) }
  "-"                     { constoken (TPM negate) }
  $w@wo                   { constoken TSpace }

{
data TokenLoc = TokenLoc { token :: Token, original :: String, location :: AlexPosn }

tokenize :: (String -> Token) -> AlexPosn -> String -> TokenLoc
tokenize = flip . (>>= TokenLoc)

constoken :: Token -> AlexPosn -> String -> TokenLoc
constoken = tokenize . const

-- The token type:
data Token
  = TPseudo (Nth -> PseudoClass)
  | TN
  | TNth Nth
  | TSpace
  | TInt Int
  | TPM (Int -> Int)
}
