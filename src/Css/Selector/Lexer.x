--vim:ft=haskell
{
module Css.Selector.Lexer(Token(..), alexScanTokens) where

import Data.Decimal(Decimal)
import Css.Selector.Utils(readCssString)
}

%wrapper "basic"

$nonascii = [^\0-\177]
$w        = [\ \t\r\n\f]
$nostar   = [^\*]
$nostars  = [^\/\*]
$tl       = [\~]

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
@string1 = \'([^\n\r\f\\"] | \\@nl | @nonaesc )*\'   -- strings with single quote
@string2 = \"([^\n\r\f\\'] | \\@nl | @nonaesc )*\"   -- strings with double quotes
@string  = @string1 | @string2

@d       = d|D|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
@e       = e|E|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
@n       = n|N|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
@o       = o|O|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
@t       = t|T|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
@v       = v|V|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\v
@cmo     = \/\*
@cmc     = \*\/


tokens :-
  @wo "="  @wo     { const TEqual }
  @wo "~=" @wo     { const TIncludes }
  @wo "|=" @wo     { const TDashMatch }
  @wo "^=" @wo     { const TPrefixMatch }
  @wo "$=" @wo     { const TSuffixMatch }
  @wo "*=" @wo     { const TSubstringMatch }
  @wo ","          { const Comma }
  "."              { const Dot }
  "|"              { const Pipe }
  "*"              { const Asterisk }
  @ident           { Ident }
  @string          { String . readCssString }
  "#" @name        { THash }
  @float           { Decimal . read }
  @int             { Integer . read }
  @wo "+"          { const Plus }
  @wo ">"          { const Greater }
  @wo $tl          { const Tilde }
  "[" @wo          { const BOpen }
  @wo "]"          { const BClose }
  $w @wo           { const Space }
  @cmo $nostar* \*+ ($nostars $nostar* \*+)* @cmc      ;

{
-- The token type:
data Token =
      TIncludes
    | TEqual
    | TDashMatch
    | TPrefixMatch
    | TSuffixMatch
    | TSubstringMatch
    | Ident String
    | String String
    | THash String
    | Decimal Decimal
    | Integer Integer
    | Comma
    | Plus
    | Greater
    | Tilde
    | Dot
    | Pipe
    | Asterisk
    | Space
    | BOpen
    | BClose
    deriving (Eq,Show)
}
