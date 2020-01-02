--vim:ft=haskell
{
module Css.Selector.Lexer(Token(..), alexScanTokens) where

import Data.Decimal(Decimal)
import Css.Selector.Utils(readCssString)
}

%wrapper "posn"

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


tokens :-
  @wo "="  @wo     { tokenize (const TEqual) }
  @wo "~=" @wo     { tokenize (const TIncludes) }
  @wo "|=" @wo     { tokenize (const TDashMatch) }
  @wo "^=" @wo     { tokenize (const TPrefixMatch) }
  @wo "$=" @wo     { tokenize (const TSuffixMatch) }
  @wo "*=" @wo     { tokenize (const TSubstringMatch) }
  @wo ","  @wo     { tokenize (const Comma) }
  "."              { tokenize (const Dot) }
  "|"              { tokenize (const Pipe) }
  "*"              { tokenize (const Asterisk) }
  @ident           { tokenize Ident }
  @string          { tokenize (String . readCssString) }
  "#" @name        { tokenize (THash . drop 1) }
  @float           { tokenize (Decimal . read) }
  @int             { tokenize (Integer . read) }
  @wo "+" @wo      { tokenize (const Plus) }
  @wo ">" @wo      { tokenize (const Greater) }
  @wo $tl @wo      { tokenize (const Tilde) }
  "[" @wo          { tokenize (const BOpen) }
  @wo "]"          { tokenize (const BClose) }
  $w @wo           { tokenize (const Space) }
  @cmo $nostar* \*+ ($nostars $nostar* \*+)* @cmc      ;

{
data TokenLoc = TokenLoc { token :: Token, location :: AlexPosn }

tokenize :: (String -> Token) -> AlexPosn -> String -> TokenLoc
tokenize = flip . (TokenLoc .)

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
