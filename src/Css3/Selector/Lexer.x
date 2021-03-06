--vim:ft=haskell
{
module Css.Selector.Lexer(AlexPosn(..), Token(..), TokenLoc(..), alexScanTokens) where

import Data.Decimal(Decimal)
import Css.Selector.Utils(readCssString, readIdentifier)
}

%wrapper "posn"

$nonascii = [^\0-\xff]
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
  @wo "="  @wo     { constoken TEqual }
  @wo "~=" @wo     { constoken TIncludes }
  @wo "|=" @wo     { constoken TDashMatch }
  @wo "^=" @wo     { constoken TPrefixMatch }
  @wo "$=" @wo     { constoken TSuffixMatch }
  @wo "*=" @wo     { constoken TSubstringMatch }
  @wo ","  @wo     { constoken Comma }
  "."              { constoken Dot }
  "|"              { constoken Pipe }
  "*"              { constoken Asterisk }
  @ident           { tokenize (Ident . readIdentifier) }
  @string          { tokenize (String . readCssString) }
  "#" @name        { tokenize (THash . readIdentifier . drop 1) }
  @float           { tokenize (Decimal . read) }
  @int             { tokenize (Integer . read) }
  @wo "+" @wo      { constoken Plus }
  @wo ">" @wo      { constoken Greater }
  @wo $tl @wo      { constoken Tilde }
  "[" @wo          { constoken BOpen }
  @wo "]"          { constoken BClose }
  $w @wo           { constoken Space }
  @cmo $nostar* \*+ ($nostars $nostar* \*+)* @cmc      ;

{
data TokenLoc = TokenLoc { token :: Token, original :: String, location :: AlexPosn }

tokenize :: (String -> Token) -> AlexPosn -> String -> TokenLoc
tokenize = flip . (>>= TokenLoc)

constoken :: Token -> AlexPosn -> String -> TokenLoc
constoken = tokenize . const

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
