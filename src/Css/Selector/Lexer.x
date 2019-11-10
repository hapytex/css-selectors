--vim:ft=haskell
{
module Css.Selector.Lexer(Token(..), alexScanTokens) where

import Data.Decimal(Decimal)
}

%wrapper "basic"

$nonascii = [^\0-\177]
$w        = [ \t\r\n\f]

@nl       = \r|\n|\r\n|\f
@unicode  = \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
@escape   = @unicode | \\[^\n\r\f0-9a-f]

@wopt = $w*
@nonaesc = $nonascii | @escape
@nmstart = [_a-z] | @nonaesc
@nmchar  = [_\-a-z0-9] | @nonaesc
@ident   = [\-]? @nmstart @nmchar*
@name    = @nmchar+
@int     = [0-9]+
@float   = [0-9]*[.][0-9]+
@string1 = \'([^\n\r\f\\"] | \\@nl | @nonaesc )*\'   -- strings with single quote
@string2 = \"([^\n\r\f\\'] | \\@nl | @nonaesc )*\"   -- strings with double quotes
@string = @string1 | @string2


tokens :-
  "~="      { const Includes }
  "|="      { const DashMatch }
  "^="      { const PrefixMatch }
  "$="      { const SuffixMatch }
  "*="      { const SubstringMatch }
  $w* ","   { const Comma }
  "."       { const Dot }
  "|"       { const Pipe }
  "*"       { const Asterisk }
  @ident    { Ident }
  @string   { String }
  "#" @name { THash }
  $w+       { const Space }
  @float    { Decimal . read }
  @int      { Integer . read }
  $w* "+"   { const Plus }
  $w* ">"   { const Greater }
  $w* "~"   { const Tilde }
  "["       { const BOpen }
  "]"       { const BClose }

{
-- The token type:
data Token
    = Includes
    | DashMatch
    | PrefixMatch
    | SuffixMatch
    | SubstringMatch
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
