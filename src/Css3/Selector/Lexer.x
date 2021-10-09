--vim:ft=haskell
{
{-# LANGUAGE PatternSynonyms #-}

module Css3.Selector.Lexer(AlexPosn(..), Token(..), TokenLoc(..), alexScanTokens) where

import Data.Decimal(Decimal)
import Css3.Selector.Utils(readCssString, readIdentifier)
import Css3.Selector.Core(
    PseudoElement(After, Before, FirstLetter, FirstLine, Marker, Selection)
  , PseudoClass(
        Active, Checked, Disabled, Empty, Enabled, Focus, Hover, InRange, Invalid, Link
      , OnlyOfType, OnlyChild, Optional, OutOfRange, ReadOnly, ReadWrite, Required, Root, Target, Valid, Visited
      )
  , Nth(Nth), pattern Even, pattern Odd
  , pattern FirstChild, pattern FirstOfType, pattern LastChild, pattern LastOfType
  , PseudoClass(..)
  )
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
  @wo "="  @wo         { constoken TEqual }
  @wo "~=" @wo         { constoken TIncludes }
  @wo "|=" @wo         { constoken TDashMatch }
  @wo "^=" @wo         { constoken TPrefixMatch }
  @wo "$=" @wo         { constoken TSuffixMatch }
  @wo "*=" @wo         { constoken TSubstringMatch }
  @wo ","  @wo         { constoken Comma }
  "."                  { constoken Dot }
  "|"                  { constoken Pipe }
  "*"                  { constoken Asterisk }
  @ident               { tokenize (Ident . readIdentifier) }
  @string              { tokenize (String . readCssString) }
  "#" @name            { tokenize (THash . readIdentifier . drop 1) }
  @float               { tokenize (Decimal . read) }
  @int                 { tokenize (Integer . read) }
  @wo "+" @wo          { constoken Plus }
  @wo ">" @wo          { constoken Greater }
  @wo $tl @wo          { constoken Tilde }
  "[" @wo              { constoken BOpen }
  @wo "]"              { constoken BClose }
  @psb "after"         { constoken (PseudoElement After) }
  @psb "before"        { constoken (PseudoElement Before) }
  @psb "first-letter"  { constoken (PseudoElement FirstLetter) }
  @psb "first-line"    { constoken (PseudoElement FirstLine) }
  @pse "marker"        { constoken (PseudoElement Marker) }
  @pse "selection"     { constoken (PseudoElement Selection) }
  @psc "active"        { constoken (PseudoClass Active) }
  @psc "checked"       { constoken (PseudoClass Checked) }
  @psc "disabled"      { constoken (PseudoClass Disabled) }
  @psc "empty"         { constoken (PseudoClass Empty) }
  @psc "enabled"       { constoken (PseudoClass Enabled) }
  @psc "first-child"   { constoken (PseudoClass FirstChild) }
  @psc "first-of-type" { constoken (PseudoClass FirstOfType) }
  @psc "focus"         { constoken (PseudoClass Focus) }
  @psc "hover"         { constoken (PseudoClass Hover) }
  @psc "in-range"      { constoken (PseudoClass InRange) }
  @psc "invalid"       { constoken (PseudoClass Invalid) }
  @psc "last-child"    { constoken (PseudoClass LastChild) }
  @psc "last-of-type"  { constoken (PseudoClass LastOfType) }
  @psc "link"          { constoken (PseudoClass Link) }
  @psc "nth-child" @nthb { tokenize (PseudoClass . NthChild . parseNth) }
  @psc "nth-last-child" @nthb { tokenize (PseudoClass . NthLastChild . parseNth) }
  @psc "nth-last-of-type" @nthb { tokenize (PseudoClass . NthLastOfType . parseNth) }
  @psc "nth-of-type" @nthb { tokenize (PseudoClass . NthOfType . parseNth) }
  @psc "only-of-type"  { constoken (PseudoClass OnlyOfType) }
  @psc "only-child"    { constoken (PseudoClass OnlyChild) }
  @psc "optional"      { constoken (PseudoClass Optional) }
  @psc "out-of-range"  { constoken (PseudoClass OutOfRange) }
  @psc "read-only"     { constoken (PseudoClass ReadOnly) }
  @psc "read-write"    { constoken (PseudoClass ReadWrite) }
  @psc "required"      { constoken (PseudoClass Required) }
  @psc "root"          { constoken (PseudoClass Root) }
  @psc "target"        { constoken (PseudoClass Target) }
  @psc "valid"         { constoken (PseudoClass Valid) }
  @psc "visited"       { constoken (PseudoClass Visited) }
  $w @wo               { constoken Space }
  @cmo $nostar* \*+ ($nostars $nostar* \*+)* @cmc      ;

{
data TokenLoc = TokenLoc { token :: Token, original :: String, location :: AlexPosn }

parseNth :: String -> Nth
parseNth ":nth-child(even)" = Even
parseNth ":nth-child(odd)" = Odd
parseNth ":nth-last-child(even)" = Even
parseNth ":nth-last-child(odd)" = Odd
parseNth ":nth-last-of-type(even)" = Even
parseNth ":nth-last-of-type(odd)" = Odd
parseNth ":nth-of-type(even)" = Even
parseNth ":nth-of-type(odd)" = Odd
parseNth x = error ("was \"" ++ x ++ "\"")

tokenize :: (String -> Token) -> AlexPosn -> String -> TokenLoc
tokenize = flip . (>>= TokenLoc)

constoken :: Token -> AlexPosn -> String -> TokenLoc
constoken = tokenize . const

-- The token type:
data Token
    = TIncludes
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
    | PseudoClass PseudoClass
    | PseudoElement PseudoElement
    deriving (Eq,Show)
}
