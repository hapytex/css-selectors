--vim:ft=haskell
{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE PatternSynonyms #-}

module Css3.Selector.Lexer(AlexPosn(..), Token(..), TokenLoc(..), alexScanTokens) where

import Css3.Selector.Utils(readCssString, readIdentifier)
import Css3.Selector.Core(
    PseudoElement(After, Before, FirstLetter, FirstLine)
  , PseudoClass(
        Active, Checked, Disabled, Empty, Enabled, Focus, Hover, InRange, Invalid, Link, NthChild, NthLastChild, NthLastOfType, NthOfType
      , OnlyOfType, OnlyChild, Optional, OutOfRange, ReadOnly, ReadWrite, Required, Root, Target, Valid, Visited
      )
  , Nth, pattern Even, pattern Odd
  , pattern FirstChild, pattern FirstOfType, pattern LastChild, pattern LastOfType
  )

import Data.Decimal(Decimal)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup(Semigroup((<>)))
#endif
}

%wrapper "monadUserState"

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
@lang    = [A-Za-z\-]+

tokens :-
 <0> {
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
  @psc "lang("         { constAndBegin TLang lang_state }
  @psc "nth-child("        { constAndBegin (PseudoFunction NthChild) nth_state }
  @psc "nth-last-child("   { constAndBegin (PseudoFunction NthLastChild) nth_state }
  @psc "nth-last-of-type(" { constAndBegin (PseudoFunction NthLastOfType) nth_state }
  @psc "nth-of-type("      { constAndBegin (PseudoFunction NthOfType) nth_state }
  @psc @n@o@t "(" @wo      { constoken TNot }
  @wo ")"                  { constoken TClose }
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
  @cmo $nostar* \*+ ($nostars $nostar* \*+)* @cmc;
 }
 <nth_state> {
  $w @wo               { constoken Space }
  @e@v@e@n             { constoken (TNth Even) }
  @o@d@d               { constoken (TNth Odd) }
  @n                   { constoken TN }
  "+"                  { constoken (TPM id) }
  "-"                  { constoken (TPM negate) }
  @int                 { tokenize (TInt . read) }
  ")"                  { constAndBegin TClose state_initial }
 }
 <lang_state> {
  @lang                { tokenize String }
  $w @wo               { skip }
  ")"                  { constAndBegin TClose state_initial }
 }
{

data TokenLoc = TokenLoc { tokenType :: Token, original :: String, location :: Maybe AlexPosn }

type AlexUserState = ()

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
    | PseudoFunction (Nth -> PseudoClass)
    | PseudoElement PseudoElement
    | TN
    | TNth Nth
    | TPM (Int -> Int)
    | TInt Int
    | TClose
    | TNot
    | TLang

tokenize :: (String -> Token) -> AlexInput -> Int -> Alex TokenLoc
tokenize f (p, _, _, str) len = pure (TokenLoc (f str') str' (Just p))
  where str' = take len str

constoken :: Token -> AlexInput -> Int -> Alex TokenLoc
constoken = tokenize . const

constAndBegin :: Token -> Int -> AlexInput -> Int -> Alex TokenLoc
constAndBegin = andBegin . constoken

state_initial :: Int
state_initial = 0

alexInitUserState :: AlexUserState
alexInitUserState = ()

alexEOF :: Alex TokenLoc
alexEOF = pure (TokenLoc undefined "" Nothing)

alexScanTokens :: String -> Either String [TokenLoc]
alexScanTokens str = runAlex str loop
  where loop :: Alex [TokenLoc]
        loop = alexMonadScan >>= p
        p (TokenLoc _ _ Nothing) = pure []
        p toc = (toc:) <$> loop
}
