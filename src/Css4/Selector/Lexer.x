--vim:ft=haskell
{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE PatternSynonyms #-}

module Css4.Selector.Lexer(
    AlexPosn(AlexPn)
  , Token(
      TIncludes, TEqual, TDashMatch, TPrefixMatch, TSuffixMatch, TSubstringMatch, Ident, String, THash, Decimal
    , Integer, Comma, Plus, Greater, Tilde, Dot, Pipe, Asterisk, Space, BOpen, BClose, PseudoClass, PseudoFunction
    , PseudoElement, TN, TNth, TPM, TInt, TClose, TNot, TLang
    )
  , TokenLoc(TokenLoc)
  , alexScanTokens
  ) where

import Css4.Selector.Utils(readCssString, readIdentifier)
import Css4.Selector.Core(
    PseudoElement(After, Before, FirstLetter, FirstLine, Marker, Placeholder, Selection)
  , PseudoClass(
        Active, Checked, Default, Disabled, Empty, Enabled, Focus, Fullscreen, Hover, Indeterminate, InRange
      , Invalid, Link, NthChild, NthLastChild, NthLastOfType, NthOfType, OnlyOfType, OnlyChild, Optional
      , OutOfRange, ReadOnly, ReadWrite, Required, Root, Target, Valid, Visited
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


@a       = a|A|\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?
@b       = b|B|\\0{0,4}(42|62)(\r\n|[ \t\r\n\f])?
@c       = c|C|\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?
@d       = d|D|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
@e       = e|E|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
@f       = f|F|\\0{0,4}(46|66)(\r\n|[ \t\r\n\f])?
@g       = g|G|\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?
@h       = h|H|\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?
@i       = i|I|\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?
@j       = j|J|\\0{0,4}(4a|6a)(\r\n|[ \t\r\n\f])?
@k       = k|K|\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?
@l       = l|L|\\0{0,4}(4c|6c)(\r\n|[ \t\r\n\f])?
@m       = m|M|\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?
@n       = n|N|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
@o       = o|O|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
@p       = p|P|\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?
@q       = q|Q|\\0{0,4}(51|71)(\r\n|[ \t\r\n\f])?
@r       = r|R|\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?
@s       = s|S|\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?
@t       = t|T|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
@u       = u|U|\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?
@v       = v|V|\\0{0,4}(56|76)(\r\n|[ \t\r\n\f])?|\\v
@w       = w|W|\\0{0,4}(57|77)(\r\n|[ \t\r\n\f])?
@x       = x|X|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?
@y       = y|Y|\\0{0,4}(59|79)(\r\n|[ \t\r\n\f])?
@z       = z|Z|\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?
@hyphen  = [\-]|\\0{0,4}2d

@firsth  = @f@i@r@s@t@hyphen
@nthh    = @n@t@h@hyphen
@onlyh   = @o@n@l@y@hyphen
@child   = @c@h@i@l@d
@oftype  = @o@f@hyphen@t@y@p@e
@lasth   = @l@a@s@t@hyphen

@cmo     = \/\*
@cmc     = \*\/
@psc     = [:]
@pse     = [:][:]
@psb     = [:][:]?
@lang    = [A-Za-z\-]+

tokens :-
 <0> {
  @wo "="  @wo                            { constoken TEqual }
  @wo "~=" @wo                            { constoken TIncludes }
  @wo "|=" @wo                            { constoken TDashMatch }
  @wo "^=" @wo                            { constoken TPrefixMatch }
  @wo "$=" @wo                            { constoken TSuffixMatch }
  @wo "*=" @wo                            { constoken TSubstringMatch }
  @wo ","  @wo                            { constoken Comma }
  "."                                     { constoken Dot }
  "|"                                     { constoken Pipe }
  "*"                                     { constoken Asterisk }
  @ident                                  { tokenize (Ident . readIdentifier) }
  @string                                 { tokenize (String . readCssString) }
  "#" @name                               { tokenize (THash . readIdentifier . drop 1) }
  @float                                  { tokenize (Decimal . read) }
  @int                                    { tokenize (Integer . read) }
  @wo "+" @wo                             { constoken Plus }
  @wo ">" @wo                             { constoken Greater }
  @wo "||" @wo                            { constoken DPipe }
  @wo $tl @wo                             { constoken Tilde }
  "[" @wo                                 { constoken BOpen }
  @wo "]"                                 { constoken BClose }
  @psb @a@f@t@e@r                         { constoken (PseudoElement After) }
  @psb @b@e@f@o@r@e                       { constoken (PseudoElement Before) }
  @psb @firsth@l@e@t@t@e@r                { constoken (PseudoElement FirstLetter) }
  @psb @firsth@l@i@n@e                    { constoken (PseudoElement FirstLine) }
  @pse @m@a@r@k@e@r                       { constoken (PseudoElement Marker) }
  @pse @p@l@a@c@e@h@o@l@d@e@r             { constoken (PseudoElement Placeholder) }
  @pse @s@e@l@e@c@t@i@o@n                 { constoken (PseudoElement Selection) }
  @psc @a@c@t@i@v@e                       { constoken (PseudoClass Active) }
  @psc @c@h@e@c@k@e@d                     { constoken (PseudoClass Checked) }
  @psc @d@e@f@a@u@l@t                     { constoken (PseudoClass Default) }
  @psc @d@i@s@a@b@l@e@d                   { constoken (PseudoClass Disabled) }
  @psc @e@m@p@t@y                         { constoken (PseudoClass Empty) }
  @psc @e@n@a@b@l@e@d                     { constoken (PseudoClass Enabled) }
  @psc @firsth@child                      { constoken (PseudoClass FirstChild) }
  @psc @firsth@oftype                     { constoken (PseudoClass FirstOfType) }
  @psc @f@o@c@u@s                         { constoken (PseudoClass Focus) }
  @psc @f@u@l@l@s@c@r@e@e@n               { constoken (PseudoClass Fullscreen) }
  @psc @h@o@v@e@r                         { constoken (PseudoClass Hover) }
  @psc @i@n@d@e@t@e@r@m@i@n@a@t@e         { constoken (PseudoClass Indeterminate) }
  @psc @i@n@hyphen@r@a@n@g@e              { constoken (PseudoClass InRange) }
  @psc @i@n@v@a@l@i@d                     { constoken (PseudoClass Invalid) }
  @psc @lasth@child                       { constoken (PseudoClass LastChild) }
  @psc @lasth@oftype                      { constoken (PseudoClass LastOfType) }
  @psc @l@i@n@k                           { constoken (PseudoClass Link) }
  @psc @l@a@n@g "("                       { constAndBegin TLang lang_state }
  @psc @nthh@child "("                    { constAndBegin (PseudoFunction NthChild) nth_state }
  @psc @nthh@lasth@child "("              { constAndBegin (PseudoFunction NthLastChild) nth_state }
  @psc @nthh@lasth@oftype "("             { constAndBegin (PseudoFunction NthLastOfType) nth_state }
  @psc @nthh@oftype "("                   { constAndBegin (PseudoFunction NthOfType) nth_state }
  @psc @n@o@t "(" @wo                     { constoken TNot }
  @wo ")"                                 { constoken TClose }
  @psc @onlyh@oftype                      { constoken (PseudoClass OnlyOfType) }
  @psc @onlyh@child                       { constoken (PseudoClass OnlyChild) }
  @psc @o@p@t@i@o@n@a@l                   { constoken (PseudoClass Optional) }
  @psc @o@u@t@hyphen@o@f@hyphen@r@a@n@g@e { constoken (PseudoClass OutOfRange) }
  @psc @r@e@a@d@hyphen@o@n@l@y            { constoken (PseudoClass ReadOnly) }
  @psc @r@e@a@d@hyphen@w@r@i@t@e          { constoken (PseudoClass ReadWrite) }
  @psc @r@e@q@u@i@r@e@d                   { constoken (PseudoClass Required) }
  @psc @r@o@o@t                           { constoken (PseudoClass Root) }
  @psc @t@a@r@g@e@t                       { constoken (PseudoClass Target) }
  @psc @v@a@l@i@d                         { constoken (PseudoClass Valid) }
  @psc @v@i@s@i@t@e@d                     { constoken (PseudoClass Visited) }
  $w @wo                                  { constoken Space }
  @cmo $nostar* \*+ ($nostars $nostar* \*+)* @cmc;
 }
 <nth_state> {
  $w @wo                                  { constoken Space }
  @e@v@e@n                                { constoken (TNth Even) }
  @o@d@d                                  { constoken (TNth Odd) }
  @n                                      { constoken TN }
  "+"                                     { constoken (TPM id) }
  "-"                                     { constoken (TPM negate) }
  @int                                    { tokenize (TInt . read) }
  ")"                                     { constAndBegin TClose state_initial }
 }
 <lang_state> {
  @lang                                   { tokenize String }
  $w @wo                                  { skip }
  ")"                                     { constAndBegin TClose state_initial }
 }
{

data TokenLoc = TokenLoc Token String (Maybe AlexPosn)

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
    | DPipe
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
