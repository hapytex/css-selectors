-- vim:ft=haskell
{
module Css.Selector.Parser where

import Css.Selector.Core
import Css.Selector.Lexer(Token(..))

import Data.List.NonEmpty(NonEmpty((:|)), (<|))
import Data.Text(pack)
}

%name cssselector
%tokentype { Token }
%error { fail "Can not parse the CSS selector" }

%token
    ','   { Comma }
    '>'   { Greater }
    '+'   { Plus }
    '~'   { Tilde }
    '.'   { Dot }
    ' '   { Space }
    '|'   { Pipe }
    '*'   { Asterisk }
    '['   { BOpen }
    ']'   { BClose }
    ident { Ident $$ }
    hash  { THash $$ }

%%

SelectorsGroup
    : SelectorGroupList                           { SelectorGroup $1 }
    ;

SelectorGroupList
    : Selector                                    { $1 :| [] }
    | Selector ',' SelectorGroupList              { $1 <| $3 }
    ;

Selector
    : SimpleSelectorSequence                      { SelectorSequence $1 }
    | SimpleSelectorSequence Combinator Selector  { $2 $1 $3 }
    ;

-- Combinator :: SimpleSelector -> Selector -> Selector
Combinator
    : '+'          { DirectlyPreceded }
    | '>'          { Child }
    | '~'          { Preceded }
    | ' '          { Descendant }
    ;

SimpleSelectorSequence
    : Type FilterList             { addFilters (SimpleSelector $1) $2 }
    | SelectorAddition FilterList { addFilters (SelectorFilter $1) $2 }
    ;

FilterList
    :                             { [] }
    | SelectorAddition FilterList { $1 : $2 }
    ;

SelectorAddition
    : hash                        { SHash (Hash (pack $1)) }
    | Class                       { SClass $1 }
    ;

Type
    : TypeSelector                { Type $1 }
    | '*'                         { Universal }
    ;

TypeSelector
    : NamespacePrefix ElementName { TypeSelector $1 $2 }
    | ElementName                 { TypeSelector NAny $1 }
    ;

ElementName
    : ident        { ElementName (pack $1) }
    ;

Class
    : '.' ident    { Class (pack $2) }
    ;

NamespacePrefix
    : ident '|'    { Namespace (pack $1) }
    | '*' '|'      { NAny }
    | '|'          { NAny }
    ;

{
}
