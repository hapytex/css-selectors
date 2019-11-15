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
    ','    { Comma }
    '>'    { Greater }
    '+'    { Plus }
    '~'    { Tilde }
    '.'    { Dot }
    ' '    { Space }
    '|'    { Pipe }
    '*'    { Asterisk }
    '['    { BOpen }
    ']'    { BClose }
    '='    { TEqual }
    '^='   { TPrefixMatch }
    '$='   { TSuffixMatch }
    '*='   { TSubstringMatch }
    '|='   { TDashMatch }
    '~='   { TIncludes }
    ident  { Ident $$ }
    string { String $$ }
    hash   { THash $$ }

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
    | SelectorAddition FilterList { addFilters (SimpleSelector universal) ($1:$2) }
    ;

FilterList
    :                             { [] }
    | SelectorAddition FilterList { $1 : $2 }
    ;

SelectorAddition
    : hash                        { SHash (Hash (pack $1)) }
    | Class                       { SClass $1 }
    | Attrib                      { SAttrib $1 }
    ;

AttribBox
    : '[' Sopt Attrib Sopt ']'             { $3 }
    ;

Attrib
    : AttribName                           { Exist $1 }
    | AttribName AttribOpS ident           { Attrib $1 $2 (pack $3) }
    | AttribName AttribOpS string          { Attrib $1 $2 (pack $3) }
    ;

AttribName
    : NamespacePrefix ident       { AttributeName $1 (pack $2) }
    | ident                       { AttributeName NAny (pack $1) }
    ;

AttribOpS
    : Sopt AttribOp Sopt          { $2 }
    ;

Sopt
    :                             { () }
    | ' '                         { () }
    ;

AttribOp
    : '='                         { Exact }
    | '~='                        { Include }
    | '|='                        { DashMatch }
    | '^='                        { PrefixMatch }
    | '$='                        { SuffixMatch }
    | '*='                        { SubstringMatch }
    ;

Type
    : TypeSelector                { $1 }
    | '*'                         { universal }
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
    | '|'          { NEmpty }
    ;

{
}
