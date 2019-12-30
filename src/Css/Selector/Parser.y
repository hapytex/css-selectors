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
    : SimpleSelectorSequence                      { Selector $1 }
    | SimpleSelectorSequence Combinator Selector  { Combined $1 $2 $3 }
    ;

Combinator
    : '+'          { DirectlyPreceded }
    | '>'          { Child }
    | '~'          { Preceded }
    | ' '          { Descendant }
    ;

SimpleSelectorSequence
    : Type FilterList             { addFilters (SimpleSelector $1) $2 }
    | SelectorAddition FilterList { addFilters (SimpleSelector Universal) ($1:$2) }
    ;

FilterList
    :                             { [] }
    | SelectorAddition FilterList { $1 : $2 }
    ;

SelectorAddition
    : hash                        { SHash (Hash (pack $1)) }
    | Class                       { SClass $1 }
    | AttribBox                   { SAttrib $1 }
    ;

AttribBox
    : '[' Attrib ']'                       { $2 }
    ;

Attrib
    : AttribName                          { Exist $1 }
    | AttribName AttribOp Ident           { Attrib $1 $2 $3 }
    | AttribName AttribOp string          { Attrib $1 $2 (pack $3) }
    ;

AttribName
    : NamespacePrefix Ident       { AttributeName $1 $2 }
    | Ident                       { AttributeName NAny $1 }
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
    | '*'                         { Universal }
    ;

TypeSelector
    : NamespacePrefix ElementName { $1 .| $2 }
    | ElementName                 { NAny .| $1 }
    ;

ElementName
    : Ident        { ElementName $1 }
    ;

Class
    : '.' Ident    { Class $2 }
    ;

NamespacePrefix
    : Ident '|'    { Namespace $1 }
    | '*' '|'      { NAny }
    | '|'          { NEmpty }
    ;

Ident
    : ident        { pack $1 }
    ;

{
}
