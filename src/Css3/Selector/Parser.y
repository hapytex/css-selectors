-- vim:ft=haskell
{
module Css3.Selector.Parser where

import Css3.Selector.Core
import Css3.Selector.Lexer(AlexPosn(..), Token(..), TokenLoc(..))

import Data.List.NonEmpty(NonEmpty((:|)), (<|))

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.Text(pack)
}

%name cssselector
%tokentype { TokenLoc }
%error { happyError }

%token
    ','    { TokenLoc Comma _ _ }
    '>'    { TokenLoc Greater _ _ }
    '+'    { TokenLoc Plus _ _ }
    '~'    { TokenLoc Tilde _ _ }
    '.'    { TokenLoc Dot _ _ }
    ' '    { TokenLoc Space _ _ }
    '|'    { TokenLoc Pipe _ _ }
    '*'    { TokenLoc Asterisk _ _ }
    '['    { TokenLoc BOpen _ _ }
    ']'    { TokenLoc BClose _ _ }
    '='    { TokenLoc TEqual _ _ }
    '^='   { TokenLoc TPrefixMatch _ _ }
    '$='   { TokenLoc TSuffixMatch _ _ }
    '*='   { TokenLoc TSubstringMatch _ _ }
    '|='   { TokenLoc TDashMatch _ _ }
    '~='   { TokenLoc TIncludes _ _ }
    ident  { TokenLoc (Ident $$) _ _ }
    string { TokenLoc (String $$) _ _ }
    hash   { TokenLoc (THash $$) _ _ }

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
    : NamespacePrefix '|' Ident   { AttributeName $1 $3 }
    | '|' Ident                   { AttributeName NEmpty $2 }
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
    : ElementName                      { NAny .| $1 }
    | '|' ElementName                  { NEmpty .| $2 }
    | NamespacePrefix '|' ElementName  { $1 .| $3 }
    ;

ElementName
    : Ident        { ElementName $1 }
    | '*'          { EAny }
    ;

Class
    : '.' Ident    { Class $2 }
    ;

NamespacePrefix
    : Ident        { Namespace $1 }
    | '*'          { NAny }
    ;

Ident
    : ident        { pack $1 }
    ;

{

happyError :: [TokenLoc] -> a
happyError [] = error "Unexpected end of string when parsing a css selector."
happyError (~(TokenLoc _ s ~(AlexPn _ l c)):_) = error ("Can not parse the CSS selector: unpexected token \"" <> s <> "\" at location (" <> show l <> ", " <> show c <> ")")

}
