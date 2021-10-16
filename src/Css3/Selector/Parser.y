-- vim:ft=haskell
{
module Css3.Selector.Parser where

import Css3.Selector.Core
import Css3.Selector.Lexer(AlexPosn(..), Token(..), TokenLoc(..))

import Data.List.NonEmpty(NonEmpty((:|)), (<|))

import Data.Default(def)
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
    pseude { TokenLoc (PseudoElement $$) _ _ }
    pseudc { TokenLoc (PseudoClass $$) _ _ }
    pseudf { TokenLoc (PseudoFunction $$) _ _ }
    pm     { TokenLoc (TPM $$) _ _ }
    'n'    { TokenLoc TN _ _ }
    int    { TokenLoc (TInt $$) _ _ }
    nth    { TokenLoc (TNth $$) _ _ }
    ')'    { TokenLoc TNthClose _ _ }

%%

SelectorsGroup
    : SelectorGroupList                           { SelectorGroup $1 }
    ;

SelectorGroupList
    : Selector                                    { $1 :| [] }
    | Selector ',' SelectorGroupList              { $1 <| $3 }
    ;

Selector
    : PseudoSelectorSequence                      { Selector $1 }
    | PseudoSelectorSequence Combinator Selector  { Combined $1 $2 $3 }
    ;

Combinator
    : '+'          { DirectlyPreceded }
    | '>'          { Child }
    | '~'          { Preceded }
    | ' '          { Descendant }
    ;

PseudoSelectorSequence
    : SimpleSelectorSequence        { Sequence $1 }
    | SimpleSelectorSequence pseude { $1 :.:: $2 }
    | pseude                        { def :.:: $1 }
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
    | pseudc                      { SPseudo $1 }
    | pseudf OptSpace Nth         { SPseudo ($1 $3) }
    | Class                       { SClass $1 }
    | AttribBox                   { SAttrib $1 }
    ;

Nth
    : nth OptSpace ')'                                       { $1 }
    | PMOpt IntOpt 'n' OptSpace ')'                          { Nth ($1 $2) 0 }
    | PMOpt IntOpt 'n' OptSpace pm OptSpace int OptSpace ')' { Nth ($1 $2) ($5 $7) }
    | PMOpt int OptSpace ')'                                 { Nth 0 ($1 $2) }
    ;

PMOpt
    :                             { id }
    | pm                          { $1 }
    ;

IntOpt
    :                             { 1 }
    | int                         { $1 }
    ;

OptSpace
    :                             { () }
    | ' '                         { () }
    ;

AttribBox
    : '[' Attrib ']'              { $2 }
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
happyError (~(TokenLoc _ s ~(Just (AlexPn _ l c))):_) = error ("Can not parse the CSS selector: unpexected token \"" <> s <> "\" at location (" <> show l <> ", " <> show c <> ")")
happyError _ = error "Unexpected end of string when parsing a css selector."

}
