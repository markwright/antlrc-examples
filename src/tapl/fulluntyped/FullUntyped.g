/* Copyright (c)2010-2011, Mark Wright.  All rights reserved. */

grammar FullUntyped;

options {
        language = C;
}

tokens {
/* Keyword tokens */
        IF = 'if';
        THEN = 'then';
        ELSE = 'else';
        TRUE = 'true';
        FALSE = 'false';
        LAMBDA = 'lambda';
        TIMESFLOAT = 'timesfloat';
        SUCC = 'succ';
        PRED = 'pred';
        ISZERO = 'iszero';
        LET = 'let';
        IN = 'in';

/* Symbolic tokens */
        APOSTROPHE = '\'';
        DQUOTE = '"';
        ARROW = '->';
        BANG = '!';
        BARGT = '|>';
        BARRCURLY = '|}';
        BARRSQUARE = '|]';
        COLON = ':';
        COLONCOLON = '::';
        COLONEQ = ':=';
        COLONHASH = ':#';
        COMMA = ',';
        DARROW = '=>';
        DDARROW = '==>';
        DOT = '.';
        EQ = '=';
        EQEQ = '==';
        GT = '>';
        HASH = '#';
        LCURLY = '{';
        LCURLYBAR = '{|';
        LEFTARROW = '<-';
        LPAREN = '(';
        LSQUARE = '[';
        LSQUAREBAR = '[|';
        LT = '<';
        RCURLY = '}';
        RPAREN = ')';
        RSQUARE = ']';
        SEMI = ';';
        SLASH = '/';
        STAR = '*';
        TRIANGLE = '$';
        USCORE = '_';
        VBAR = '|';
}

scope GS {
        pSemanticActions sa;
}

@parser::includes {
#include "SemanticActions.h"
}

toplevel[pSemanticActions theSemanticActions] returns [HsStablePtr value]
scope GS;
@init { $GS::sa = theSemanticActions; }
        :   declaration_seq? EOF
                {$value=$declaration_seq.value;}
        ;

declaration_seq returns [HsStablePtr value]
@init { $value = _saDeclarationListNew($GS::sa); }
        :   (declaration
                {$value=_saDeclarationListAppend($GS::sa, $value, $declaration.value);}
            )+
        ;

declaration returns [HsStablePtr value]
        :   command SEMI
                {$value=$command.value;}
        ;

command returns [HsStablePtr value]
        :   term
                {$value = _saTerm($GS::sa, $term.value);}
        |   LCID binder
                {$value = _saLcidBinder($GS::sa, $LCID, $binder.value);}
        ;

binder  returns [HsStablePtr value]
        :   SLASH
                {$value = _saSlash($GS::sa, $SLASH);}
        |   EQ term
                {$value = _saEqTerm($GS::sa, $EQ, $term.value);}
        ;

term returns [HsStablePtr value]
        :   appterm
                {$value = $appterm.value;}
        |   IF t1=term THEN t2=term ELSE t3=term
                {$value = _saIf($GS::sa, $IF, $t1.value, $t2.value, $t3.value);}
        |   LAMBDA LCID DOT t=term
                {$value = _saLambdaLcidDotTerm($GS::sa, $LAMBDA, $LCID, $t.value);}
        |   LET LCID EQ t1=term IN t2=term
                {$value = _saLetLcidEqTermInTerm($GS::sa, $LET, $LCID, $t1.value, $t2.value);}
        |   LET USCORE EQ t1=term IN t2=term
                {$value = _saLetUscoreEqTermInTerm($GS::sa, $LET, $t1.value, $t2.value);}
        ;

/* appterm with left recursion removed */
appterm returns [HsStablePtr value]
        :   appterm_lhs appterm_tail?
                {$value = _saAppTerm($GS::sa, $appterm_lhs.value, $appterm_tail.value);}
        ;

appterm_lhs returns [HsStablePtr value]
        :   pathterm
                {$value = $pathterm.value;}
        |   TIMESFLOAT p1=pathterm p2=pathterm
                {$value = _saTimesFloat($GS::sa, $TIMESFLOAT, $p1.value, $p2.value);}
        |   SUCC pathterm
                {$value = _saSucc($GS::sa, $SUCC, $pathterm.value);}
        |   PRED pathterm
                {$value = _saPred($GS::sa, $PRED, $pathterm.value);}
        |   ISZERO pathterm
                {$value = _saIsZero($GS::sa, $ISZERO, $pathterm.value);}
        ;

appterm_tail returns [HsStablePtr value]
        :   pathterm a=appterm_tail
                {$value = _saAppTermTail($GS::sa, $pathterm.value, $a.value);}
        ;

/* pathterm with left recursion removed */
pathterm returns [HsStablePtr value]
        :   aterm pathterm_tail?
                {$value = _saPathTerm($GS::sa, $aterm.value, $pathterm_tail.value);}
        ;

pathterm_tail returns [HsStablePtr value]
        :   pathterm_rhs p=pathterm_tail
                {$value = _saPathTermTail($GS::sa, $pathterm_rhs.value, $p.value);}
        ;

pathterm_rhs returns [HsStablePtr value]
        :   DOT LCID
                {$value = _saDotLcid($GS::sa, $DOT, $LCID);}
        |   DOT INTV
                {$value = _saDotIntV($GS::sa, $DOT, $INTV);}
        ;

/* aterm - atomic terms are terms that do not require extra parentheses */
aterm returns [HsStablePtr value]
        :   LPAREN term RPAREN
                {$value = $term.value;}
        |   TRUE
                {$value = _saTrue($GS::sa, $TRUE);}
        |   FALSE
                {$value = _saFalse($GS::sa, $FALSE);}
        |   LCID
                {$value = _saLcid($GS::sa, $LCID);}
        |   LCURLY fields? RCURLY
                {$value = _saFieldsInCurlyBraces($GS::sa, $LCURLY, $fields.value);}
        |   FLOATV
                {$value = _saFloatV($GS::sa, $FLOATV);}
        |   STRINGV
                {$value = _saStringV($GS::sa, $STRINGV);}
        |   INTV
                {$value = _saIntV($GS::sa, $INTV);}
        ;

fields returns [HsStablePtr value]
        :   f1=field
                {$value = _saFieldListNew($GS::sa, $f1.value);}
            (COMMA f2=field
                {$value = _saFieldListAppend($GS::sa, $value, $f2.value);}
            )*
        ;

field returns [HsStablePtr value]
        :   LCID EQ term
                {$value = _saLcidEqTerm($GS::sa, $LCID, $term.value);}
        |   term
                {$value = _saFieldTerm($GS::sa, $term.value);}
        ;

fragment
DIGIT
        :   '0'..'9'
        ;

fragment
HEXADECIMAL_DIGIT
        :   ('0'..'9'|'a'..'f'|'A'..'F')
        ;

UCID
        :	('A'..'Z')('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
        ;

LCID
        :	('a'..'z'|'_')('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
        ;

INTV
        :	DIGIT+
        ;

fragment
NONZERO_DIGIT
        :   '1'..'9'
        ;

FLOATV
        :   FRACTIONAL_CONSTANT
        ;

fragment
FRACTIONAL_CONSTANT
        :   DIGIT_SEQUENCE '.'
        |   DIGIT_SEQUENCE? '.' DIGIT_SEQUENCE
        ;

fragment
DIGIT_SEQUENCE
        :   DIGIT+
        ;

fragment
UNIVERSAL_CHARACTER_NAME
        :   '\\' 'u' HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT
        |   '\\' 'U' HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT
                     HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT
        ;

STRINGV
        :   '"' S_CHAR_SEQUENCE? '"'
        ;

fragment
S_CHAR_SEQUENCE
        :   S_CHAR+
        ;

fragment
S_CHAR
        :   ESCAPE_SEQUENCE
        |   ~('"'|'\\'|'\n')
        |   UNIVERSAL_CHARACTER_NAME
        ;

fragment
ESCAPE_SEQUENCE
        :   SIMLE_ESCAPE_SEQUENCE
        |   OCTAL_ESCAPE_SEQUENCE
        |   HEXADECIMAL_ESCAPE_SEQUENCE
        ;

fragment
SIMLE_ESCAPE_SEQUENCE
        :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
        ;

fragment
OCTAL_ESCAPE_SEQUENCE
        :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
        |   '\\' ('0'..'7') ('0'..'7')
        |   '\\' ('0'..'7')
        ;

fragment
HEXADECIMAL_ESCAPE_SEQUENCE
        :   '\\' 'x' HEXADECIMAL_DIGIT+
        ;

WS      :	(' '|'\t'|'\n'|'\r')+ {$channel=HIDDEN;} ;

ML_COMMENT
        :	'/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
        ;	
