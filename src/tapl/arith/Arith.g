/* Copyright (c)2010-2011, Mark Wright.  All rights reserved. */

grammar Arith;

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
        SUCC = 'succ';
        PRED = 'pred';
        ISZERO = 'iszero';

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

toplevel[pSemanticActions theSemanticActions]
scope GS;
@init {
        $GS::sa = theSemanticActions;
}
        :   declaration_seq? EOF
        ;

declaration_seq
        :   declaration+
        ;

declaration
        :   command SEMI
                {_saEvaluate($GS::sa, $command.value);}
        ;

command returns [HsStablePtr value]
        :   term
                {$value = $term.value;}
        ;

term returns [HsStablePtr value]
        :   appterm
                {$value = $appterm.value;}
        |   IF t1=term THEN t2=term ELSE t3=term
                {$value = _saIf($GS::sa, $IF, $t1.value, $t2.value, $t3.value);}
        ;

appterm returns [HsStablePtr value]
        :   aterm
                {$value = $aterm.value;}
        |   SUCC aterm
                {$value = _saSucc($GS::sa, $SUCC, $aterm.value);}
        |   PRED aterm
                {$value = _saPred($GS::sa, $PRED, $aterm.value);}
        |   ISZERO aterm
                {$value = _saIsZero($GS::sa, $ISZERO, $aterm.value);}
        ;

aterm returns [HsStablePtr value]
        :   LPAREN term RPAREN
                {$value = $term.value;}
        |   TRUE
                {$value = _saTrue($GS::sa, $TRUE);}
        |   FALSE
                {$value = _saFalse($GS::sa, $FALSE);}
        |   INTV
                {$value = _saIntV($GS::sa, $INTV);}
        ;

fragment
DIGIT
        :   '0'..'9'
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

WS      :	(' '|'\t'|'\n'|'\r')+ {$channel=HIDDEN;} ;

ML_COMMENT
        :	'/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
        ;	
