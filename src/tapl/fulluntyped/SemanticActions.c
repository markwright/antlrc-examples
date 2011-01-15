/* Copyright (c)2010-2011, Mark Wright.  All rights reserved. */

#include <stdio.h>
#include <ucontext.h>

#include "Semantic_stub.h"
#include "SemanticActions.h"
#include "FullUntypedLexer.h"
#include "FullUntypedParser.h"

void semanticActionsDefaultContructor(SemanticActions *self)
{
  self->ctx = (FullUntypedParser *)0;
  self->lexer = (FullUntypedLexer *)0;
  self->logDSPs = false;
  self->logDSPStackTrace = false;
  self->logDSPRuleInvocationStack = false;
  self->logDSPCached = false;
}

void semanticActionsContructor(SemanticActions *self,
                               FullUntypedParser *theFullUntypedParser,
                               FullUntypedLexer *theFullUntypedLexer,
                               bool theLogDSPs,
                               bool theLogDSPStackTrace,
                               bool theLogDSPRuleInvocationStack,
                               bool theLogDSPCached)
{
  semanticActionsDefaultContructor(self);
  self->ctx = theFullUntypedParser;
  self->lexer = theFullUntypedLexer;
  self->logDSPs = theLogDSPs;
  self->logDSPStackTrace = theLogDSPStackTrace;
  self->logDSPRuleInvocationStack = theLogDSPRuleInvocationStack;
  self->logDSPCached = theLogDSPCached;
}

bool logDSP(SemanticActions *self, bool answer, pANTLR3_TOKEN_STREAM input, const char *methodName, bool cached)
{
  if ((!self->logDSPCached && cached) || !self->logDSPs)
    return answer;
  ANTLR3_COMMON_TOKEN *token = input->_LT(input, 1);
  int line = 0;
  int charPositionInLine = 0;
  if (token != (ANTLR3_COMMON_TOKEN *)0)
  {
    line = token->getLine(token);
    charPositionInLine = token->getCharPositionInLine(token);
  }
  char tokenInfo[512];
  ANTLR3_STRING *fileName = self->lexer->pLexer->input->fileName;
  snprintf(tokenInfo, sizeof(tokenInfo), "%s: %d:%d", fileName->chars, line, charPositionInLine);
  const char *cachedStackString = (cached ? "(cached) " : "");
  printf("%s %s %s %s\n", tokenInfo, methodName, (answer ? "true" : "false"), cachedStackString);
  if (self->logDSPStackTrace)
    printstack(fileno(stdout));
  return answer;
}

void _saEvaluate(SemanticActions *self, HsStablePtr t)
{
  saEvaluate(t);
}

HsStablePtr _saDeclarationListNew(SemanticActions *self);

HsStablePtr _saDeclarationListAppend(SemanticActions *self, HsStablePtr ls, HsStablePtr l);

HsStablePtr _saTerm(SemanticActions *self, HsStablePtr term);

HsStablePtr _saLcidBinder(SemanticActions *self, pANTLR3_COMMON_TOKEN lcidToken, HsStablePtr binder);

HsStablePtr _saSlash(SemanticActions *self, pANTLR3_COMMON_TOKEN slashToken);

HsStablePtr _saEqTerm(SemanticActions *self, pANTLR3_COMMON_TOKEN eqToken, HsStablePtr term);

HsStablePtr _saIf(SemanticActions *self, pANTLR3_COMMON_TOKEN ifToken, HsStablePtr t1, HsStablePtr t2, HsStablePtr t3)
{
  return saIf(ifToken, t1, t2, t3);
}

HsStablePtr _saLambdaLcidDotTerm(SemanticActions *self,
                                 pANTLR3_COMMON_TOKEN lambdaToken,
                                 pANTLR3_COMMON_TOKEN lcidToken,
                                 HsStablePtr term);

HsStablePtr _saLetLcidEqTermInTerm(SemanticActions *self,
                                   pANTLR3_COMMON_TOKEN letToken,
                                   pANTLR3_COMMON_TOKEN lcidToken,
                                   HsStablePtr t1,
                                   HsStablePtr t2);

HsStablePtr _saLetUscoreEqTermInTerm(SemanticActions *self,
                                     pANTLR3_COMMON_TOKEN letToken,
                                     HsStablePtr t1,
                                     HsStablePtr t2);

HsStablePtr _saAppTerm(SemanticActions *self, HsStablePtr appTermLhs, HsStablePtr appTermTail);

HsStablePtr _saTimesFloat(SemanticActions *self, pANTLR3_COMMON_TOKEN timesToken, HsStablePtr p1, HsStablePtr p2);

HsStablePtr _saSucc(SemanticActions *self, pANTLR3_COMMON_TOKEN succToken, HsStablePtr t)
{
  return saSucc(succToken, t);
}

HsStablePtr _saPred(SemanticActions *self, pANTLR3_COMMON_TOKEN predToken, HsStablePtr t)
{
  return saPred(predToken, t);
}

HsStablePtr _saIsZero(SemanticActions *self, pANTLR3_COMMON_TOKEN isZeroToken, HsStablePtr t)
{
  return saIsZero(isZeroToken, t);
}

HsStablePtr _saAppTermTail(SemanticActions *self, HsStablePtr pathTerm, HsStablePtr appTermTail);

HsStablePtr _saPathTerm(SemanticActions *self, HsStablePtr aTerm, HsStablePtr pathTermTail);

HsStablePtr _saPathTermTail(SemanticActions *self, HsStablePtr pathTermRhs, HsStablePtr pathTermTail);

HsStablePtr _saDotLcid(SemanticActions *self, pANTLR3_COMMON_TOKEN dotToken, pANTLR3_COMMON_TOKEN lcidToken);

HsStablePtr _saDotIntV(SemanticActions *self, pANTLR3_COMMON_TOKEN dotToken, pANTLR3_COMMON_TOKEN intVToken);

HsStablePtr _saTrue(SemanticActions *self, pANTLR3_COMMON_TOKEN trueToken)
{
  return saTrue(trueToken);
}

HsStablePtr _saFalse(SemanticActions *self, pANTLR3_COMMON_TOKEN falseToken)
{
  return saFalse(falseToken);
}

HsStablePtr _saLcid(SemanticActions *self, pANTLR3_COMMON_TOKEN lcidToken)
{
  return saLcid(intVToken);
}

HsStablePtr _saFieldsInCurlyBraces(SemanticActions *self, pANTLR3_COMMON_TOKEN leftCurlyToken, HsStablePtr fields);

HsStablePtr _saFloatV(SemanticActions *self, pANTLR3_COMMON_TOKEN floatVToken)
{
  return saFloatV(intVToken);
}

HsStablePtr _saStringV(SemanticActions *self, pANTLR3_COMMON_TOKEN stringVToken)
{
  return saStringV(intVToken);
}

HsStablePtr _saIntV(SemanticActions *self, pANTLR3_COMMON_TOKEN intVToken)
{
  return saIntV(intVToken);
}

HsStablePtr _saFieldListNew(SemanticActions *self, HsStablePtr f1)
{
  saFieldListNew(f1);
}

HsStablePtr _saFieldListAppend(SemanticActions *self, HsStablePtr fs, HsStablePtr f2)
{
  saFieldListAppend(fs, f2);
}

HsStablePtr _saLcidEqTerm(SemanticActions *self, pANTLR3_COMMON_TOKEN lcidToken, HsStablePtr termFunc)
{
  return saLcidTerm(lcidToken, termFunc);
}

HsStablePtr _saFieldTerm(SemanticActions *self, HsStablePtr termFunc)
{
  return saFieldTerm(termFunc);
}
