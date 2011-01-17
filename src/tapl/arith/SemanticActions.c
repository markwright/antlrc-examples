/* Copyright (c)2010-2011, Mark Wright.  All rights reserved. */

#include <stdio.h>

#if defined(__sun) && defined(__SVR4)
#include <ucontext.h>
#else
#if defined(__linux__)
#include <execinfo.h>
#endif
#endif

#include "Semantic_stub.h"
#include "SemanticActions.h"
#include "ArithLexer.h"
#include "ArithParser.h"

#if defined(__sun) && defined(__SVR4)
void print_stack()
{
  printstack(fileno(stdout));
}
#else
#if defined(__linux__)
void print_stack()
{
  const int max_stack_frames = 100;
  void* tracePtrs[max_stack_frames];
  int count = backtrace(tracePtrs, max_stack_frames);
  char** funcNames = backtrace_symbols(tracePtrs, count);

  // Print the stack trace
  for( int ii = 0; ii < count; ii++)
    printf( "%s\n", funcNames[ii]);

  // Free the string pointers
  free(funcNames);
}
#else
void print_stack()
{
}
#endif
#endif

void semanticActionsDefaultContructor(SemanticActions *self)
{
  self->ctx = (ArithParser *)0;
  self->lexer = (ArithLexer *)0;
  self->logDSPs = false;
  self->logDSPStackTrace = false;
  self->logDSPRuleInvocationStack = false;
  self->logDSPCached = false;
}

void semanticActionsContructor(SemanticActions *self,
                               ArithParser *theArithParser,
                               ArithLexer *theArithLexer,
                               bool theLogDSPs,
                               bool theLogDSPStackTrace,
                               bool theLogDSPRuleInvocationStack,
                               bool theLogDSPCached)
{
  semanticActionsDefaultContructor(self);
  self->ctx = theArithParser;
  self->lexer = theArithLexer;
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
    print_stack();
  return answer;
}

void _saEvaluate(SemanticActions *self, HsStablePtr t)
{
  saEvaluate(t);
}

HsStablePtr _saIf(SemanticActions *self, pANTLR3_COMMON_TOKEN ifToken, HsStablePtr t1, HsStablePtr t2, HsStablePtr t3)
{
  return saIf(ifToken, t1, t2, t3);
}

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

HsStablePtr _saTrue(SemanticActions *self, pANTLR3_COMMON_TOKEN trueToken)
{
  return saTrue(trueToken);
}

HsStablePtr _saFalse(SemanticActions *self, pANTLR3_COMMON_TOKEN falseToken)
{
  return saFalse(falseToken);
}

HsStablePtr _saIntV(SemanticActions *self, pANTLR3_COMMON_TOKEN intVToken)
{
  return saIntV(intVToken);
}

