#ifndef SEMANTIC_ACTIONS_H
#define SEMANTIC_ACTIONS_H

#include <stdbool.h>

#include <antlr3defs.h>

// c2hs can not seem to find HsFFI.h
#ifndef HSFFI_H
typedef void*			HsStablePtr;
#endif

struct ArithParser_Ctx_struct;
struct ArithLexer_Ctx_struct;

typedef struct SemanticActions_struct
{
  struct ArithParser_Ctx_struct *ctx;
  struct ArithLexer_Ctx_struct *lexer;
  bool logDSPs;
  bool logDSPStackTrace;
  bool logDSPRuleInvocationStack;
  bool logDSPCached;
} SemanticActions;

typedef SemanticActions* pSemanticActions;

void semanticActionsDefaultContructor(SemanticActions *self);

void semanticActionsContructor(SemanticActions *self,
                               struct ArithParser_Ctx_struct *theArithParser,
                               struct ArithLexer_Ctx_struct *theLexer,
                               bool theLogDSPs,
                               bool theLogDSPStackTrace,
                               bool theLogDSPRuleInvocationStack,
                               bool theLogDSPCached);

bool logDSP(SemanticActions *self, bool answer, pANTLR3_TOKEN_STREAM input, const char *methodName, bool cached);

void _saEvaluate(SemanticActions *self, HsStablePtr t);

HsStablePtr _saIf(SemanticActions *self, pANTLR3_COMMON_TOKEN ifToken, HsStablePtr t1, HsStablePtr t2, HsStablePtr t3);

HsStablePtr _saSucc(SemanticActions *self, pANTLR3_COMMON_TOKEN succToken, HsStablePtr t);

HsStablePtr _saPred(SemanticActions *self, pANTLR3_COMMON_TOKEN predToken, HsStablePtr t);

HsStablePtr _saIsZero(SemanticActions *self, pANTLR3_COMMON_TOKEN isZeroToken, HsStablePtr t);

HsStablePtr _saTrue(SemanticActions *self, pANTLR3_COMMON_TOKEN trueToken);

HsStablePtr _saFalse(SemanticActions *self, pANTLR3_COMMON_TOKEN falseToken);

HsStablePtr _saIntV(SemanticActions *self, pANTLR3_COMMON_TOKEN intVToken);

#endif
