/* Copyright (c)2010-2011, Mark Wright.  All rights reserved. */

#ifndef SEMANTIC_ACTIONS_H
#define SEMANTIC_ACTIONS_H

#include <stdbool.h>

#include <antlr3defs.h>

// c2hs can not seem to find HsFFI.h
#ifndef HSFFI_H
typedef void*			HsStablePtr;
#endif

struct FullUntypedParser_Ctx_struct;
struct FullUntypedLexer_Ctx_struct;

typedef struct SemanticActions_struct
{
  struct FullUntypedParser_Ctx_struct *ctx;
  struct FullUntypedLexer_Ctx_struct *lexer;
  bool logDSPs;
  bool logDSPStackTrace;
  bool logDSPRuleInvocationStack;
  bool logDSPCached;
} SemanticActions;

typedef SemanticActions* pSemanticActions;

void semanticActionsDefaultContructor(SemanticActions *self);

void semanticActionsContructor(SemanticActions *self,
                               struct FullUntypedParser_Ctx_struct *theFullUntypedParser,
                               struct FullUntypedLexer_Ctx_struct *theLexer,
                               bool theLogDSPs,
                               bool theLogDSPStackTrace,
                               bool theLogDSPRuleInvocationStack,
                               bool theLogDSPCached);

bool logDSP(SemanticActions *self, bool answer, pANTLR3_TOKEN_STREAM input, const char *methodName, bool cached);

HsStablePtr _saDeclarationListNew(SemanticActions *self);

HsStablePtr _saDeclarationListAppend(SemanticActions *self, HsStablePtr ls, HsStablePtr l);

HsStablePtr _saTerm(SemanticActions *self, HsStablePtr term);

HsStablePtr _saLcidBinder(SemanticActions *self, pANTLR3_COMMON_TOKEN lcidToken, HsStablePtr binder);

HsStablePtr _saSlash(SemanticActions *self, pANTLR3_COMMON_TOKEN slashToken);

HsStablePtr _saEqTerm(SemanticActions *self, pANTLR3_COMMON_TOKEN eqToken, HsStablePtr term);

HsStablePtr _saIf(SemanticActions *self, pANTLR3_COMMON_TOKEN ifToken, HsStablePtr t1, HsStablePtr t2, HsStablePtr t3);

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

HsStablePtr _saSucc(SemanticActions *self, pANTLR3_COMMON_TOKEN succToken, HsStablePtr t);

HsStablePtr _saPred(SemanticActions *self, pANTLR3_COMMON_TOKEN predToken, HsStablePtr t);

HsStablePtr _saIsZero(SemanticActions *self, pANTLR3_COMMON_TOKEN isZeroToken, HsStablePtr t);

HsStablePtr _saAppTermTail(SemanticActions *self, HsStablePtr pathTerm, HsStablePtr appTermTail);

HsStablePtr _saPathTerm(SemanticActions *self, HsStablePtr aTerm, HsStablePtr pathTermTail);

HsStablePtr _saPathTermTail(SemanticActions *self, HsStablePtr pathTermRhs, HsStablePtr pathTermTail);

HsStablePtr _saDotLcid(SemanticActions *self, pANTLR3_COMMON_TOKEN dotToken, pANTLR3_COMMON_TOKEN lcidToken);

HsStablePtr _saDotIntV(SemanticActions *self, pANTLR3_COMMON_TOKEN dotToken, pANTLR3_COMMON_TOKEN intVToken);

HsStablePtr _saTrue(SemanticActions *self, pANTLR3_COMMON_TOKEN trueToken);

HsStablePtr _saFalse(SemanticActions *self, pANTLR3_COMMON_TOKEN falseToken);

HsStablePtr _saLcid(SemanticActions *self, pANTLR3_COMMON_TOKEN lcidToken);

HsStablePtr _saFieldsInCurlyBraces(SemanticActions *self, pANTLR3_COMMON_TOKEN leftCurlyToken, HsStablePtr fields);

HsStablePtr _saFloatV(SemanticActions *self, pANTLR3_COMMON_TOKEN floatVToken);

HsStablePtr _saStringV(SemanticActions *self, pANTLR3_COMMON_TOKEN stringVToken);

HsStablePtr _saIntV(SemanticActions *self, pANTLR3_COMMON_TOKEN intVToken);

HsStablePtr _saFieldListNew(SemanticActions *self, HsStablePtr f);

HsStablePtr _saFieldListAppend(SemanticActions *self, HsStablePtr fs, HsStablePtr f);

HsStablePtr _saLcidEqTerm(SemanticActions *self, pANTLR3_COMMON_TOKEN lcidToken, HsStablePtr term);

HsStablePtr _saFieldTerm(SemanticActions *self, HsStablePtr term);

#endif
