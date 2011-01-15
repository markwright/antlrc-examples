/* Copyright (c)2010-2011, Mark Wright.  All rights reserved. */

#include "HsFFI.h"

#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "antlr3lexer.h"

#include "FullUntypedLexer.h"
#include "FullUntypedParser.h"

#ifdef __GLASGOW_HASKELL__
#include "Semantic_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_Semantic(void);
#endif

typedef struct ProgramOptions_struct
{
  bool logDSPs;
  bool logDSPStackTrace;
  bool logDSPRuleInvocationStack;
  bool logDSPCached;
} ProgramOptions;

void programOptionsDefaultConstructor(ProgramOptions *self)
{
  self->logDSPs = false;
  self->logDSPStackTrace = false;
  self->logDSPRuleInvocationStack = false;
  self->logDSPCached = false;
}

int processFile(ProgramOptions *opts, const char *fileName)
{
  if (fileName == (const char *)0)
  {
    printf("ERROR: fileName null pointer\n");
    return 1;
  }
  
  pANTLR3_INPUT_STREAM input = antlr3AsciiFileStreamNew((pANTLR3_UINT8)fileName);
  if (input == (pANTLR3_INPUT_STREAM)0)
  {
    printf("ERROR: antlr3AsciiFileStreamNew %s failed: %s\n", fileName, strerror(errno));
    return 1;
  }

  pFullUntypedLexer lexer = FullUntypedLexerNew(input);
  if (lexer == (pFullUntypedLexer)0)
  {
    printf("ERROR: CLexerNew failed\n");
    input->free(input);
    return 1;
  }

  pANTLR3_COMMON_TOKEN_STREAM tstream = antlr3CommonTokenStreamSourceNew(ANTLR3_SIZE_HINT, TOKENSOURCE(lexer));
  if (tstream == (pANTLR3_COMMON_TOKEN_STREAM)0)
  {
    printf("ERROR: antlr3CommonTokenStreamSourceNew failed\n");
    lexer->free(lexer);
    input->free(input);
    return 1;
  }

  pFullUntypedParser parser = FullUntypedParserNew(tstream);
  if (parser == (pFullUntypedParser)0)
  {
    printf("ERROR: FullUntypedParserNew failed\n");
    tstream->free(tstream);
    lexer->free(lexer);
    input->free(input);
    return 1;
  }

  pSemanticActions semanticActions = malloc(sizeof(SemanticActions));
  if (semanticActions == (pSemanticActions)0)
  {
    printf("ERROR: malloc SemanticActions failed\n");
    parser->free(parser);
    tstream->free(tstream);
    lexer->free(lexer);
    input->free(input);
    return 1;
  }
  semanticActionsContructor(semanticActions,
                            parser,
                            lexer,
                            opts->logDSPs,
                            opts->logDSPStackTrace,
                            opts->logDSPRuleInvocationStack,
                            opts->logDSPCached);
 
  parser->toplevel(parser, semanticActions);
  free(semanticActions);
  parser->free(parser);
  tstream->free(tstream);
  lexer->free(lexer);
  input->free(input);
  return 0;
}

int printUsage(const char *prog_pathname)
{
  printf("%s usage: [GHC flags] [-logDSPs] [-logDSPStackTrace] [-logDSPRuleInvocationStack] [-logDSPCached]", prog_pathname);
  return 1;
}

int processArgs(int argc, const char *argv[], ProgramOptions *opts, int *filesc, const char *filesv[])
{
  *filesc = 0;
  
  if (argc <= 1)
    return printUsage(argv[0]);
  
  for (int i = 1; i < argc; i++)
  {
    const char *arg = argv[i];
    if (strcmp("-logDSPs", arg) == 0)
      opts->logDSPs = true;
    else if (strcmp("-logDSPStackTrace", arg) == 0)
      opts->logDSPStackTrace = true;
    else if (strcmp("-logDSPRuleInvocationStack", arg) == 0)
    {
      printf("pANTLR3_STACK getRuleInvocationStack(pANTLR3_BASE_RECOGNIZER recognizer) is not\n implmentrf, it just returns 0.  Setting logDSPStackTrace instead.\n");
      opts->logDSPStackTrace = true;
      opts->logDSPRuleInvocationStack = true; // Has no effect, not currently implemented in the C target.
    }
    else if (strcmp("-logDSPCached", arg) == 0)
      opts->logDSPCached = true;
    else if (arg[0] == '-')
    {
      printf("invalid option: %s", arg);
      return 1;
    }
    else
    {
      filesv[*filesc] = arg;
      ++(*filesc);
    }
  }
  return 0;
}
  
int ANTLR3_CDECL
main (int argc, const char *argv[])
{
  hs_init(&argc, (char***)&argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_Semantic);
  /* hs_add_root(__stginit_prettyzm1zi0zi1zi1_TextziPrettyPrintziHughesPJ_); */
  /* hs_add_root(__stginit_FullUntypedInterface); */
  /* hs_add_root(__stginit_Lexer); */
  /* hs_add_root(__stginit_C2HS); */
#endif

  ProgramOptions *opts = malloc(sizeof(ProgramOptions));
  programOptionsDefaultConstructor(opts);
  int filesc = 0;
  const char **filesv = calloc(argc, sizeof(const char *));
  int retVal = processArgs(argc, argv, opts, &filesc, filesv);
  
  for (int i = 0; (retVal == 0) && (i < filesc); ++i)
    retVal = processFile(opts, filesv[i]);
  
  free(opts);
  free(filesv);
  hs_exit();  // shutdownHaskellAndExit();
  return retVal;
}

