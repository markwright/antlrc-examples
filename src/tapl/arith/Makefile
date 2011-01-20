C2HS=c2hs
HC=ghc
HC_FLAGS=-fwarn-name-shadowing -optc -std=gnu99

ANTLR_CLASSPATH='/h/argus/2/j/antlr3/antlr-3.2/target/antlr-master-3.2-completejar.jar'

C_SOURCE_FILES=ArithParser.c \
	ArithLexer.c \
	Arith.c \
	SemanticActions.c \
	Semantic_stub.c

HS_OBJECT_FILES=C2HS.o Semantic.o ArithInterface.o

arith:	$(C_SOURCE_FILES) $(HS_OBJECT_FILES)
	$(HC) $(HC_FLAGS) --make -o arith $(HS_OBJECT_FILES) $(C_SOURCE_FILES) -lantlr3c \
	-L/usr/lib64/ghc-7.0.1/pretty-1.0.1.2 -lHSpretty-1.0.1.2 \
	-L/usr/lib64/antlrc-9999/ghc-7.0.1 -lHSantlrc-0.0.2 \
	-L/usr/lib64/ghc-7.0.1/haskell98-1.1.0.0 -lHShaskell98-1.1.0.0

ArithTokens.h: Arith.tokens
	antlrcmkenums Arith.tokens

ArithParser.c Arith.tokens:	Arith.g
	java -Xmx512m -classpath $(ANTLR_CLASSPATH) org.antlr.Tool -Xconversiontimeout 600000 -report Arith.g

C2HS.hs:	/usr/bin/c2hs
	$(C2HS) --copy-library -v

C2HS.o:	C2HS.hs
	$(HC) $(HC_FLAGS) $(HC_FFI_FLAGS) -c C2HS.hs

ArithInterface.hs:	ArithInterface.chs ArithTokens.h
	$(C2HS) --include=/usr/lib64/antlrc-9999/ghc-7.0.1 ArithInterface.chs

Semantic.o Semantic_stub.h Semantic_stub.c Semantic_stub.o:	Semantic.hs ArithInterface.o
	$(HC) $(HC_FLAGS) -ignore-package monads-fd -ignore-package monads-tf -c Semantic.hs

ArithInterface.o:	ArithInterface.hs
	$(HC) $(HC_FLAGS) -c ArithInterface.hs

Arith.o:	Arith.c SemanticActions.h Semantic_stub.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c Arith.c

SemanticActions.o:	SemanticActions.c SemanticActions.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c SemanticActions.c

ArithParser.o:	ArithParser.c SemanticActions.h Semantic_stub.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ArithParser.c

ArithLexer.o:	ArithLexer.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ArithLexer.c

lexerc.o:	lexerc.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c lexerc.c

clean:
	rm -f *.o ArithLexer.c ArithParser.c ArithTokens.h

