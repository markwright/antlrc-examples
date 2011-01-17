C2HS=c2hs
HC=ghc
HC_FLAGS=-fwarn-name-shadowing -optc -std=gnu99

ANTLR_CLASSPATH='/h/argus/2/j/antlr3/antlr-3.2/target/antlr-master-3.2-completejar.jar'

C_SOURCE_FILES=FullUntypedParser.c \
	FullUntypedLexer.c \
	FullUntyped.c \
	SemanticActions.c \
	Semantic_stub.c

HS_OBJECT_FILES=C2HS.o Semantic.o FullUntypedInterface.o

fulluntyped:	$(C_SOURCE_FILES) $(HS_OBJECT_FILES)
	$(HC) $(HC_FLAGS) --make -o fulluntyped $(HS_OBJECT_FILES) $(C_SOURCE_FILES) -lantlr3c \
	-L/usr/lib64/ghc-7.0.1/pretty-1.0.1.2 -lHSpretty-1.0.1.2 \
	-L/usr/lib64/antlrc-9999/ghc-7.0.1 -lHSantlrc-0.0.1 \
	-L/usr/lib64/ghc-7.0.1/haskell98-1.1.0.0 -lHShaskell98-1.1.0.0

FullUntypedTokens.h: FullUntyped.tokens
	antlrcmkenums FullUntyped.tokens

FullUntypedParser.c FullUntyped.tokens:	FullUntyped.g
	java -Xmx512m -classpath $(ANTLR_CLASSPATH) org.antlr.Tool -Xconversiontimeout 600000 -report FullUntyped.g

C2HS.hs:	/usr/bin/c2hs
	$(C2HS) --copy-library -v

C2HS.o:	C2HS.hs
	$(HC) $(HC_FLAGS) $(HC_FFI_FLAGS) -c C2HS.hs

FullUntypedInterface.hs:	FullUntypedInterface.chs FullUntypedTokens.h
	$(C2HS) --include=/usr/lib64/antlrc-9999/ghc-7.0.1 FullUntypedInterface.chs

Semantic.o Semantic_stub.h Semantic_stub.c Semantic_stub.o:	Semantic.hs FullUntypedInterface.o
	$(HC) $(HC_FLAGS) -ignore-package monads-fd -ignore-package monads-tf -c Semantic.hs

FullUntypedInterface.o:	FullUntypedInterface.hs
	$(HC) $(HC_FLAGS) -c FullUntypedInterface.hs

FullUntyped.o:	FullUntyped.c SemanticActions.h Semantic_stub.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c FullUntyped.c

SemanticActions.o:	SemanticActions.c SemanticActions.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c SemanticActions.c

FullUntypedParser.o:	FullUntypedParser.c SemanticActions.h Semantic_stub.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c FullUntypedParser.c

FullUntypedLexer.o:	FullUntypedLexer.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c FullUntypedLexer.c

lexerc.o:	lexerc.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c lexerc.c

clean:
	rm *.o FullUntypedLexer.c FullUntypedParser.c
