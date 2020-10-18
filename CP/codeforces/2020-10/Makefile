CSC=csc
CSHARP_FLAGS = -debug -langversion:8 -d:ONLINE_JUDGE -r:System.Numerics.dll

CXX=/usr/bin/g++
CFLAGS=-g -std=c++17 -DONLINE
LDFLAGS=-static -lm -DONLINE_JUDGE

JAVAC=javac
JAVA_FLAGS=-g -Xlint

EXE = 1424G_2.exe

CC_ELF = 1424G_2.celf

JAVA = CF1424G.class

all: $(EXE) $(CC_ELF) $(JAVA)

csharp: $(EXE)

cimpl: $(CC_ELF)

%.exe: %.cs
	$(CSC) $< $(CSHARP_FLAGS) -out:$@

%.celf: %.cc
	$(CXX) $< $(CFLAGS) $(LDFLAGS) -o $@

%.class: %.java
	$(JAVAC) $< $(JAVA_FLAGS)

.PHONY: clean
clean:
	@echo "Cleaning"
	-rm -f $(EXE) $(CC_ELF)
	-rm -f *.mdb
	-rm -f *.pdb
	-rm -f *.class
	-rm -f a.out
