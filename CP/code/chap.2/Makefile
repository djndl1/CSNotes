CSC=mcs
MONO=mono
CSHARP_FLAGS = -debug -sdk:4.7.2-api -langversion:7

CXX=/usr/bin/g++
CFLAGS=-g -std=c++11
LDFLAGS=-lm -DONLINE_JUDGE

EXE = uva11222.exe \
	  uva11988.exe \
	  uva146.exe

CC_ELF = uva11222.celf \
		 uva11988.celf \
		 uva514.celf \
		 uva146.celf \
		 uva10107.celf

all: $(EXE) $(CC_ELF)

csharp: $(EXE)

cimpl: $(CC_ELF)

%.exe: %.cs
	$(CSC) $< $(CSHARP_FLAGS) -out:$@

%.celf: %.cc
	$(CXX) $< $(CFLAGS) $(LDFLAGS) -o $@

.PHONY: clean
clean:
	@echo "Cleaning"
	-rm -f $(EXE) $(CC_ELF)
	-rm -f *.mdb
	-rm -f *.class
	-rm -f a.out
