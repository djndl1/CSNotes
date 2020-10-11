CSC=mcs
MONO=mono
CSHARP_FLAGS = -debug -sdk:4.7.2-api -langversion:7 -optimize

JAVAC=javac8
JAVA_FLAGS=-g

CXX=/usr/bin/g++
CFLAGS=-g -std=c++11
LDFLAGS=-lm -DONLINE_JUDGE

DOTNET_EXE = uva11222.exe \
	  uva11988.exe \
	  uva146.exe \
	  uva10107.exe

CC_ELF = uva11222.celf \
		 uva11988.celf \
		 uva514.celf \
		 uva146.celf \
		 uva10107.celf \
		 uva10258.celf

JAVA = uva10107.class

all: $(DOTNET_EXE) $(CC_ELF)

csharp: $(DOTNET_EXE)

cimpl: $(CC_ELF)

java: $(JAVA)

%.exe: %.cs
	$(CSC) $< $(CSHARP_FLAGS) -out:$@

%.celf: %.cc
	$(CXX) $< $(CFLAGS) $(LDFLAGS) -o $@

%.class: %.java
	$(JAVAC) $(JAVA_FLAGS) $<

.PHONY: clean
clean:
	@echo "Cleaning"
	-rm -f $(DOTNET_EXE) $(CC_ELF) $(JAVA)
	-rm -f *.mdb
	-rm -f *.class
	-rm -f a.out
