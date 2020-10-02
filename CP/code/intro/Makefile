CSC=mcs
MONO=mono
CSHARP_FLAGS = -debug -sdk:4.7.2-api -langversion:7

CXX=clang++
CFLAGS=-g -std=c++11
LDFLAGS=-lm -DONLINE_JUDGE

EXE = 1-2-3.1.exe \
      uva_11799.exe \
      uva_11942.exe

CC_ELF = uva_11799.celf \
      uva_11942.celf \
	    uva_12468.celf \
			uva_12554.celf \
			uva_573.celf \
			uva_661.celf \
			uva_10141.celf \
			uva10324.celf \
			uva119.celf \
			uva10424.celf \
			uva394.celf

all: $(EXE) $(CC_ELF)

%.exe: %.cs
	$(CSC) $< $(CSHARP_FLAGS) -out:$@

%.celf: %.cc
	$(CXX) $< $(CFLAGS) $(LDFLAGS) -o $@

.PHONY:
clean:
	@echo "Cleaning"
	@rm -v $(EXE) $(CC_ELF)
	@rm -v *.mdb
	@rm -v *.class
