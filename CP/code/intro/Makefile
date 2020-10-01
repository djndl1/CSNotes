CSC=mcs
MONO=mono
CSHARP_FLAGS = -debug -sdk:4.7.2-api -langversion:7

CXX=clang++
CFLAGS=-g -std=c++11
LDFLAGS=-lm -DONLINE_JUDGE

EXE = 1-2-3.1.exe \
      uva_11799.exe \
      uva_11942.exe

CC_ELF = uva_11799.cc.elf \
      uva_11942.cc.elf \
	    uva_12468.cc.elf \
			uva_12554.cc.elf \
			uva_573.cc.elf \
			uva_661.cc.elf \
			uva_10141.cc.elf

all: $(EXE) $(CC_ELF)

%.exe: %.cs
	$(CSC) $< $(CSHARP_FLAGS) -out:$@

%.cc.elf: %.cc
	$(CXX) $< $(CFLAGS) $(LDFLAGS) -o $@

.PHONY:
clean:
	@echo "Cleaning"
	@rm -v $(EXE) $(CC_ELF)
	@rm -v *.mdb
	@rm -v *.class
