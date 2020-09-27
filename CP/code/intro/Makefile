CSC=mcs
MONO=mono
CSHARP_FLAGS = -debug -sdk:4.7.2-api -langversion:7

CXX=clang++
CFLAGS=-g -std=c++11
LDFLAGS=-lm -DONLINE_JUDGE

EXE = 1-2-3.1.exe \
      uva_11799.exe \
      uva_11942.exe

ELF = uva_11799.elf \
      uva_11942.elf

all: $(EXE) $(ELF)

%.exe: %.cs
	$(CSC) $< $(CSHARP_FLAGS) -out:$@

%.elf: %.cc
	$(CXX) $< $(CFLAGS) $(LDFLAGS) -o $@

.PHONY:
clean:
	@echo "Cleaning"
	@rm -v $(EXE) $(ELF)
	@rm -v *.mdb