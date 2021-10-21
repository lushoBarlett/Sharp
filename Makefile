GHC   := ghc
BUILD := build
BIN   := bin
SRC   := src

ODIR  := $(BUILD)
HIDIR := $(BUILD)

CRISP := crisp

$(CRISP): $(SRC)/Main.hs
	ghc --make Main -i$(SRC) -odir $(ODIR) -hidir $(HIDIR) -o $(BIN)/$(CRISP)

.PHONY: Main.hs
$(SRC)/Main.hs:

.PHONY: clean
clean:
	rm -r $(BUILD)
