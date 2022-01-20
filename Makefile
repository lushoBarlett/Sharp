GHC   := ghc
BUILD := build
BIN   := bin
SRC   := src

ODIR  := $(BUILD)
HIDIR := $(BUILD)

SHARP := sharp

$(SHARP): $(SRC)/Main.hs
	@mkdir -p $(ODIR)
	@mkdir -p $(HIDIR)
	@mkdir -p $(BIN)
	ghc --make Main -i$(SRC) -odir $(ODIR) -hidir $(HIDIR) -o $(BIN)/$(SHARP)

.PHONY: Main.hs
$(SRC)/Main.hs:

.PHONY: clean
clean:
	rm -r $(BUILD)
	rm -r $(BIN)
