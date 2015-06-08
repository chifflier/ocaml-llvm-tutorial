SRC_DIR:=src

TOOLS:=tutorial01

#TARGET:=native
TARGET:=byte

LLVM_VERSION := 3.5

OCAMLBUILDFLAGS:=-classic-display -j 0 -cflags -w,@a-4

export OCAMLPATH=/usr/lib/ocaml/llvm-$(LLVM_VERSION)

tutorial01_OCAMLBUILDFLAGS:=-use-ocamlfind -pkgs llvm,llvm.bitreader




################
OCAMLBUILD:=ocamlbuild



CLEAN_RULES:=$(patsubst %,%-clean,$(TOOLS))



.PHONY: $(TOOLS) clean $(CLEAN_RULES) default run

default: $(TOOLS)

$(TOOLS):
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $($@_OCAMLBUILDFLAGS) $($@_OCAMLBUILDFLAGS_$(TARGET)) -I $(SRC_DIR) -build-dir build/$@ $@.$(TARGET)

run: $(TOOLS)
	CAML_LD_LIBRARY_PATH=/usr/lib/ocaml/llvm-$(LLVM_VERSION) ./build/llvmBCTargetTriple/src/llvmBCTargetTriple.byte

clean: $(CLEAN_RULES)

$(CLEAN_RULES):
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -I $(SRC_DIR) -build-dir build/$(patsubst %-clean,%,$@) -clean $(patsubst %-clean,%,$@).$(TARGET)


