#
# Compiles all challenges
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = 		corebuild $(OCB_FLAGS)
SOURCES = chal4 chal6 chal7 chal8 chal10 chal11 chal12 chal13
NATIVE = $(SOURCES:=.native)
BYTE = $(SOURCES:=.byte)
all: 		native # profile debug

clean:
			$(OCB) -clean

native: 	#sanity
			$(OCB) $(SOURCES:=.native)

byte:		sanity
			$(OCB) $(SOURCES:=.byte)

profile: 	sanity
			$(OCB) -tag profile $(SOURCES).native

debug: 		sanity
			$(OCB) -tag debug $(SOURCES).byte

sanity:
# check that packages can be found
			ocamlfind query core
			ocamlfind query cryptokit

# test: 		native
# 			echo '[1, 2, "three", {"four": 4}]' | ./main.native

.PHONY: 	all clean byte native profile debug sanity test
