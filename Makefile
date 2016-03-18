#
# Compiles all challenges
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = 		corebuild $(OCB_FLAGS)
SOURCES = chal4.native chal6.native chal7.native chal8.native chal10.native chal11.native chal12.native chal13.native

all: 		native # profile debug

clean:
			$(OCB) -clean

native: 	#sanity
			$(OCB) $(SOURCES)
			# $(OCB) .native

byte:		sanity
			$(OCB) *.byte

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
