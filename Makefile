all: demo

demo:
	stack build && stack exec -- drawgerber --width=400 -o test.pdf

bnfc:
	bnfc --haskell grammar/GerberAST.cf -p Data.Gerber -d -o src/
	sed -i '' 's/Main/Data\.Gerber\.GerberAST\.Test/g' src/Data/Gerber/GerberAST/Test.hs
	rm src/Data/Gerber/GerberAST/*.bak

benchmark:
	mkdir -p benchmark/dataset benchmark/output
	for i in `seq 100 1000 10000`; \
		do rm -vf benchmark/dataset/$$i.gbr ; \
		stack exec -- generategerber --count $$i benchmark/dataset/$$i.gbr ; \
		time stack exec -- drawgerber benchmark/dataset/$$i.gbr benchmark/output/$$i.svg +RTS -N8 ; \
		done

.PROHY: benchmark
