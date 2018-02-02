all: demo

demo:
	stack build && stack exec -- drawgerber --width=400 -o test.pdf

bnfc:
	bnfc --haskell grammar/GerberAST.cf -p Data.Gerber -d -o src/
	sed -i '' 's/Main/Data\.Gerber\.GerberAST\.Test/g' src/Data/Gerber/GerberAST/Test.hs
	rm src/Data/Gerber/GerberAST/*.bak
