bnfc:
	bnfc --haskell grammar/Gerber.cf -p Data -d -o src/
	sed -i '' 's/Main/Data\.Gerber\.Test/g' src/Data/Gerber/Test.hs
