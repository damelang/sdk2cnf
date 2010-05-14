default: sdk2cnf sol2sdk

%: %.hs
	ghc -O -o $@ $^ 

clean:
	rm -f *.o *.hi sdk2cnf sol2sdk *.cnf *.sol
