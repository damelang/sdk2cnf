default: sdk2cnf sol2sdk sdk2cnf-2 sol2sdk-2

%: %.hs
	ghc -O -o $@ $^ 

clean:
	rm -f *.o *.hi sdk2cnf sol2sdk sol2sdk-2 sdk2cnf-2 *.cnf *.sol
