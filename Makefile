all:
	ghc --make dka-2-mka.hs

clean:
	rm -f dka-2-mka{,.hi,.o}
