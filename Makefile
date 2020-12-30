DUNE = dune

main:
	(make clean) && $(DUNE) build @install && $(DUNE) install

clean: 
	$(DUNE) clean