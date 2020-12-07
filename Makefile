MAIN := bin/boa
OBJS := bin/ast.cmo bin/lexer.cmo bin/parser.cmo bin/pprint.cmo bin/eval.cmo bin/check.cmo bin/main.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

$(MAIN): $(OBJS)
	ocamlc -o $@ $^

src/lexer.ml: src/lexer.mll
	ocamllex -q $<

src/parser.ml: src/parser.mly
	ocamlyacc -v $<

src/parser.mli: src/parser.mly
	ocamlyacc -v $<

clean:
	rm -f *.cmo *.cmi src/lexer.ml src/parser.ml src/parser.mli $(MAIN)

# Dependencies generated by `ocamldep -bytecode *.mli *.ml`.
bin/ast.cmo :
bin/check.cmo : bin/ast.cmo
bin/eval.cmo : bin/ast.cmo bin/pprint.cmo
bin/lexer.cmo : bin/parser.cmi
bin/main.cmo : bin/pprint.cmo bin/parser.cmi bin/lexer.cmo bin/eval.cmo bin/check.cmo bin/ast.cmo
bin/parser.cmo : bin/ast.cmo bin/parser.cmi
bin/parser.cmi : bin/ast.cmo
bin/pprint.cmo : bin/ast.cmo