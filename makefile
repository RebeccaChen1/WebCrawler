all: askshiebs crawl time_test askshiebs_tests

askshiebs: askshiebs.ml
	ocamlbuild -use-ocamlfind askshiebs.byte

askshiebs_tests: askshiebs_tests.ml
	ocamlbuild -use-ocamlfind askshiebs_tests.byte

crawl: crawl.ml
	ocamlbuild -use-ocamlfind crawl.byte

time_test: time_test.ml
	ocamlbuild -use-ocamlfind time_test.byte

clean:
	rm -rf *.byte _build
