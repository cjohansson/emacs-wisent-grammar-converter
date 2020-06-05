EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L .

.PHONY: test
test:
	$(EMACS_CMD) -l test/emacs-wisent-grammar-converter-test-lexer.el -l test/emacs-wisent-grammar-converter-test-parser.el -l test/emacs-wisent-grammar-converter-test.el
