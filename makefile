EMACS = "emacs"
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := test/emacs-wisent-grammar-converter-test.el

test:
	$(EMACS_CMD) -l test/emacs-wisent-grammar-converter-test.el
