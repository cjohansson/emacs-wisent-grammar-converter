EMACS = "emacs"
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := test.el

test:
	$(EMACS_CMD) -l test.el
