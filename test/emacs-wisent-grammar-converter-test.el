;;; emacs-wisent-grammar-converter-test.el --- Tests for Emacs Wisent Grammar Converter -*- lexical-binding:t -*-

;; Copyright (C) 2018-2020 Christian Johansson

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Spathoftware Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Run from terminal with `make test'


;; TODO Replace all terminals in rules with symbols that can be user-defined
;; TODO Add unit tests for formatting rules


;;; Code:

(require 'emacs-wisent-grammar-converter)
(require 'ert)

(message "\nUnit tests for integration started\n")

(defun emacs-wisent-grammar-converter-test--reformat-logic-block()
  "Test conversion of C to Wisent Emacs-Lisp."

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$$ = zend_ast_append_str($1, $3);")
    "(let ((r)) (setq r (ZEND_AST_APPEND_STR $1 $3)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 1")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$$ = zend_ast_create(ZEND_AST_HALT_COMPILER,
			      zend_ast_create_zval_from_long(zend_get_scanned_file_offset()));
			  zend_stop_lexing();")
    "(let ((r)) (setq r (ZEND_AST_CREATE 'ZEND_AST_HALT_COMPILER (ZEND_AST_CREATE_ZVAL_FROM_LONG (ZEND_GET_SCANNED_FILE_OFFSET))))(ZEND_STOP_LEXING) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 2")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "			zval zv;
			zend_lex_tstring(&zv);
			$$ = zend_ast_create_zval(&zv);
")
    "(let ((r)(zv)) (ZEND_LEX_TSTRING (lambda(return) (setq zv return)))(setq r (ZEND_AST_CREATE_ZVAL zv)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 3")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = zend_ast_list_add($1, $3); ")
    "(let ((r)) (setq r (ZEND_AST_LIST_ADD $1 $3)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 4")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = NULL; zend_throw_exception(zend_ce_compile_error,
			      \"__HALT_COMPILER() can only be used from the outermost scope\", 0); YYERROR; ")
    "(let ((r)) (setq r nil)(ZEND_THROW_EXCEPTION zend_ce_compile_error \"__HALT_COMPILER() can only be used from the outermost scope\" 0)(setq r 'YYERROR) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 5")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = zend_ast_create_decl(ZEND_AST_METHOD, $3 | $1 | $12, $2, $5,
				  zend_ast_get_str($4), $7, NULL, $11, $9); CG(extra_fn_flags) = $10; ")
    "(let ((r)) (setq r (ZEND_AST_CREATE_DECL 'ZEND_AST_METHOD (logior $3 (logior $1 $12)) $2 $5 (ZEND_AST_GET_STR $4) $7 nil $11 $9))(CG 'extra_fn_flags $10) r)"))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 6")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = zend_add_class_modifier($1, $2); if (!$$) { YYERROR; }")
    "(let ((r)) (setq r (ZEND_ADD_CLASS_MODIFIER $1 $2))(if (not r) (setq r 'YYERROR)) r)"))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 7")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "			$$ = $2;
			if ($$->kind == ZEND_AST_CONDITIONAL) { $$->attr = ZEND_PARENTHESIZED_CONDITIONAL; }
		")
    "(let ((r)) (setq r $2)(if (equal (semantic-tag-get-attribute r 'kind) 'ZEND_AST_CONDITIONAL) (semantic-tag-put-attribute r 'attr 'ZEND_PARENTHESIZED_CONDITIONAL)) r)"))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 8")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "			$$ = $2;
			if ($$->kind == ZEND_AST_CONDITIONAL) $$->attr = ZEND_PARENTHESIZED_CONDITIONAL;
		")
    "(let ((r)) (setq r $2)(if (equal (semantic-tag-get-attribute r 'kind) 'ZEND_AST_CONDITIONAL) (semantic-tag-put-attribute r 'attr 'ZEND_PARENTHESIZED_CONDITIONAL)) r)"))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 9")

  )

(emacs-wisent-grammar-converter-test--reformat-logic-block)

(message "\nUnit tests for integration completed\n")

(provide 'emacs-wisent-grammar-converter-test)
;;; emacs-wisent-grammar-converter-test.el ends here
