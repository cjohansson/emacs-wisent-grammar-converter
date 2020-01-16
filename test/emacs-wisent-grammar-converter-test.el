;;; emacs-wisent-grammar-converter-test.el --- Tests for Emacs Wisent Grammar Converter -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 Christian Johansson

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


;;; Code:

(require 'emacs-wisent-grammar-converter)
(require 'ert)

(message " ")
(message "Unit tests started")

;; White-space
(should (equal "random-statement" (emacs-wisent-grammar/reformat-logic-block "	random-statement;  	\n\n")))

;; Return a argument
(should (equal "$3" (emacs-wisent-grammar/reformat-logic-block "  $$ = $3;  	\n\n")))

;; Function calls with arguments
(should (equal "(zend_ast_create ZEND_AST_EMPTY $3)" (emacs-wisent-grammar/reformat-logic-block "  zend_ast_create(ZEND_AST_EMPTY, $3);  	\n\n")))

;; Function calls without arguments
(should (equal "(zend_ast_create)" (emacs-wisent-grammar/reformat-logic-block "  zend_ast_create();  	\n\n")))

;; NULL values like    ($$ = NULLABLE)
(should (equal "$$ = nil" (emacs-wisent-grammar/reformat-logic-block "  $$ = NULL;  	\n\n")))

;; Attribute assignments like    $$->attr = ZEND_NAME_NOT_FQ;
(should (equal "(put $$ 'attr 'ZEND_NAME_NOT_FQ)" (emacs-wisent-grammar/reformat-logic-block "  $$->attr = ZEND_NAME_NOT_FQ;  	\n\n")))

;; TODO Syntactic sugar like    1 ? 2 : 0

;; TODO Place return statements last in block    $$ = ...

;; Logical or like    $1 | $2
(should (equal "(logior $1 $2)" (emacs-wisent-grammar/reformat-logic-block "  $1 | $2  	\n\n")))

;; TODO Function assignments like     (CG extra_fn_flags) = 0

;; TODO Logical or assignment like    (CG extra_fn_flags) |= ZEND_ACC_GENERATOR)

;; TODO Dereferenced pointers like    (zend_ast *decl

;; Doc comments like    /* allow single trailing comma */ (zend_ast_list_rtrim $1)
(should (equal ";; allow single trailing comma\n(zend_ast_list_rtrim $1)" (emacs-wisent-grammar/reformat-logic-block "/* allow single trailing comma */ (zend_ast_list_rtrim $1)")))

;; Return function call
(should (equal "(zend_ast_create ZEND_AST_EMPTY $3)" (emacs-wisent-grammar/reformat-logic-block "  $$ = zend_ast_create(ZEND_AST_EMPTY, $3);  	\n\n")))


(message "Unit tests completed")

(provide 'emacs-wisent-grammar-converter-test)
;;; emacs-wisent-grammar-converter-test.el ends here