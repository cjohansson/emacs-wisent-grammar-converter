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


;;; Code:

(require 'emacs-wisent-grammar-converter)
(require 'ert)

(message "\nUnit tests started\n")

(defun emacs-wisent-grammar-converter-test--reformat-logic-block ()
  "Test `emacs-wisent-grammar-converter--reformat-logic-block'."

  ;; Test function and variable prefix
  (should (equal
           "(random-random-statement)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "	random-statement();  	\n\n" "random-")))

  ;; White-space
  (should (equal
           "random-statement"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "	random-statement;  	\n\n")))

  ;; Return a argument
  (should (equal
           "$3"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  $$ = $3;  	\n\n")))

  ;; Function calls with arguments
  (should (equal
           "(zend_ast_create ZEND_AST_EMPTY $3)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  zend_ast_create(ZEND_AST_EMPTY, $3);  	\n\n")))

  ;; Function calls without arguments
  (should (equal
           "(zend_ast_create)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  zend_ast_create();  	\n\n")))

  ;; NULL values like    ($$ = NULLABLE)
  (should (equal
           "$$ = nil"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  $$ = NULL;  	\n\n")))

  ;; Attribute assignments like    $$->attr = ZEND_NAME_NOT_FQ;
  (should (equal
           "(put $$ 'attr 'ZEND_NAME_NOT_FQ)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  $$->attr = ZEND_NAME_NOT_FQ;  	\n\n")))

  ;; Logical or like    $1 | $2
  (should (equal
           "(logior $1 $2)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  $1 | $2  	\n\n")))

  ;; Doc comments like    /* allow single trailing comma */ (zend_ast_list_rtrim $1)
  (should (equal
           ";; allow single trailing comma\n(zend_ast_list_rtrim $1)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "/* allow single trailing comma */ (zend_ast_list_rtrim $1)")))

  ;; Return function call
  (should (equal
           "(zend_ast_create ZEND_AST_EMPTY $3)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  $$ = zend_ast_create(ZEND_AST_EMPTY, $3);  	\n\n")))

  ;; TODO Syntactic sugar like    1 ? 2 : 0
  (should (equal
           "(if 1 2 0)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  1 ? 2 : 0  	\n\n")))

  ;; TODO Place return statements last in block    $$ = ...
  (should (equal
           "(a)(b)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "  $$ = b(); a();")))

  ;; TODO Function assignments like     (CG extra_fn_flags) = 0 -> (CG extra_fn_lags 0)
  (should (equal
           "(CG exra_fn_lags 0)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "CG(extra_fn_lags) = 0;")))

  ;; TODO Logical or assignment like    (CG extra_fn_flags) |= ZEND_ACC_GENERATOR -> (CG extra_fn_flags (bitwise-or (CG extra_fn_lags)))
  (should (equal
           "(CG exra_fn_lags (bitwise-or (CG extra_fn_lags) ZEND_ACC_GENERATOR))"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "	(CG extra_fn_lags) |= ZEND_ACC_GENERATOR")))

  ;; TODO Dereferenced pointers like    (zend_ast *decl ?
  (should (equal
           "(symbol-value SYMBOL)"
           (emacs-wisent-grammar-converter--reformat-logic-block
            "	*SYMBOL")))

  ;; TODO zend_string_init("closure) (", sizeof("closure)") - 1 0) $5 $7 $11 $8) (CG extra_fn_flags) = $9)
  )

(defun emacs-wisent-grammar-converter-test--lex-c-string ()
  "Test `emacs-wisent-grammar-converter-test--lex-c-string'"
  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "	  \nmask();	\n  "
            )
           (list
            (list 'FUNCTION "mask")
            (list 'OPEN_PARENTHESIS "(")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "$$ = $1;"
            )
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'PARAMETER "$1")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "$$ = zend_ast_create_ex(ZEND_AST_INCLUDE_OR_EVAL, ZEND_REQUIRE_ONCE, $2);"
            )
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create_ex")
            (list 'OPEN_PARENTHESIS "(")
            (list 'VARIABLE "ZEND_AST_INCLUDE_OR_EVAL")
            (list 'COMMA ",")
            (list 'VARIABLE "ZEND_REQUIRE_ONCE")
            (list 'COMMA ",")
            (list 'PARAMETER "$2")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";")))
          )

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "zval zv; zend_lex_tstring(&zv); $$ = zend_ast_create(ZEND_AST_TRAIT_ALIAS, $1, zend_ast_create_zval(&zv));"
            )
           (list
            (list 'DECLARATION "zval")
            (list 'VARIABLE "zv")
            (list 'SEMICOLON ";")
            (list 'FUNCTION "zend_lex_tstring")
            (list 'OPEN_PARENTHESIS "(")
            (list 'REFERENCE "zv")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";")
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create")
            (list 'OPEN_PARENTHESIS "(")
            (list 'VARIABLE "ZEND_AST_TRAIT_ALIAS")
            (list 'COMMA ",")
            (list 'PARAMETER "$1")
            (list 'COMMA ",")
            (list 'FUNCTION "zend_ast_create_zval")
            (list 'OPEN_PARENTHESIS "(")
            (list 'REFERENCE "zv")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))
           ))

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "$$->attr = ZEND_NAME_NOT_FQ;"
            )
           (list
            (list 'RETURN "$$")
            (list 'MEMBER_OPERATOR "->")
            (list 'VARIABLE "attr")
            (list 'ASSIGNMENT "=")
            (list 'VARIABLE "ZEND_NAME_NOT_FQ")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "	  \n$$->attr |= ZEND_TYPE_NULLABLE;	\n  "
            )
           (list
            (list 'RETURN "$$")
            (list 'MEMBER_OPERATOR "->")
            (list 'VARIABLE "attr")
            (list 'BITWISE_OR_ASSIGNMENT "|=")
            (list 'VARIABLE "ZEND_TYPE_NULLABLE")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            " $$ = zend_ast_create(ZEND_AST_PROP_ELEM, $1, NULL, ($2 ? zend_ast_create_zval_from_str($2) : NULL)); ")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create")
            (list 'OPEN_PARENTHESIS "(")
            (list 'VARIABLE "ZEND_AST_PROP_ELEM")
            (list 'COMMA ",")
            (list 'PARAMETER "$1")
            (list 'COMMA ",")
            (list 'NULL "NULL")
            (list 'COMMA ",")
            (list 'OPEN_PARENTHESIS "(")
            (list 'PARAMETER "$2")
            (list 'QUESTION_MARK "?")
            (list 'FUNCTION "zend_ast_create_zval_from_str")
            (list 'OPEN_PARENTHESIS "(")
            (list 'PARAMETER "$2")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'COLON ":")
            (list 'NULL "NULL")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "/* allow single trailing comma */ $$ = zend_ast_list_rtrim($1); ")
           (list
            (list 'DOC_COMMENT "allow single trailing comma")
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_list_rtrim")
            (list 'OPEN_PARENTHESIS "(")
            (list 'PARAMETER "$1")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))

  )

(defun emacs-wisent-grammar-converter-test--converted-lexer-tokens-to-lisp ()
  "Test `emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp'"
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(mask) return-item)"))
  (message "Passed test: function-call without arguments")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "zv")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(mask zv) return-item)"))
  (message "Passed test: function-call with one argument")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "zv")
             (list 'PARAMETER "$2")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((parameter-2 '(value $2))(return-item '(value $$)))(mask zv parameter-2) return-item)"))
  (message "Passed test: function-call with two arguments")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'FUNCTION "mask2")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "zv")
             (list 'COMMA ",")
             (list 'VARIABLE "zv2")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'COMMA ",")
             (list 'VARIABLE "zv3")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(mask (mask2 zv zv2) zv3) return-item)"))
  (message "Passed test: nested function calls")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "zv")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";"))
            "namespace-")
           "(let ((return-item '(value $$)))(namespace-mask namespace-zv) return-item)"))
  (message "Passed test: function-call with argument and namespace")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'PARAMETER "$3")
             (list 'SEMICOLON ";")))
           "(let ((parameter-3 '(value $3))(return-item '(value $$)))(plist-put return-item 'value parameter-3) return-item)"))
  (message "Passed test: assign return-value value of parameter")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'NULL "null")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(plist-put return-item 'value nil) return-item)"))
  (message "Passed test: assign return-value value of nil")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_append_str")
             (list 'OPEN_PARENTHESIS)
             (list 'PARAMETER "$1")
             (list 'COMMA ",")
             (list 'PARAMETER "$3")
             (list 'CLOSE_PARENTHESIS)
             (list 'SEMICOLON ";")))
           "(let ((parameter-3 '(value $3))(parameter-1 '(value $1))(return-item '(value $$)))(plist-put return-item 'value (zend_ast_append_str parameter-1 parameter-3)) return-item)"))
  (message "Passed test: assign return-value function-call with parameter arguments")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'MEMBER_OPERATOR "->")
             (list 'VARIABLE "attr")
             (list 'ASSIGNMENT "=")
             (list 'VARIABLE "ZEND_NAME_NOT_FQ")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(plist-put return-string 'attr 'ZEND_NAME_NOT_FQ) return-item)"))
  (message "Passed test: set attribute of return-item")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'PARAMETER "$1")
             (list 'SEMICOLON ";")
             (list 'RETURN "$$")
             (list 'MEMBER_OPERATOR "->")
             (list 'VARIABLE "attr")
             (list 'ASSIGNMENT "=")
             (list 'VARIABLE "ZEND_NAME_NOT_FQ")
             (list 'SEMICOLON ";")))
           "(let ((parameter-1 '(value $1))(return-item '(value $$)))(plist-put return-item 'value parameter-1)(plist-put return-string 'attr 'ZEND_NAME_NOT_FQ) return-item)"))
  (message "Passed test: set return-item to parameter and then change attribute of return-item")

  ;; Support things like    $$ = $2; $$->attr |= ZEND_TYPE_NULLABLE;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'PARAMETER "$2")
             (list 'SEMICOLON ";")
             (list 'RETURN "$$")
             (list 'MEMBER_OPERATOR "->")
             (list 'VARIABLE "attr")
             (list 'BITWISE_OR_ASSIGNMENT "|=")
             (list 'VARIABLE "ZEND_TYPE_NULLABLE")
             (list 'SEMICOLON ";")))
           "(let ((parameter-2 '(value $2))(return-item '(value $$)))(plist-put return-item 'value parameter-2)(plist-put return-string 'attr (logior (plist-get return-item 'attr) 'ZEND_TYPE_NULLABLE)) return-item)"))
  (message "Passed test: set return-item to parameter and then change attribute with a bitwise-or assignment of return-item")

  ;; CG(extra_fn_flags) = $9;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "CG")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "extra_fn_flags")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'ASSIGNMENT "=")
             (list 'PARAMETER "$9")
             (list 'SEMICOLON ";")))
           "(let ((parameter-9 '(value $9))(return-item '(value $$)))(CG 'extra_fn_flags parameter-9) return-item)"))
  (message "Passed test: set function-value via assignment")

  ;; CG(extra_fn_flags) |= ZEND_ACC_GENERATOR;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "CG")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "extra_fn_flags")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'BITWISE_OR_ASSIGNMENT "|=")
             (list 'VARIABLE "ZEND_ACC_GENERATOR")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(CG 'extra_fn_flags (logior (CG 'extra_fn_flags) 'ZEND_ACC_GENERATOR)) return-item)"))
  (message "Passed test: set function-value via bitwise-or-assignment")

  ;; CG(extra_fn_flags) &= ZEND_ACC_GENERATOR;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "CG")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "extra_fn_flags")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'BITWISE_AND_ASSIGNMENT "|=")
             (list 'VARIABLE "ZEND_ACC_GENERATOR")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(CG 'extra_fn_flags (logand (CG 'extra_fn_flags) 'ZEND_ACC_GENERATOR)) return-item)"))
  (message "Passed test: set function-value via bitwise-and-assignment")

  ;; $$ |= ZEND_ACC_PUBLIC;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'BITWISE_OR_ASSIGNMENT "|=")
             (list 'VARIABLE "ZEND_ACC_PUBLIC")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(plist-put return-item 'value (logior (plist-get return-item 'value) 'ZEND_ACC_PUBLIC)) return-item)"))
  (message "Passed test: assign return-item bitwise-or of variable")

  ;; $$ &= ZEND_ACC_PUBLIC;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'BITWISE_AND_ASSIGNMENT "|=")
             (list 'VARIABLE "ZEND_ACC_PUBLIC")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(plist-put return-item 'value (logand (plist-get return-item 'value) 'ZEND_ACC_PUBLIC)) return-item)"))
  (message "Passed test: assign return-item bitwise-and of variable")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_create_decl")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "ZEND_AST_CLOSURE")
             (list 'COMMA ",")
             (list 'PARAMETER "$2")
             (list 'BITWISE_OR "|")
             (list 'PARAMETER "$13")
             (list 'CLOSE_PARENTHESIS)
             (list 'SEMICOLON ";")))
           "(let ((parameter-13 '(value $13))(parameter-2 '(value $2))(return-item '(value $$)))(plist-put return-item 'value (zend_ast_create_decl ZEND_AST_CLOSURE (logior parameter-2 parameter-13))) return-item)"))
  (message "Passed test: bitwise-or on function arguments")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_create_decl")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "ZEND_AST_CLOSURE")
             (list 'COMMA ",")
             (list 'PARAMETER "$2")
             (list 'BITWISE_AND "&")
             (list 'PARAMETER "$13")
             (list 'CLOSE_PARENTHESIS)
             (list 'SEMICOLON ";")))
           "(let ((parameter-13 '(value $13))(parameter-2 '(value $2))(return-item '(value $$)))(plist-put return-item 'value (zend_ast_create_decl ZEND_AST_CLOSURE (logand parameter-2 parameter-13))) return-item)"))
  (message "Passed test: bitwise-and on function arguments")

  ;; TODO ternary stuff like
  ;; { $$ = zend_ast_create(ZEND_AST_PROP_ELEM, $1, NULL, ($2 ? zend_ast_create_zval_from_str($2) : NULL)); }

  ;; TODO string stuff like { $$ = zend_ast_create_decl(ZEND_AST_CLOSURE, $2 | $13, $1, $3,
  ;; zend_string_init("{closure}", sizeof("{closure}") - 1, 0),
  ;; $5, $7, $11, $8); CG(extra_fn_flags) = $9; }

  ;; TODO Dereferenced pointers like    {
  ;; 	zend_ast *decl = zend_ast_create_decl(
  ;; 		ZEND_AST_CLASS, ZEND_ACC_ANON_CLASS, $<num>2, $6, NULL,
  ;; 		$4, $5, $8, NULL);
  ;; 	$$ = zend_ast_create(ZEND_AST_NEW, decl, $3);
  ;; }

  ;; TODO Dereference stuff like
  ;; { $$ = $2; ((zend_ast_decl *) $$)->flags |= ZEND_ACC_STATIC; }

  )

(emacs-wisent-grammar-converter-test--lex-c-string)
(emacs-wisent-grammar-converter-test--converted-lexer-tokens-to-lisp)
;; (emacs-wisent-grammar-converter-test--reformat-logic-block)

(message "\nUnit tests completed\n")

(provide 'emacs-wisent-grammar-converter-test)
;;; emacs-wisent-grammar-converter-test.el ends here
