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

;; TODO Add tests for converting C to Emacs-Lisp
;; TODO Refactor code into separate files


;;; Code:

(require 'emacs-wisent-grammar-converter)
(require 'ert)

(message "\nUnit tests started\n")

(defun emacs-wisent-grammar-converter-test--reformat-logic-block()
  "Test conversion of C to Wisent Emacs-Lisp."

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$$ = zend_ast_append_str($1, $3);")
    "(let ((parameter-3 '(value $3))(parameter-1 '(value $1))(return-item '(value $$)))(plist-put return-item 'value (zend_ast_append_str parameter-1 parameter-3)) return-item)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 1")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$$ = zend_ast_create(ZEND_AST_HALT_COMPILER,
			      zend_ast_create_zval_from_long(zend_get_scanned_file_offset()));
			  zend_stop_lexing();")
    "(let ((return-item '(value $$)))(plist-put return-item 'value (zend_ast_create 'zend_ast_halt_compiler (zend_ast_create_zval_from_long (zend_get_scanned_file_offset))))(zend_stop_lexing) return-item)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 2")

  ;; TODO
  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "			zval zv;
			zend_lex_tstring(&zv);
			$$ = zend_ast_create_zval(&zv);
")
    "(let ((return-item '(value $$)))(mask) return-item)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 3")

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
            (list 'SYMBOL "zend_ast_include_or_eval")
            (list 'COMMA ",")
            (list 'SYMBOL "zend_require_once")
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
            (list 'SYMBOL "zend_ast_trait_alias")
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
            (list 'SYMBOL "zend_name_not_fq")
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
            (list 'SYMBOL "zend_type_nullable")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            " $$ = zend_ast_create(ZEND_AST_PROP_ELEM, $1, NULL, ($2 ? zend_ast_create_zval_from_str($2) : NULL)); ")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "zend_ast_prop_elem")
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

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "$$ = zend_ast_create_decl(ZEND_AST_CLOSURE, $2 | $13, $1, $3, zend_string_init(\"{closure}\", sizeof(\"{closure}\") - 1, 0), $5, $7, $11, $8); CG(extra_fn_flags) = $9;")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create_decl")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "zend_ast_closure")
            (list 'COMMA ",")
            (list 'PARAMETER "$2")
            (list 'BITWISE_OR "|")
            (list 'PARAMETER "$13")
            (list 'COMMA ",")
            (list 'PARAMETER "$1")
            (list 'COMMA ",")
            (list 'PARAMETER "$3")
            (list 'COMMA ",")
            (list 'FUNCTION "zend_string_init")
            (list 'OPEN_PARENTHESIS "(")
            (list 'STRING "{closure}")
            (list 'COMMA ",")
            (list 'FUNCTION "sizeof")
            (list 'OPEN_PARENTHESIS "(")
            (list 'STRING "{closure}")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SUBTRACTION "-")
            (list 'INTEGER "1")
            (list 'COMMA ",")
            (list 'INTEGER "0")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'COMMA ",")
            (list 'PARAMETER "$5")
            (list 'COMMA ",")
            (list 'PARAMETER "$7")
            (list 'COMMA ",")
            (list 'PARAMETER "$11")
            (list 'COMMA ",")
            (list 'PARAMETER "$8")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";")
            (list 'FUNCTION "cg")
            (list 'OPEN_PARENTHESIS "(")
            (list 'VARIABLE "extra_fn_flags")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'ASSIGNMENT "=")
            (list 'PARAMETER "$9")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test with strings and integers")

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "$$ = $2; ((zend_ast_decl *) $$)->flags |= ZEND_ACC_STATIC;")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'PARAMETER "$2")
            (list 'SEMICOLON ";")
            (list 'OPEN_PARENTHESIS "(")
            (list 'OPEN_PARENTHESIS "(")
            (list 'DECLARATION "zend_ast_decl")
            (list 'POINTER "")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'RETURN "$$")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'MEMBER_OPERATOR "->")
            (list 'VARIABLE "flags")
            (list 'BITWISE_OR_ASSIGNMENT "|=")
            (list 'SYMBOL "zend_acc_static")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test lexing of de-referenced variable")

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "zend_ast *decl = zend_ast_create_decl(\nZEND_AST_CLASS, ZEND_ACC_ANON_CLASS, $<num>2, $6, NULL,\n$4, $5, $8, NULL);\n$$ = zend_ast_create(ZEND_AST_NEW, decl, $3);")
           (list
            (list 'DECLARATION "zend_ast")
            (list 'POINTER "decl")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create_decl")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "zend_ast_class")
            (list 'COMMA ",")
            (list 'SYMBOL "zend_acc_anon_class")
            (list 'COMMA ",")
            (list 'PARAMETER "$2")
            (list 'COMMA ",")
            (list 'PARAMETER "$6")
            (list 'COMMA ",")
            (list 'NULL "NULL")
            (list 'COMMA ",")
            (list 'PARAMETER "$4")
            (list 'COMMA ",")
            (list 'PARAMETER "$5")
            (list 'COMMA ",")
            (list 'PARAMETER "$8")
            (list 'COMMA ",")
            (list 'NULL "NULL")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";")
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "zend_ast_new")
            (list 'COMMA ",")
            (list 'VARIABLE "decl")
            (list 'COMMA ",")
            (list 'PARAMETER "$3")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test lexing of de-referenced variable 2")

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "$$ = zend_ast_create(ZEND_AST_HALT_COMPILER,
			      zend_ast_create_zval_from_long(zend_get_scanned_file_offset()));
			  zend_stop_lexing();")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "zend_ast_halt_compiler")
            (list 'COMMA ",")
            (list 'FUNCTION "zend_ast_create_zval_from_long")
            (list 'OPEN_PARENTHESIS "(")
            (list 'FUNCTION "zend_get_scanned_file_offset")
            (list 'OPEN_PARENTHESIS "(")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";")
            (list 'FUNCTION "zend_stop_lexing")
            (list 'OPEN_PARENTHESIS "(")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))
  (message "Passed test assignment with nested function-calls")

  (should (equal
           (emacs-wisent-grammar-converter--lex-c-string
            "			zval zv;
			zend_lex_tstring(&zv);
			$$ = zend_ast_create_zval(&zv);
")
           (list
            (list 'DECLARATION "zval") (list 'VARIABLE "zv") (list 'SEMICOLON ";") (list 'FUNCTION "zend_lex_tstring") (list 'OPEN_PARENTHESIS "(") (list 'REFERENCE "zv") (list 'CLOSE_PARENTHESIS ")") (list 'SEMICOLON ";") (list 'RETURN "$$") (list 'ASSIGNMENT "=") (list 'FUNCTION "zend_ast_create_zval") (list 'OPEN_PARENTHESIS "(") (list 'REFERENCE "zv") (list 'CLOSE_PARENTHESIS ")") (list 'SEMICOLON ";"))))
  (message "Passed test assignment with nested function with referenced variable")

  
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
             (list 'SYMBOL "zend_name_not_fq")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(plist-put return-string 'attr 'zend_name_not_fq) return-item)"))
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
             (list 'SYMBOL "zend_name_not_fq")
             (list 'SEMICOLON ";")))
           "(let ((parameter-1 '(value $1))(return-item '(value $$)))(plist-put return-item 'value parameter-1)(plist-put return-string 'attr 'zend_name_not_fq) return-item)"))
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
             (list 'SYMBOL "zend_type_nullable")
             (list 'SEMICOLON ";")))
           "(let ((parameter-2 '(value $2))(return-item '(value $$)))(plist-put return-item 'value parameter-2)(plist-put return-string 'attr (logior (plist-get return-item 'attr) 'zend_type_nullable)) return-item)"))
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
             (list 'SYMBOL "zend_acc_generator")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(CG 'extra_fn_flags (logior (CG 'extra_fn_flags) 'zend_acc_generator)) return-item)"))
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
             (list 'SYMBOL "zend_acc_generator")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(CG 'extra_fn_flags (logand (CG 'extra_fn_flags) 'zend_acc_generator)) return-item)"))
  (message "Passed test: set function-value via bitwise-and-assignment")

  ;; $$ |= ZEND_ACC_PUBLIC;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'BITWISE_OR_ASSIGNMENT "|=")
             (list 'SYMBOL "zend_acc_public")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(plist-put return-item 'value (logior (plist-get return-item 'value) 'zend_acc_public)) return-item)"))
  (message "Passed test: assign return-item bitwise-or of variable")

  ;; $$ &= ZEND_ACC_PUBLIC;
  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'BITWISE_AND_ASSIGNMENT "|=")
             (list 'SYMBOL "zend_acc_public")
             (list 'SEMICOLON ";")))
           "(let ((return-item '(value $$)))(plist-put return-item 'value (logand (plist-get return-item 'value) 'zend_acc_public)) return-item)"))
  (message "Passed test: assign return-item bitwise-and of variable")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_create_decl")
             (list 'OPEN_PARENTHESIS "(")
             (list 'SYMBOL "zend_ast_closure")
             (list 'COMMA ",")
             (list 'PARAMETER "$2")
             (list 'BITWISE_OR "|")
             (list 'PARAMETER "$13")
             (list 'CLOSE_PARENTHESIS)
             (list 'SEMICOLON ";")))
           "(let ((parameter-13 '(value $13))(parameter-2 '(value $2))(return-item '(value $$)))(plist-put return-item 'value (zend_ast_create_decl 'zend_ast_closure (logior parameter-2 parameter-13))) return-item)"))
  (message "Passed test: bitwise-or on function arguments")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_create_decl")
             (list 'OPEN_PARENTHESIS "(")
             (list 'SYMBOL "zend_ast_closure")
             (list 'COMMA ",")
             (list 'PARAMETER "$2")
             (list 'BITWISE_AND "&")
             (list 'PARAMETER "$13")
             (list 'CLOSE_PARENTHESIS)
             (list 'SEMICOLON ";")))
           "(let ((parameter-13 '(value $13))(parameter-2 '(value $2))(return-item '(value $$)))(plist-put return-item 'value (zend_ast_create_decl 'zend_ast_closure (logand parameter-2 parameter-13))) return-item)"))
  (message "Passed test: bitwise-and on function arguments")

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_create")
             (list 'OPEN_PARENTHESIS "(")
             (list 'SYMBOL "zend_ast_prop_elem")
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
             (list 'CLOSE_PARENTHESIS)
             (list 'COLON ":")
             (list 'NULL "NULL")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((parameter-2 '(value $2))(parameter-2 '(value $2))(parameter-1 '(value $1))(return-item '(value $$)))(plist-put return-item 'value (zend_ast_create 'zend_ast_prop_elem parameter-1 nil (if parameter-2 (zend_ast_create_zval_from_str parameter-2) nil))) return-item)"))
  (message "Passed test: ternary expression in function arguments")
  ;; { $$ = zend_ast_create(ZEND_AST_PROP_ELEM, $1, NULL, ($2 ? zend_ast_create_zval_from_str($2) : NULL)); }

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'PARAMETER "$2")
             (list 'SEMICOLON ";")
             (list 'OPEN_PARENTHESIS "(")
             (list 'OPEN_PARENTHESIS "(")
             (list 'DECLARATION "zend_ast_decl")
             (list 'POINTER "")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'RETURN "$$")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'MEMBER_OPERATOR "->")
             (list 'VARIABLE "flags")
             (list 'BITWISE_OR_ASSIGNMENT "|=")
             (list 'SYMBOL "zend_acc_static")
             (list 'SEMICOLON ";")))
           "(let ((parameter-2 '(value $2))(return-item '(value $$)))(plist-put return-item 'value parameter-2)(plist-put return-string 'flags (logior (plist-get return-item 'flags) 'zend_acc_static)) return-item)"))
  (message "Passed test: de-referenced variable first test")
  ;; { $$ = $2; ((zend_ast_decl *) $$)->flags |= ZEND_ACC_STATIC; }

  (should (equal
           (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
            (list
             (list 'DECLARATION "zend_ast")
             (list 'POINTER "decl")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_create_decl")
             (list 'OPEN_PARENTHESIS "(")
             (list 'SYMBOL "zend_ast_class")
             (list 'COMMA ",")
             (list 'SYMBOL "zend_acc_anon_class")
             (list 'COMMA ",")
             (list 'PARAMETER "$2")
             (list 'COMMA ",")
             (list 'PARAMETER "$6")
             (list 'COMMA ",")
             (list 'NULL "NULL")
             (list 'COMMA ",")
             (list 'PARAMETER "$4")
             (list 'COMMA ",")
             (list 'PARAMETER "$5")
             (list 'COMMA ",")
             (list 'PARAMETER "$8")
             (list 'COMMA ",")
             (list 'NULL "NULL")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'FUNCTION "zend_ast_create")
             (list 'OPEN_PARENTHESIS "(")
             (list 'SYMBOL "zend_ast_new")
             (list 'COMMA ",")
             (list 'VARIABLE "decl")
             (list 'COMMA ",")
             (list 'PARAMETER "$3")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((parameter-3 '(value $3))(parameter-8 '(value $8))(parameter-5 '(value $5))(parameter-4 '(value $4))(parameter-6 '(value $6))(parameter-2 '(value $2))(return-item '(value $$)))(setq decl (zend_ast_create_decl 'zend_ast_class 'zend_acc_anon_class parameter-2 parameter-6 nil parameter-4 parameter-5 parameter-8 nil))(plist-put return-item 'value (zend_ast_create 'zend_ast_new decl parameter-3)) return-item)"))
  (message "Passed test: de-referenced variable second test")
  ;; {
  ;; 	zend_ast *decl = zend_ast_create_decl(
  ;; 		ZEND_AST_CLASS, ZEND_ACC_ANON_CLASS, $<num>2, $6, NULL,
  ;; 		$4, $5, $8, NULL);
  ;; 	$$ = zend_ast_create(ZEND_AST_NEW, decl, $3);
  ;; }

  (should
   (equal
    (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
     (list
      (list 'RETURN "$$")
      (list 'ASSIGNMENT "=")
      (list 'FUNCTION "zend_ast_create")
      (list 'OPEN_PARENTHESIS "(")
      (list 'SYMBOL "zend_ast_halt_compiler")
      (list 'COMMA ",")
      (list 'FUNCTION "zend_ast_create_zval_from_long")
      (list 'OPEN_PARENTHESIS "(")
      (list 'FUNCTION "zend_get_scanned_file_offset")
      (list 'OPEN_PARENTHESIS "(")
      (list 'CLOSE_PARENTHESIS ")")
      (list 'CLOSE_PARENTHESIS ")")
      (list 'CLOSE_PARENTHESIS ")")
      (list 'SEMICOLON ";")
      (list 'FUNCTION "zend_stop_lexing")
      (list 'OPEN_PARENTHESIS "(")
      (list 'CLOSE_PARENTHESIS ")")
      (list 'SEMICOLON ";")))
    "(let ((return-item '(value $$)))(plist-put return-item 'value (zend_ast_create 'zend_ast_halt_compiler (zend_ast_create_zval_from_long (zend_get_scanned_file_offset))))(zend_stop_lexing) return-item)"))
  (message "Passed test: assignment with nested function calls without arguments")

  (should
   (equal
    (emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp
     (list
      (list 'DECLARATION "zval") (list 'VARIABLE "zv") (list 'SEMICOLON ";") (list 'FUNCTION "zend_lex_tstring") (list 'OPEN_PARENTHESIS "(") (list 'REFERENCE "zv") (list 'CLOSE_PARENTHESIS ")") (list 'SEMICOLON ";") (list 'RETURN "$$") (list 'ASSIGNMENT "=") (list 'FUNCTION "zend_ast_create_zval") (list 'OPEN_PARENTHESIS "(") (list 'REFERENCE "zv") (list 'CLOSE_PARENTHESIS ")") (list 'SEMICOLON ";")))
    "nil"))
  (message "Passed test: function call with referenced variable")

  )

(emacs-wisent-grammar-converter-test--lex-c-string)
(emacs-wisent-grammar-converter-test--converted-lexer-tokens-to-lisp)
(emacs-wisent-grammar-converter-test--reformat-logic-block)

(message "\nUnit tests completed\n")

(provide 'emacs-wisent-grammar-converter-test)
;;; emacs-wisent-grammar-converter-test.el ends here
