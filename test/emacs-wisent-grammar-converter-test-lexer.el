;;; emacs-wisent-grammar-converter-test-lexer.el --- Tests for Emacs Wisent Grammar Converter Lexer -*- lexical-binding:t -*-

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


;;; Code:

(require 'emacs-wisent-grammar-converter)
(require 'ert)

(message "\nUnit tests for lexer started\n")

(defun emacs-wisent-grammar-converter-test-lexer--lex-c-string ()
  "Test `emacs-wisent-grammar-converter-test--lex-c-string'"
  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "	  \nmask();	\n  "
            )
           (list
            (list 'FUNCTION "mask")
            (list 'OPEN_PARENTHESIS "(")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "$$ = $1;"
            )
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'PARAMETER "$1")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "$$ = zend_ast_create_ex(ZEND_AST_INCLUDE_OR_EVAL, ZEND_REQUIRE_ONCE, $2);"
            )
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create_ex")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "ZEND_AST_INCLUDE_OR_EVAL")
            (list 'COMMA ",")
            (list 'SYMBOL "ZEND_REQUIRE_ONCE")
            (list 'COMMA ",")
            (list 'PARAMETER "$2")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";")))
          )

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
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
            (list 'SYMBOL "ZEND_AST_TRAIT_ALIAS")
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
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "$$->attr = ZEND_NAME_NOT_FQ;"
            )
           (list
            (list 'RETURN "$$")
            (list 'MEMBER_OPERATOR "->")
            (list 'VARIABLE "attr")
            (list 'ASSIGNMENT "=")
            (list 'SYMBOL "ZEND_NAME_NOT_FQ")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "	  \n$$->attr |= ZEND_TYPE_NULLABLE;	\n  "
            )
           (list
            (list 'RETURN "$$")
            (list 'MEMBER_OPERATOR "->")
            (list 'VARIABLE "attr")
            (list 'BITWISE_OR_ASSIGNMENT "|=")
            (list 'SYMBOL "ZEND_TYPE_NULLABLE")
            (list 'SEMICOLON ";"))))

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            " $$ = zend_ast_create(ZEND_AST_PROP_ELEM, $1, NULL, ($2 ? zend_ast_create_zval_from_str($2) : NULL)); ")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "ZEND_AST_PROP_ELEM")
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
           (emacs-wisent-grammar-converter-lexer--lex-c-string
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
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "$$ = zend_ast_create_decl(ZEND_AST_CLOSURE, $2 | $13, $1, $3, zend_string_init(\"{closure}\", sizeof(\"{closure}\") - 1, 0), $5, $7, $11, $8); CG(extra_fn_flags) = $9;")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create_decl")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "ZEND_AST_CLOSURE")
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
            (list 'FUNCTION "CG")
            (list 'OPEN_PARENTHESIS "(")
            (list 'VARIABLE "extra_fn_flags")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'ASSIGNMENT "=")
            (list 'PARAMETER "$9")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test: with strings and integers")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
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
            (list 'SYMBOL "ZEND_ACC_STATIC")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test: lexing of de-referenced variable")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "zend_ast *decl = zend_ast_create_decl(\nZEND_AST_CLASS, ZEND_ACC_ANON_CLASS, $<num>2, $6, NULL,\n$4, $5, $8, NULL);\n$$ = zend_ast_create(ZEND_AST_NEW, decl, $3);")
           (list
            (list 'DECLARATION "zend_ast")
            (list 'POINTER "decl")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create_decl")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "ZEND_AST_CLASS")
            (list 'COMMA ",")
            (list 'SYMBOL "ZEND_ACC_ANON_CLASS")
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
            (list 'SYMBOL "ZEND_AST_NEW")
            (list 'COMMA ",")
            (list 'VARIABLE "decl")
            (list 'COMMA ",")
            (list 'PARAMETER "$3")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test: lexing of de-referenced variable 2")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "$$ = zend_ast_create(ZEND_AST_HALT_COMPILER,
			      zend_ast_create_zval_from_long(zend_get_scanned_file_offset()));
			  zend_stop_lexing();")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_ast_create")
            (list 'OPEN_PARENTHESIS "(")
            (list 'SYMBOL "ZEND_AST_HALT_COMPILER")
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
  (message "Passed lexer test: assignment with nested function-calls")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "			zval zv;
			zend_lex_tstring(&zv);
			$$ = zend_ast_create_zval(&zv);
")
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
            (list 'FUNCTION "zend_ast_create_zval")
            (list 'OPEN_PARENTHESIS "(")
            (list 'REFERENCE "zv")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test: assignment with nested function with referenced variable")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            " $$ = zend_add_class_modifier($1, $2); if (!$$) { YYERROR; }")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "zend_add_class_modifier")
            (list 'OPEN_PARENTHESIS "(")
            (list 'PARAMETER "$1")
            (list 'COMMA ",")
            (list 'PARAMETER "$2")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";")
            (list 'IF "if")
            (list 'OPEN_PARENTHESIS "(")
            (list 'LOGICAL_NOT "!")
            (list 'RETURN "$$")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'OPEN_CURLY_BRACKET "{")
            (list 'SYMBOL "YYERROR")
            (list 'SEMICOLON ";")
            (list 'CLOSE_CURLY_BRACKET "}"))))
  (message "Passed lexer test: if block checking return value")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "			$$ = $2;
			if ($$->kind == ZEND_AST_CONDITIONAL) $$->attr = ZEND_PARENTHESIZED_CONDITIONAL;
		")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'PARAMETER "$2")
            (list 'SEMICOLON ";")
            (list 'IF "if")
            (list 'OPEN_PARENTHESIS "(")
            (list 'RETURN "$$")
            (list 'MEMBER_OPERATOR "->")
            (list 'VARIABLE "kind")
            (list 'EQUAL "==")
            (list 'SYMBOL "ZEND_AST_CONDITIONAL")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'RETURN "$$")
            (list 'MEMBER_OPERATOR "->")
            (list 'VARIABLE "attr")
            (list 'ASSIGNMENT "=")
            (list 'SYMBOL "ZEND_PARENTHESIZED_CONDITIONAL")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test: if block changing return value attribute")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "$<num>$ = CG(zend_lineno);")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'FUNCTION "CG")
            (list 'OPEN_PARENTHESIS "(")
            (list 'VARIABLE "zend_lineno")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'SEMICOLON ";"))))
  (message "Passed lexer test: assignment of return-value of function return-value with type conversion")

  (should (equal
           (emacs-wisent-grammar-converter-lexer--lex-c-string
            "$$ = $1; if (!($$ & ZEND_ACC_PPP_MASK)) { $$ |= ZEND_ACC_PUBLIC; } ")
           (list
            (list 'RETURN "$$")
            (list 'ASSIGNMENT "=")
            (list 'PARAMETER "$1")
            (list 'SEMICOLON ";")
            (list 'IF "if")
            (list 'OPEN_PARENTHESIS "(")
            (list 'LOGICAL_NOT "!")
            (list 'OPEN_PARENTHESIS "(")
            (list 'RETURN "$$")
            (list 'BITWISE_AND "&")
            (list 'SYMBOL "ZEND_ACC_PPP_MASK")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'CLOSE_PARENTHESIS ")")
            (list 'OPEN_CURLY_BRACKET "{")
            (list 'RETURN "$$")
            (list 'BITWISE_OR_ASSIGNMENT "|=")
            (list 'SYMBOL "ZEND_ACC_PUBLIC")
            (list 'SEMICOLON ";")
            (list 'CLOSE_CURLY_BRACKET "}"))))
  (message "Passed lexer test: if conditional with negation and nested parenthesis")


  )

;; (setq debug-on-error t)

(emacs-wisent-grammar-converter-test-lexer--lex-c-string)

(message "\nUnit tests for lexer completed\n")

(provide 'emacs-wisent-grammar-converter-test-lexer)
;;; emacs-wisent-grammar-converter-test-lexer.el ends here
