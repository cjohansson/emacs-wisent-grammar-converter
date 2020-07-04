;;; emacs-wisent-grammar-converter-test-parser.el --- Tests for Emacs Wisent Grammar Converter Parser -*- lexical-binding:t -*-

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

(message "\nUnit tests for parser started\n")

(defun emacs-wisent-grammar-converter-test-parser--converted-lexer-tokens-to-lisp ()
  "Test `emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp'"
  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((r)) (MASK) r)"))
  (message "Passed test: function-call without arguments")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "zv")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((r)) (MASK zv) r)"))
  (message "Passed test: function-call with one argument")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "zv")
             (list 'PARAMETER "$2")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";")))
           "(let ((r)) (MASK zv $2) r)"))
  (message "Passed test: function-call with two arguments")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (MASK (MASK2 zv zv2) zv3) r)"))
  (message "Passed test: nested function calls")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "mask")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "zv")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'SEMICOLON ";"))
            "namespace-")
           "(let ((r)) (MASK namespace-zv) r)"))
  (message "Passed test: function-call with argument and namespace")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'PARAMETER "$3")
             (list 'SEMICOLON ";")))
           "(let ((r)) (setq r $3) r)"))
  (message "Passed test: assign return-value value of parameter")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'ASSIGNMENT "=")
             (list 'NULL "null")
             (list 'SEMICOLON ";")))
           "(let ((r)) (setq r nil) r)"))
  (message "Passed test: assign return-value value of nil")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (setq r (ZEND_AST_APPEND_STR $1 $3)) r)"))
  (message "Passed test: assign return-value function-call with parameter arguments")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'PARAMETER "$1")
             (list 'MEMBER_OPERATOR "->")
             (list 'VARIABLE "attr")
             (list 'ASSIGNMENT "=")
             (list 'SYMBOL "zend_name_not_fq")
             (list 'SEMICOLON ";")))
           "(let ((r)) (semantic-tag-put-attribute $1 'attr 'zend_name_not_fq) r)"))
  (message "Passed test: set attribute of parameter")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (setq r $1)(semantic-tag-put-attribute r 'attr 'zend_name_not_fq) r)"))
  (message "Passed test: set return-item to parameter and then change attribute of return-item")

  ;; Support things like    $$ = $2; $$->attr |= ZEND_TYPE_NULLABLE;
  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (setq r $2)(semantic-tag-put-attribute r 'attr (logior (semantic-tag-get-attribute r 'attr) 'zend_type_nullable)) r)"))
  (message "Passed test: set return-item to parameter and then change attribute with a bitwise-or assignment of return-item")

  ;; CG(extra_fn_flags) = $9;
  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "CG")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "extra_fn_flags")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'ASSIGNMENT "=")
             (list 'PARAMETER "$9")
             (list 'SEMICOLON ";")))
           "(let ((r)) (CG 'extra_fn_flags $9) r)"))
  (message "Passed test: set function-value via assignment")

  ;; CG(extra_fn_flags) |= ZEND_ACC_GENERATOR;
  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "CG")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "extra_fn_flags")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'BITWISE_OR_ASSIGNMENT "|=")
             (list 'SYMBOL "zend_acc_generator")
             (list 'SEMICOLON ";")))
           "(let ((r)) (CG 'extra_fn_flags (logior (CG 'extra_fn_flags) 'zend_acc_generator)) r)"))
  (message "Passed test: set function-value via bitwise-or-assignment")

  ;; CG(extra_fn_flags) &= ZEND_ACC_GENERATOR;
  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'FUNCTION "CG")
             (list 'OPEN_PARENTHESIS "(")
             (list 'VARIABLE "extra_fn_flags")
             (list 'CLOSE_PARENTHESIS ")")
             (list 'BITWISE_AND_ASSIGNMENT "|=")
             (list 'SYMBOL "zend_acc_generator")
             (list 'SEMICOLON ";")))
           "(let ((r)) (CG 'extra_fn_flags (logand (CG 'extra_fn_flags) 'zend_acc_generator)) r)"))
  (message "Passed test: set function-value via bitwise-and-assignment")

  ;; $$ |= ZEND_ACC_PUBLIC;
  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'BITWISE_OR_ASSIGNMENT "|=")
             (list 'SYMBOL "zend_acc_public")
             (list 'SEMICOLON ";")))
           "(let ((r)) (setq r (logior r 'zend_acc_public)) r)"))
  (message "Passed test: assign return-item bitwise-or of variable")

  ;; $$ &= ZEND_ACC_PUBLIC;
  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
            (list
             (list 'RETURN "$$")
             (list 'BITWISE_AND_ASSIGNMENT "|=")
             (list 'SYMBOL "zend_acc_public")
             (list 'SEMICOLON ";")))
           "(let ((r)) (setq r (logand r 'zend_acc_public)) r)"))
  (message "Passed test: assign return-item bitwise-and of variable")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (setq r (ZEND_AST_CREATE_DECL 'zend_ast_closure (logior $2 $13))) r)"))
  (message "Passed test: bitwise-or on function arguments")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (setq r (ZEND_AST_CREATE_DECL 'zend_ast_closure (logand $2 $13))) r)"))
  (message "Passed test: bitwise-and on function arguments")

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (setq r (ZEND_AST_CREATE 'zend_ast_prop_elem $1 nil (if $2 (ZEND_AST_CREATE_ZVAL_FROM_STR $2) nil))) r)"))
  (message "Passed test: ternary expression in function arguments")
  ;; { $$ = zend_ast_create(ZEND_AST_PROP_ELEM, $1, NULL, ($2 ? zend_ast_create_zval_from_str($2) : NULL)); }

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)) (setq r $2)(semantic-tag-put-attribute r 'flags (logior (semantic-tag-get-attribute r 'flags) 'zend_acc_static)) r)"))
  (message "Passed test: de-referenced variable first test")
  ;; { $$ = $2; ((zend_ast_decl *) $$)->flags |= ZEND_ACC_STATIC; }

  (should (equal
           (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
           "(let ((r)(decl)) (setq decl (ZEND_AST_CREATE_DECL 'zend_ast_class 'zend_acc_anon_class $2 $6 nil $4 $5 $8 nil))(setq r (ZEND_AST_CREATE 'zend_ast_new decl $3)) r)"))
  (message "Passed test: de-referenced variable second test")
  ;; {
  ;; 	zend_ast *decl = zend_ast_create_decl(
  ;; 		ZEND_AST_CLASS, ZEND_ACC_ANON_CLASS, $<num>2, $6, NULL,
  ;; 		$4, $5, $8, NULL);
  ;; 	$$ = zend_ast_create(ZEND_AST_NEW, decl, $3);
  ;; }

  (should
   (equal
    (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
    "(let ((r)) (setq r (ZEND_AST_CREATE 'zend_ast_halt_compiler (ZEND_AST_CREATE_ZVAL_FROM_LONG (ZEND_GET_SCANNED_FILE_OFFSET))))(ZEND_STOP_LEXING) r)"))
  (message "Passed test: assignment with nested function calls without arguments")

  (should
   (equal
    (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
      (list 'SEMICOLON ";")))
    "(let ((r)(zv)) (ZEND_LEX_TSTRING (lambda(return) (setq zv return)))(setq r (ZEND_AST_CREATE_ZVAL zv)) r)"))
  (message "Passed test: function call with referenced variable")

  (should
   (equal
    (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
      (list 'OPEN_SQUARE_BRACKET "{")
      (list 'SYMBOL "yyerror")
      (list 'SEMICOLON ";")
      (list 'CLOSE_SQUARE_BRACKET "}")))
    "(let ((r)) (setq r (ZEND_ADD_CLASS_MODIFIER $1 $2))(if (not r) (setq r 'yyerror)) r)"))
  (message "Passed test: if statement with logical not")

  (should
   (equal
    (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
     (list
      (list 'DOC_COMMENT "allow single trailing comma")
      (list 'RETURN "$$")
      (list 'ASSIGNMENT "=")
      (list 'FUNCTION "zend_ast_list_rtrim")
      (list 'OPEN_PARENTHESIS "(")
      (list 'PARAMETER "$1")
      (list 'CLOSE_PARENTHESIS ")")
      (list 'SEMICOLON ";")))
    "(let ((r)) ;; allow single trailing comma
(setq r (ZEND_AST_LIST_RTRIM $1)) r)"))
  (message "Passed test: code starting with doc comment")

  (should
   (equal
    (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
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
      (list 'SEMICOLON ";")))
    "(let ((r)) (setq r (ZEND_AST_CREATE_DECL 'zend_ast_closure (logior $2 $13) $1 $3 (ZEND_STRING_INIT \"{closure}\" (- (SIZEOF \"{closure}\") 1) 0) $5 $7 $11 $8))(CG 'extra_fn_flags $9) r)"))
  (message "Passed test: subtraction of function return in function arguments")

  (should
   (equal
    (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp
     (list
      (list 'RETURN "$$")
      (list 'ASSIGNMENT "=")
      (list 'PARAMETER "$1")
      (list 'SEMICOLON ";")
      (list 'IF "if")
      (list 'OPEN_PARENTHESIS "(")
      (list 'LOGICAL_NOT "!")
      (list 'OPEN_PARENTHESIS "(")
      (list 'BITWISE_AND "&")
      (list 'SYMBOL "ZEND_ACC_PPP_MASK")
      (list 'CLOSE_PARENTHESIS ")")
      (list 'CLOSE_PARENTHESIS ")")
      (list 'OPEN_CURLY_BRACKET "{")
      (list 'RETURN "$$")
      (list 'BITWISE_OR_ASSIGNMENT "|=")
      (list 'SYMBOL "ZEND_ACC_PUBLIC")
      (list 'SEMICOLON ";")
      (list 'CLOSE_CURLY_BRACKET "}"))
     "(let ((r)) (setq r $1)(if (not (bitwise-and r 'zend_acc_ppp_mask)) (setq r (bitwise-or r 'zend_acc_public))) r)")))
   (message "Passed test: condition bitwise-and on return value")

  )

(emacs-wisent-grammar-converter-test-parser--converted-lexer-tokens-to-lisp)

(message "\nUnit tests for parser completed\n")

(provide 'emacs-wisent-grammar-converter-test-parser)
;;; emacs-wisent-grammar-converter-test-parser.el ends here
