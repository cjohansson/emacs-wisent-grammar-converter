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


;; TODO Copy parser configuration


;;; Code:

(require 'emacs-wisent-grammar-converter)
(require 'ert)

(message "\nUnit tests for integration started\n")

(defun emacs-wisent-grammar-converter-test--parse-string (string &optional header-string prefix terminal-replacements macro-list)
  "Run `emacs-wisent-grammar-convert--parse-buffer' from string."
  (let ((buffer (generate-new-buffer "*Parser Test*"))
        (buffer-string))
    (switch-to-buffer buffer)
    (insert string)
    (setq
     buffer-string
     (emacs-wisent-grammar-convert--parse-buffer
      buffer
      header-string
      prefix
      terminal-replacements
      macro-list))
    (kill-buffer)
    buffer-string))

(defun emacs-wisent-grammar-converter-test--parse-buffer ()
  "Test parsing whole buffer."
  (message "Unit tests for parsing entire buffers started.\n")

  (should
   (equal
    (emacs-wisent-grammar-converter-test--parse-string
     "\n\n%%\n\nreserved_non_modifiers:\n	  T_INCLUDE | T_INCLUDE_ONCE | T_EVAL | T_REQUIRE | T_REQUIRE_ONCE | T_LOGICAL_OR | T_LOGICAL_XOR | T_LOGICAL_AND\n	| T_INSTANCEOF | T_NEW | T_CLONE | T_EXIT | T_IF | T_ELSEIF | T_ELSE | T_ENDIF | T_ECHO | T_DO | T_WHILE | T_ENDWHILE\n	| T_FOR | T_ENDFOR | T_FOREACH | T_ENDFOREACH | T_DECLARE | T_ENDDECLARE | T_AS | T_TRY | T_CATCH | T_FINALLY\n	| T_THROW | T_USE | T_INSTEADOF | T_GLOBAL | T_VAR | T_UNSET | T_ISSET | T_EMPTY | T_CONTINUE | T_GOTO\n	| T_FUNCTION | T_CONST | T_RETURN | T_PRINT | T_YIELD | T_LIST | T_SWITCH | T_ENDSWITCH | T_CASE | T_DEFAULT | T_BREAK\n	| T_ARRAY | T_CALLABLE | T_EXTENDS | T_IMPLEMENTS | T_NAMESPACE | T_TRAIT | T_INTERFACE | T_CLASS\n	| T_CLASS_C | T_TRAIT_C | T_FUNC_C | T_METHOD_C | T_LINE | T_FILE | T_DIR | T_NS_C | T_FN\n;\n\n\n%%\n")
    ";; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n\nreserved_non_modifiers:\n    T_INCLUDE\n    | T_INCLUDE_ONCE\n    | T_EVAL\n    | T_REQUIRE\n    | T_REQUIRE_ONCE\n    | T_LOGICAL_OR\n    | T_LOGICAL_XOR\n    | T_LOGICAL_AND\n    | T_INSTANCEOF\n    | T_NEW\n    | T_CLONE\n    | T_EXIT\n    | T_IF\n    | T_ELSEIF\n    | T_ELSE\n    | T_ENDIF\n    | T_ECHO\n    | T_DO\n    | T_WHILE\n    | T_ENDWHILE\n    | T_FOR\n    | T_ENDFOR\n    | T_FOREACH\n    | T_ENDFOREACH\n    | T_DECLARE\n    | T_ENDDECLARE\n    | T_AS\n    | T_TRY\n    | T_CATCH\n    | T_FINALLY\n    | T_THROW\n    | T_USE\n    | T_INSTEADOF\n    | T_GLOBAL\n    | T_VAR\n    | T_UNSET\n    | T_ISSET\n    | T_EMPTY\n    | T_CONTINUE\n    | T_GOTO\n    | T_FUNCTION\n    | T_CONST\n    | T_RETURN\n    | T_PRINT\n    | T_YIELD\n    | T_LIST\n    | T_SWITCH\n    | T_ENDSWITCH\n    | T_CASE\n    | T_DEFAULT\n    | T_BREAK\n    | T_ARRAY\n    | T_CALLABLE\n    | T_EXTENDS\n    | T_IMPLEMENTS\n    | T_NAMESPACE\n    | T_TRAIT\n    | T_INTERFACE\n    | T_CLASS\n    | T_CLASS_C\n    | T_TRAIT_C\n    | T_FUNC_C\n    | T_METHOD_C\n    | T_LINE\n    | T_FILE\n    | T_DIR\n    | T_NS_C\n    | T_FN\n    ;\n\n\n%%\n\n;; NOTE Generated grammar ends here"
    ))
  (message "Passed whole file first test")

  (should
   (equal
    (emacs-wisent-grammar-converter-test--parse-string
     "\n\n%%\n\nreserved_non_modifiers:\n	  T_INCLUDE | T_INCLUDE_ONCE | T_EVAL | T_REQUIRE | T_REQUIRE_ONCE | T_LOGICAL_OR | T_LOGICAL_XOR | T_LOGICAL_AND\n	| T_INSTANCEOF | T_NEW | T_CLONE | T_EXIT | T_IF | T_ELSEIF | T_ELSE | T_ENDIF | T_ECHO | T_DO | T_WHILE | T_ENDWHILE\n	| T_FOR | T_ENDFOR | T_FOREACH | T_ENDFOREACH | T_DECLARE | T_ENDDECLARE | T_AS | T_TRY | T_CATCH | T_FINALLY\n	| T_THROW | T_USE | T_INSTEADOF | T_GLOBAL | T_VAR | T_UNSET | T_ISSET | T_EMPTY | T_CONTINUE | T_GOTO\n	| T_FUNCTION | T_CONST | T_RETURN | T_PRINT | T_YIELD | T_LIST | T_SWITCH | T_ENDSWITCH | T_CASE | T_DEFAULT | T_BREAK\n	| T_ARRAY | T_CALLABLE | T_EXTENDS | T_IMPLEMENTS | T_NAMESPACE | T_TRAIT | T_INTERFACE | T_CLASS\n	| T_CLASS_C | T_TRAIT_C | T_FUNC_C | T_METHOD_C | T_LINE | T_FILE | T_DIR | T_NS_C | T_FN\n;\n\n\n%%\n"
     "\nrandom prefix stuff\n")
    "random prefix stuff\n\n\n;; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n\nreserved_non_modifiers:\n    T_INCLUDE\n    | T_INCLUDE_ONCE\n    | T_EVAL\n    | T_REQUIRE\n    | T_REQUIRE_ONCE\n    | T_LOGICAL_OR\n    | T_LOGICAL_XOR\n    | T_LOGICAL_AND\n    | T_INSTANCEOF\n    | T_NEW\n    | T_CLONE\n    | T_EXIT\n    | T_IF\n    | T_ELSEIF\n    | T_ELSE\n    | T_ENDIF\n    | T_ECHO\n    | T_DO\n    | T_WHILE\n    | T_ENDWHILE\n    | T_FOR\n    | T_ENDFOR\n    | T_FOREACH\n    | T_ENDFOREACH\n    | T_DECLARE\n    | T_ENDDECLARE\n    | T_AS\n    | T_TRY\n    | T_CATCH\n    | T_FINALLY\n    | T_THROW\n    | T_USE\n    | T_INSTEADOF\n    | T_GLOBAL\n    | T_VAR\n    | T_UNSET\n    | T_ISSET\n    | T_EMPTY\n    | T_CONTINUE\n    | T_GOTO\n    | T_FUNCTION\n    | T_CONST\n    | T_RETURN\n    | T_PRINT\n    | T_YIELD\n    | T_LIST\n    | T_SWITCH\n    | T_ENDSWITCH\n    | T_CASE\n    | T_DEFAULT\n    | T_BREAK\n    | T_ARRAY\n    | T_CALLABLE\n    | T_EXTENDS\n    | T_IMPLEMENTS\n    | T_NAMESPACE\n    | T_TRAIT\n    | T_INTERFACE\n    | T_CLASS\n    | T_CLASS_C\n    | T_TRAIT_C\n    | T_FUNC_C\n    | T_METHOD_C\n    | T_LINE\n    | T_FILE\n    | T_DIR\n    | T_NS_C\n    | T_FN\n    ;\n\n\n%%\n\n;; NOTE Generated grammar ends here"
    ))
  (message "Passed whole file test with header")

  (should
   (equal
    (emacs-wisent-grammar-converter-test--parse-string
     "\n\n%% /* Rules */\n\nstart:\n	top_statement_list	{ CG(ast) = $1; }\n;\n\n%%\n"
     nil
     "random-")
    ";; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n\nstart:\n    top_statement_list (let ((r)) (random-cg 'random-ast $1) r)\n    ;\n\n\n%%\n\n;; NOTE Generated grammar ends here"))
  (message "Passed whole file test with namespace")

  (let ((terminal-replacements (make-hash-table :test 'equal)))
    (puthash "'{'" "OPEN_CURLY_BRACKET" terminal-replacements)
    (should
     (equal
      (emacs-wisent-grammar-converter-test--parse-string
       "\n\n%% /* Rules */\n\nstart:\n	top_statement_list '{'	{ CG(ast) = $1; }\n;\n\n%%\n"
       nil
       "random-"
       terminal-replacements)
      ";; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n\nstart:\n    top_statement_list OPEN_CURLY_BRACKET (let ((r)) (random-cg 'random-ast $1) r)\n    ;\n\n\n%%\n\n;; NOTE Generated grammar ends here")))
  (message "Passed whole file test with terminal-replacements")

  (let ((macro-list (make-hash-table :test 'equal)))
    (puthash "CG" t macro-list)
    (should
     (equal
      (emacs-wisent-grammar-converter-test--parse-string
       "\n\n%% /* Rules */\n\nstart:\n	top_statement_list	{ CG(ast) = $1; }\n;\n\n%%\n"
       nil
       "random-"
       nil
       macro-list)
      ";; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n\nstart:\n    top_statement_list (let ((r)) (CG 'random-ast $1) r)\n    ;\n\n\n%%\n\n;; NOTE Generated grammar ends here")))
  (message "Passed whole file test with macro-list")

  (let ((terminal-replacements (make-hash-table :test 'equal)))
    (puthash "'{'" "OPEN_CURLY_BRACKET" terminal-replacements)
    (puthash "'}'" "CLOSE_CURLY_BRACKET" terminal-replacements)
    (should
     (equal
      (emacs-wisent-grammar-converter-test--parse-string
       "\n\n%%\n\nclass_declaration_statement:\n		class_modifiers T_CLASS { $<num>$ = CG(zend_lineno); }\n		T_STRING extends_from implements_list backup_doc_comment '{' class_statement_list '}'\n			{ $$ = zend_ast_create_decl(ZEND_AST_CLASS, $1, $<num>3, $7, zend_ast_get_str($4), $5, $6, $9, NULL); }\n	|	T_CLASS { $<num>$ = CG(zend_lineno); }\n		T_STRING extends_from implements_list backup_doc_comment '{' class_statement_list '}'\n			{ $$ = zend_ast_create_decl(ZEND_AST_CLASS, 0, $<num>2, $6, zend_ast_get_str($3), $4, $5, $8, NULL); }\n;\n\n\n%%\n"
       nil
       nil
       terminal-replacements)
      ";; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n\nclass_declaration_statement:\n    class_modifiers T_CLASS (let ((r)) (setq r (cg zend_lineno)) r) T_STRING extends_from implements_list backup_doc_comment OPEN_CURLY_BRACKET class_statement_list CLOSE_CURLY_BRACKET (let ((r)) (setq r (zend_ast_create_decl 'ZEND_AST_CLASS $1 $3 $7 (zend_ast_get_str $4) $5 $6 $9 nil)) r)\n    | T_CLASS (let ((r)) (setq r (cg zend_lineno)) r) T_STRING extends_from implements_list backup_doc_comment OPEN_CURLY_BRACKET class_statement_list CLOSE_CURLY_BRACKET (let ((r)) (setq r (zend_ast_create_decl 'ZEND_AST_CLASS 0 $2 $6 (zend_ast_get_str $3) $4 $5 $8 nil)) r)\n    ;\n\n\n%%\n\n;; NOTE Generated grammar ends here")))
  (message "Passed whole file test with multiple logic blocks in a rule")

  (let ((terminal-replacements (make-hash-table :test 'equal)))
    (puthash "'{'" "OPEN_CURLY_BRACKET" terminal-replacements)
    (puthash "'}'" "CLOSE_CURLY_BRACKET" terminal-replacements)
    (puthash "'('" "OPEN_ROUND_BRACKET" terminal-replacements)
    (puthash "')'" "CLOSE_ROUND_BRACKET" terminal-replacements)
    (puthash "';'" "SEMI_COLON" terminal-replacements)
    (puthash "':'" "COLON" terminal-replacements)
    (should
     (equal
      (emacs-wisent-grammar-converter-test--parse-string
       "\n\n%%\n\nstatement:\n		'{' inner_statement_list '}' { $$ = $2; }\n	|	if_stmt { $$ = $1; }\n	|	alt_if_stmt { $$ = $1; }\n	|	T_WHILE '(' expr ')' while_statement\n			{ $$ = zend_ast_create(ZEND_AST_WHILE, $3, $5); }\n	|	T_DO statement T_WHILE '(' expr ')' ';'\n			{ $$ = zend_ast_create(ZEND_AST_DO_WHILE, $2, $5); }\n	|	T_FOR '(' for_exprs ';' for_exprs ';' for_exprs ')' for_statement\n			{ $$ = zend_ast_create(ZEND_AST_FOR, $3, $5, $7, $9); }\n	|	T_SWITCH '(' expr ')' switch_case_list\n			{ $$ = zend_ast_create(ZEND_AST_SWITCH, $3, $5); }\n	|	T_BREAK optional_expr ';'		{ $$ = zend_ast_create(ZEND_AST_BREAK, $2); }\n	|	T_CONTINUE optional_expr ';'	{ $$ = zend_ast_create(ZEND_AST_CONTINUE, $2); }\n	|	T_RETURN optional_expr ';'		{ $$ = zend_ast_create(ZEND_AST_RETURN, $2); }\n	|	T_GLOBAL global_var_list ';'	{ $$ = $2; }\n	|	T_STATIC static_var_list ';'	{ $$ = $2; }\n	|	T_ECHO echo_expr_list ';'		{ $$ = $2; }\n	|	T_INLINE_HTML { $$ = zend_ast_create(ZEND_AST_ECHO, $1); }\n	|	expr ';' { $$ = $1; }\n	|	T_UNSET '(' unset_variables possible_comma ')' ';' { $$ = $3; }\n	|	T_FOREACH '(' expr T_AS foreach_variable ')' foreach_statement\n			{ $$ = zend_ast_create(ZEND_AST_FOREACH, $3, $5, NULL, $7); }\n	|	T_FOREACH '(' expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ')'\n		foreach_statement\n			{ $$ = zend_ast_create(ZEND_AST_FOREACH, $3, $7, $5, $9); }\n	|	T_DECLARE '(' const_list ')'\n			{ if (!zend_handle_encoding_declaration($3)) { YYERROR; } }\n		declare_statement\n			{ $$ = zend_ast_create(ZEND_AST_DECLARE, $3, $6); }\n	|	';'	/* empty statement */ { $$ = NULL; }\n	|	T_TRY '{' inner_statement_list '}' catch_list finally_statement\n			{ $$ = zend_ast_create(ZEND_AST_TRY, $3, $5, $6); }\n	|	T_GOTO T_STRING ';' { $$ = zend_ast_create(ZEND_AST_GOTO, $2); }\n	|	T_STRING ':' { $$ = zend_ast_create(ZEND_AST_LABEL, $1); }\n;\n\n\n\n%%\n"
       nil
       nil
       terminal-replacements)
      ";; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n\nstatement:\n    OPEN_CURLY_BRACKET inner_statement_list CLOSE_CURLY_BRACKET (let ((r)) (setq r $2) r)\n    | if_stmt (let ((r)) (setq r $1) r)\n    | alt_if_stmt (let ((r)) (setq r $1) r)\n    | T_WHILE OPEN_ROUND_BRACKET expr CLOSE_ROUND_BRACKET while_statement (let ((r)) (setq r (zend_ast_create 'ZEND_AST_WHILE $3 $5)) r)\n    | T_DO statement T_WHILE OPEN_ROUND_BRACKET expr CLOSE_ROUND_BRACKET SEMI_COLON (let ((r)) (setq r (zend_ast_create 'ZEND_AST_DO_WHILE $2 $5)) r)\n    | T_FOR OPEN_ROUND_BRACKET for_exprs SEMI_COLON for_exprs SEMI_COLON for_exprs CLOSE_ROUND_BRACKET for_statement (let ((r)) (setq r (zend_ast_create 'ZEND_AST_FOR $3 $5 $7 $9)) r)\n    | T_SWITCH OPEN_ROUND_BRACKET expr CLOSE_ROUND_BRACKET switch_case_list (let ((r)) (setq r (zend_ast_create 'ZEND_AST_SWITCH $3 $5)) r)\n    | T_BREAK optional_expr SEMI_COLON (let ((r)) (setq r (zend_ast_create 'ZEND_AST_BREAK $2)) r)\n    | T_CONTINUE optional_expr SEMI_COLON (let ((r)) (setq r (zend_ast_create 'ZEND_AST_CONTINUE $2)) r)\n    | T_RETURN optional_expr SEMI_COLON (let ((r)) (setq r (zend_ast_create 'ZEND_AST_RETURN $2)) r)\n    | T_GLOBAL global_var_list SEMI_COLON (let ((r)) (setq r $2) r)\n    | T_STATIC static_var_list SEMI_COLON (let ((r)) (setq r $2) r)\n    | T_ECHO echo_expr_list SEMI_COLON (let ((r)) (setq r $2) r)\n    | T_INLINE_HTML (let ((r)) (setq r (zend_ast_create 'ZEND_AST_ECHO $1)) r)\n    | expr SEMI_COLON (let ((r)) (setq r $1) r)\n    | T_UNSET OPEN_ROUND_BRACKET unset_variables possible_comma CLOSE_ROUND_BRACKET SEMI_COLON (let ((r)) (setq r $3) r)\n    | T_FOREACH OPEN_ROUND_BRACKET expr T_AS foreach_variable CLOSE_ROUND_BRACKET foreach_statement (let ((r)) (setq r (zend_ast_create 'ZEND_AST_FOREACH $3 $5 nil $7)) r)\n    | T_FOREACH OPEN_ROUND_BRACKET expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable CLOSE_ROUND_BRACKET foreach_statement (let ((r)) (setq r (zend_ast_create 'ZEND_AST_FOREACH $3 $7 $5 $9)) r)\n    | T_DECLARE OPEN_ROUND_BRACKET const_list CLOSE_ROUND_BRACKET (let ((r)) (if (not (zend_handle_encoding_declaration $3)) (setq r 'YYERROR)) r) declare_statement (let ((r)) (setq r (zend_ast_create 'ZEND_AST_DECLARE $3 $6)) r)\n    | SEMI_COLON ;; empty statement (let ((r)) (setq r nil) r)\n    | T_TRY OPEN_CURLY_BRACKET inner_statement_list CLOSE_CURLY_BRACKET catch_list finally_statement (let ((r)) (setq r (zend_ast_create 'ZEND_AST_TRY $3 $5 $6)) r)\n    | T_GOTO T_STRING SEMI_COLON (let ((r)) (setq r (zend_ast_create 'ZEND_AST_GOTO $2)) r)\n    | T_STRING COLON (let ((r)) (setq r (zend_ast_create 'ZEND_AST_LABEL $1)) r)\n    ;\n\n\n%%\n\n;; NOTE Generated grammar ends here"
      )))
  (message "Passed whole file test with multiple logic blocks in rule 2")

  (message "Unit tests for parsing entire buffers ended.\n")

  )

(defun emacs-wisent-grammar-converter-test--reformat-logic-block ()
  "Test conversion of C to Wisent Emacs-Lisp."
  (message "Unit tests for converting C to Wisent Emacs-Lisp started.\n")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$$ = zend_ast_append_str($1, $3);")
    "(let ((r)) (setq r (zend_ast_append_str $1 $3)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 1")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$$ = zend_ast_create(ZEND_AST_HALT_COMPILER,
			      zend_ast_create_zval_from_long(zend_get_scanned_file_offset()));
			  zend_stop_lexing();")
    "(let ((r)) (setq r (zend_ast_create 'ZEND_AST_HALT_COMPILER (zend_ast_create_zval_from_long (zend_get_scanned_file_offset))))(zend_stop_lexing) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 2")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "			zval zv;
			zend_lex_tstring(&zv);
			$$ = zend_ast_create_zval(&zv);
")
    "(let ((r)(zv)) (zend_lex_tstring (lambda(return) (setq zv return)))(setq r (zend_ast_create_zval zv)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 3")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = zend_ast_list_add($1, $3); ")
    "(let ((r)) (setq r (zend_ast_list_add $1 $3)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 4")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = NULL; zend_throw_exception(zend_ce_compile_error,
			      \"__HALT_COMPILER() can only be used from the outermost scope\", 0); YYERROR; ")
    "(let ((r)) (setq r nil)(zend_throw_exception zend_ce_compile_error \"__HALT_COMPILER() can only be used from the outermost scope\" 0)(setq r 'YYERROR) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 5")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = zend_ast_create_decl(ZEND_AST_METHOD, $3 | $1 | $12, $2, $5,
				  zend_ast_get_str($4), $7, NULL, $11, $9); CG(extra_fn_flags) = $10; ")
    "(let ((r)) (setq r (zend_ast_create_decl 'ZEND_AST_METHOD (logior $3 (logior $1 $12)) $2 $5 (zend_ast_get_str $4) $7 nil $11 $9))(cg 'extra_fn_flags $10) r)"))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 6")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     " $$ = zend_add_class_modifier($1, $2); if (!$$) { YYERROR; }")
    "(let ((r)) (setq r (zend_add_class_modifier $1 $2))(if (not r) (setq r 'YYERROR)) r)"))
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

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "if (!zend_handle_encoding_declaration($3)) { YYERROR; }")
    "(let ((r)) (if (not (zend_handle_encoding_declaration $3)) (setq r 'YYERROR)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 10")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$<num>$ = CG(zend_lineno);")
    "(let ((r)) (setq r (cg zend_lineno)) r)"
    ))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 11")

  (should
   (equal
    (emacs-wisent-grammar-converter--reformat-logic-block
     "$$ = $1; if (!($$ & ZEND_ACC_PPP_MASK)) { $$ |= ZEND_ACC_PUBLIC; } ")
    "(let ((r)) (setq r $1)(if (not (logand r 'ZEND_ACC_PPP_MASK)) (setq r (logior r 'ZEND_ACC_PUBLIC))) r)"))
  (message "Passed Bison-C to Wisent-Emacs Lisp test 12")


  (message "Unit tests for converting C to Wisent Emacs-Lisp completed.\n")
  )

(emacs-wisent-grammar-converter-test--reformat-logic-block)
(emacs-wisent-grammar-converter-test--parse-buffer)

(message "\nUnit tests for integration completed\n")

(provide 'emacs-wisent-grammar-converter-test)
;;; emacs-wisent-grammar-converter-test.el ends here
