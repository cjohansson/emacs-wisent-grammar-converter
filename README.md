# Emacs Wisent Grammar Converter

As a first goal this project is about creating elisp functions that convert YACC parser-generator grammar into Wisent. In the future maybe more parser generator grammars could be supported as well.

## The problem

I was about creating a Emacs PHP mode with full semantic support and there is a YACC grammar available, I needed to convert this to Wisent but I dreaded the idea of having to do it manually so I started working on this. Another issue is that PHP is in active development so the grammar would need to be manually updated from time to time.

After a while I though maybe other people could find this useful as well so I started this project.

## Sub-problems

* YACC used C-code in the rules, this needs to be converted to Emacs-Lisp. I solved this by using a shift/reduce top-down parser.
* YACC uses `$$` as return-value for each rule. I solved this by changing the structure of code to `(let ((r)) ... r)` to always return the equivalence of `$$`
* By using macros for some functions and ordinary functions for others, it easier to implement a grammar. I solved this by passing a macro-list hash-list argument that specifies what functions are to be used as macros and what are to be used as functions.

## Main Goal

1. Convert PHP Language YACC Grammar to valid Wisent Grammar (Completed)
2. Improve documentation

## Example conversion of PHP YACC from emacs-phps-mode

``` emacs-lisp
(let ((php-yacc-url "https://raw.githubusercontent.com/php/php-src/master/Zend/zend_language_parser.y")
      (php-yacc-file (expand-file-name "zend_language_parser.y"))
      (wisent-destination (expand-file-name "../phps-mode-parser.wy"))
      (header (expand-file-name "phps-mode-automation-header.wy"))
      (macro-list (make-hash-table :test 'equal)))

  (puthash "zend_ast_create" t macro-list)
  (puthash "zend_ast_create_assign_op" t macro-list)
  (puthash "zend_ast_create_binary_op" t macro-list)
  (puthash "zend_ast_create_cast" t macro-list)
  (puthash "zend_ast_create_class_const_or_name" t macro-list)
  (puthash "zend_ast_create_ex" t macro-list)
  (puthash "zend_ast_create_list" t macro-list)
  (puthash "zend_ast_create_zval" t macro-list)
  (puthash "zend_ast_list_add" t macro-list)
  (puthash "zend_ast_list_rtrim" t macro-list)
  (puthash "zend_lex_tstring" t macro-list)
  (puthash "zend_negate_num_string" t macro-list)
  (puthash "zval_interned_str" t macro-list)

  ;; download Yacc if not available
  (unless (file-exists-p php-yacc-file)
    (message "Downloading PHP Yacc grammar..")
    (url-copy-file php-yacc-url php-yacc-file t t)
    (message "Download completed"))

  ;; Generate grammar
  (message "Generating Wisent grammar..")
  (if (fboundp 'emacs-wisent-grammar-converter--generate-grammar-from-filename)
      (emacs-wisent-grammar-converter--generate-grammar-from-filename
       php-yacc-file
       wisent-destination
       header
       "phps-mode-parser--"
       macro-list)
    (display-warning
     'warning
     "Missing emacs-wisent-grammar-converter!"))
  (message "Automation completed"))
```

