# Emacs Wisent Grammar Converter

As a first goal this project is about creating elisp functions that convert Yacc parser-generator grammar into Wisent. In the future maybe more parser generator grammars could be supported as well.

## The problem

I was about creating a Emacs PHP mode with full semantic support and there is a Yacc grammar available, I needed to convert this to Wisent but I dreaded the idea of having to do it manually so I started working on this. 

After a while I though maybe other people could find this useful as well so I started this project.

## Main Goal

1. Convert PHP Language Yacc Grammar to valid Wisent Grammar

## Example conversion of PHP Yacc

``` emacs-lisp
;; Your source path here
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-wisent-grammar-converter/"))
(require 'emacs-wisent-grammar-converter)

;; Change Yacc definition paths here
(let ((php-yacc-url "https://raw.githubusercontent.com/php/php-src/master/Zend/zend_language_parser.y")
      (php-yacc-file (expand-file-name "zend_language_parser.y"))
      (wisent-destination (expand-file-name "zend_language_parser.wy")))

  ;; Download Yacc if not available
  (unless (file-exists-p php-yacc-file)
    (message "Downloading PHP Yacc grammar..")
    (url-copy-file php-yacc-url php-yacc-file t t)
    (message "Downlad completed"))

  ;; Generate grammar
  (message "Generating Wisent grammar..")
  (emacs-wisent-grammar-converter/generate-grammar-from-filename php-yacc-file wisent-destination)
  (message "Automation completed"))
```

