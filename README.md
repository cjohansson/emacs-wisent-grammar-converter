# Emacs Wisent Grammar Converter

As a first goal this project is about creating Elisp functions that convert YACC parser-generator grammar into Wisent. In the future maybe more parser generator grammars could be supported as well.

## The problem

I was about creating a Emacs PHP mode with full semantic support and there is a YACC grammar available, I needed to convert this to Wisent but I dreaded the idea of having to do it manually so I started working on this. 

After a while I though maybe other people could find this useful as well so I started this project.

## Main Goal

1. Convert PHP Language YACC Grammar to valid Wisent Grammar

## Usage

``` emacs-lisp
;; Your source path here
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-wisent-grammar-converter/"))
(require 'emacs-wisent-grammar-converter)

;; Change YACC definition paths here
(let* ((source "~/Documents/php-src/Zend/zend_language_parser.y")
       (destination "zend_language_parser.wy"))
  (emacs-wisent-grammar-converter/generate-grammar-from-filename source destination))
```

