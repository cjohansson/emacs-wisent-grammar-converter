# Emacs Wisent Grammar Converter

As a first goal this project is about creating elisp functions that convert YACC parser-generator grammar into Wisent. In the future maybe more parser generator grammars could be supported as well.

## The problem

I was about creating a Emacs PHP mode with full semantic support and there is a YACC grammar available, I needed to convert this to Wisent but I dreaded the idea of having to do it manually so I started working on this. Another issue is that PHP is in active development so the grammar would need to be manually updated from time to time.

After a while I though maybe other people could find this useful as well so I started this project.

## Main Goal

1. Convert PHP Language YACC Grammar to valid Wisent Grammar

## Example conversion of PHP Yacc

``` emacs-lisp
(let ((php-yacc-url "https://raw.githubusercontent.com/php/php-src/master/Zend/zend_language_parser.y")
      (php-yacc-file (expand-file-name "zend_language_parser.y"))
      (wisent-destination (expand-file-name "../phps-mode-parser-grammar-raw.wy"))
      (header (expand-file-name "phps-mode-automation-header.wy"))
      (terminal-replacements (make-hash-table :test 'equal)))

  (puthash "'+'" "ADDITION" terminal-replacements)
  (puthash "'='" "ASSIGN" terminal-replacements)
  (puthash "'@'" "AT" terminal-replacements)
  (puthash "'`'" "BACKTICK" terminal-replacements)
  (puthash "'&'" "BITWISE_AND" terminal-replacements)
  (puthash "'|'" "BITWISE_OR" terminal-replacements)
  (puthash "'}'" "CLOSE_CURLY_BRACKET" terminal-replacements)
  (puthash "')'" "CLOSE_PARENTHESIS" terminal-replacements)
  (puthash "']'" "CLOSE_SQUARE_BRACKET" terminal-replacements)
  (puthash "':'" "COLON" terminal-replacements)
  (puthash "','" "COMMA" terminal-replacements)
  (puthash "'$'" "DOLLAR_SIGN" terminal-replacements)
  (puthash "'\"'" "DOUBLE_QUOTE" terminal-replacements)
  (puthash "'/'" "DIVISION" terminal-replacements)
  (puthash "'.'" "DOT" terminal-replacements)
  (puthash "'>'" "GREATER_THAN" terminal-replacements)
  (puthash "'<'" "LESSER_THAN" terminal-replacements)
  (puthash "'%'" "MODULO" terminal-replacements)
  (puthash "'*'" "MULTIPLICATION" terminal-replacements)
  (puthash "'!'" "NEGATION" terminal-replacements)
  (puthash "'{'" "OPEN_CURLY_BRACKET" terminal-replacements)
  (puthash "'('" "OPEN_PARENTHESIS" terminal-replacements)
  (puthash "'['" "OPEN_SQUARE_BRACKET" terminal-replacements)
  (puthash "'^'" "POW" terminal-replacements)
  (puthash "'?'" "QUESTION_MARK" terminal-replacements)
  (puthash "';'" "SEMICOLON" terminal-replacements)
  (puthash "'''" "SINGLE_QUOTE" terminal-replacements)
  (puthash "'-'" "SUBTRACTION" terminal-replacements)
  (puthash "'~'" "UNARY" terminal-replacements)

  ;; Download Yacc if not available
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
       terminal-replacements)
    (display-warning
     'warning
     "Missing emacs-wisent-grammar-converter!"))
  (message "Automation completed"))
```

