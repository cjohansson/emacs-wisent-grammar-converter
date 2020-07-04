;;; emacs-wisent-grammar-converter-lexer.el --- Emacs Wisent Grammar Converter Lexer  -*- lexical-binding:t -*-

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

;; This file lexes a string of C-code into tokens.


;;; Code:


(defun emacs-wisent-grammar-converter-lexer--lex-c-string (string)
  "Run lexer on STRING, return list of tokens."
  (let ((tokens '())
        (continue t)
        (start 0)
        (case-fold-search nil))
    (while continue
      (cond
       ((equal
         (string-match
          "/\\* \\(.+\\) \\*/"
          string
          start)
         start)
        (push (list 'DOC_COMMENT (match-string 1 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\([a-zA-Z0-9_]+\\)("
          string
          start)
         start)
        (push (list 'FUNCTION (match-string 1 string)) tokens)
        (setq start (match-end 1)))
       ((equal
         (string-match
          "&\\([a-zA-Z0-9_]+\\)"
          string
          start)
         start)
        (push (list 'REFERENCE (match-string 1 string)) tokens)
        (setq start (match-end 1)))
       ((equal
         (string-match
          "\*\\([a-zA-Z0-9_]+\\)"
          string
          start)
         start)
        (push (list 'POINTER (match-string 1 string)) tokens)
        (setq start (match-end 1)))
       ((equal
         (string-match
          "\\(if\\|IF\\)[\n\t ]*("
          string
          start)
         start)
        (push (list 'IF (match-string 1 string)) tokens)
        (setq start (match-end 1)))
       ((equal
         (string-match
          "!"
          string
          start)
         start)
        (push (list 'LOGICAL_NOT (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "||"
          string
          start)
         start)
        (push (list 'LOGICAL_OR (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "&&"
          string
          start)
         start)
        (push (list 'LOGICAL_AND (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\([a-zA-Z0-9_]+\\)[\t\n ]+\\([a-zA-Z_][a-zA-Z0-9_]*\\|\*\\([a-zA-Z_]*[a-zA-Z0-9_]*\\)\\)"
          string
          start)
         start)
        (push (list 'DECLARATION (match-string 1 string)) tokens)
        (if (string-match-p "\*[a-zA-Z_]*[a-zA-Z0-9_]*" (match-string 2 string))
            (progn
              (push (list 'POINTER (match-string 3 string)) tokens)
              (setq start (match-end 3)))
          (push (list 'VARIABLE (match-string 2 string)) tokens)
          (setq start (match-end 2))))
       ((equal
         (string-match
          "\"\\([^\"]*\\)\""
          string
          start)
         start)
        (push (list 'STRING (match-string 1 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\(null\\|NULL\\)"
          string
          start)
         start)
        (push (list 'NULL (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "[A-Z_]+[A-Z_0-9]*"
          string
          start)
         start)
        (push (list 'SYMBOL (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "[a-zA-Z_][a-zA-Z0-9_]*"
          string
          start)
         start)
        (push (list 'VARIABLE (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "[0-9_]+"
          string
          start)
         start)
        (push (list 'INTEGER (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "?"
          string
          start)
         start)
        (push (list 'QUESTION_MARK (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          ":"
          string
          start)
         start)
        (push (list 'COLON (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "->"
          string
          start)
         start)
        (push (list 'MEMBER_OPERATOR (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "-"
          string
          start)
         start)
        (push (list 'SUBTRACTION (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\+"
          string
          start)
         start)
        (push (list 'ADDITION (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\*"
          string
          start)
         start)
        (push (list 'ASTERIX (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "/"
          string
          start)
         start)
        (push (list 'DIVISION (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\$\\(<[a-zA-Z]+>\\)?\\$"
          string
          start)
         start)
        (push (list 'RETURN "$$") tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\$\\(<[a-zA-Z]+>\\)?\\([0-9]+\\)"
          string
          start)
         start)
        ;; $<num>2 is actually a casted value that is not supported in lisp
        (if (string-match-p
             "\\$<[a-zA-Z]+>[0-9]+"
             (match-string 0 string))
            (progn
              (push
               (list
                'PARAMETER
                (concat "$" (match-string 2 string)))
               tokens))
          (push
           (list
            'PARAMETER
            (match-string 0 string))
           tokens))
        (setq start (match-end 0)))
       ((equal
         (string-match
          "("
          string
          start)
         start)
        (push (list 'OPEN_PARENTHESIS (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          ")"
          string
          start)
         start)
        (push (list 'CLOSE_PARENTHESIS (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "{"
          string
          start)
         start)
        (push (list 'OPEN_CURLY_BRACKET (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "}"
          string
          start)
         start)
        (push (list 'CLOSE_CURLY_BRACKET (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\["
          string
          start)
         start)
        (push (list 'OPEN_SQUARE_BRACKET (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\]"
          string
          start)
         start)
        (push (list 'CLOSE_SQUARE_BRACKET (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          ";"
          string
          start)
         start)
        (push (list 'SEMICOLON (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          ","
          string
          start)
         start)
        (push (list 'COMMA (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\."
          string
          start)
         start)
        (push (list 'DOT (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "=="
          string
          start)
         start)
        (push (list 'EQUAL (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "=="
          string
          start)
         start)
        (push (list 'EQUAL (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "|="
          string
          start)
         start)
        (push (list 'BITWISE_OR_ASSIGNMENT (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "|"
          string
          start)
         start)
        (push (list 'BITWISE_OR (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "&"
          string
          start)
         start)
        (push (list 'BITWISE_AND (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "&="
          string
          start)
         start)
        (push (list 'BITWISE_AND_ASSIGNMENT (match-string 0 string)) tokens)
        (setq start (match-end 1)))
       ((equal
         (string-match
          "="
          string
          start)
         start)
        (push (list 'ASSIGNMENT (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       (t
        (if (equal
             (string-match
              "[\t\n ]+"
              string
              start)
             start)
            (setq start (match-end 0))
          (setq continue nil)))))
    (nreverse tokens)))

(provide 'emacs-wisent-grammar-converter-lexer)
;;; emacs-wisent-grammar-converter-lexer.el ends here
