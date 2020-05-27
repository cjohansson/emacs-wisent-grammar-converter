;;; emacs-wisent-grammar-converter.el --- Emacs Wisent Grammar Converter  -*- lexical-binding:t -*-

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 9 Aug 2018
;; Modified: 13 Apr 2020
;; Version: 0.2.1
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-wisent-grammar-converter

;; Package-Requires: ((emacs "24.1"))

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

;; This project converts a Bison YACC grammar into a Wisent grammar.


;;; Code:


(defun emacs-wisent-grammar-converter--string-trim (string)
  "Trim STRING from white-space."
  (emacs-wisent-grammar-converter--string-trim-left (emacs-wisent-grammar-converter--string-trim-right string)))

(defun emacs-wisent-grammar-converter--string-trim-left (string)
  "Trim STRING from white-space."
  (replace-regexp-in-string "^[\n\t\\ ]+" "" string))

(defun emacs-wisent-grammar-converter--string-trim-right (string)
  "Trim STRING from white-space."
  (replace-regexp-in-string "[\n\t\\ ]+$" "" string))

(defun emacs-wisent-grammar-converter--lex-c-string (string)
  "Run lexer on STRING, return list of tokens."
  (let ((tokens '())
        (continue t)
        (start 0))
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
          "null"
          string
          start)
         start)
        (push (list 'NULL (match-string 0 string)) tokens)
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
          "\\$\\$"
          string
          start)
         start)
        (push (list 'RETURN (match-string 0 string)) tokens)
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

;; NOTE Recursive shift/reduce parser - BEGINS here

(defvar emacs-wisent-grammar-converter--lexer-tokens-stack nil
  "A stack of lexer tokens to parse.")

(defun emacs-wisent-grammar-converter--parameter-to-plist (parameter)
  "Convert PARAMETER to corresponding property-list variable."
  (format
   "parameter-%s"
   (replace-regexp-in-string
    "\\$"
    ""
    parameter)))

(defun emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp (tokens &optional namespace)
  "Convert Bison grammar TOKENS into emacs-lisp using parser for each statement."
  (setq emacs-wisent-grammar-converter--lexer-tokens-stack tokens)

  ;; Set default value for namespace
  (unless namespace
    (setq namespace ""))
  
  (let ((return-string nil)
        (property-items '(("return-item" "$$"))))

    ;; Generate list of every parameter and return variable in block
    (dolist (item emacs-wisent-grammar-converter--lexer-tokens-stack)
      (when (equal (car item) 'PARAMETER)
        (let* ((property-value (car (cdr item)))
               (property-name
                (emacs-wisent-grammar-converter--parameter-to-plist property-value)))
          (push (list
                 property-name
                 property-value)
                property-items))))

    ;; Initialize all parameters and return value as property-lists
    (setq return-string "(let (")
    (dolist (item property-items)
      (setq
       return-string
       (concat
        return-string
        (format
         "(%s '(value %s))"
         (car item)
         (car (cdr item))))))
    (setq
     return-string
     (concat
      return-string
      ")"))

    (let ((last-token nil))
      (while emacs-wisent-grammar-converter--lexer-tokens-stack
        (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
               (token-id (car token))
               (token-value (car (cdr token))))
          ;; (message "token %s, id: %s, value: %s" token token-id token-value)
          (pcase token-id
            ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS 'DECLARATION))
            ('POINTER
             (unless (string= token-value "")
               (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter--variable token-value namespace)))))
            ('FUNCTION
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter--function token-value namespace))))
            ('VARIABLE
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter--variable token-value namespace))))
            ('PARAMETER
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter--parameter token-value namespace))))
            ('RETURN
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter--return namespace))))
            ('SEMICOLON)
            (_ (signal 'error (list (format "Unexpected root token %s" token)))))
          (setq last-token token))))

    (format "%s return-item)" return-string)))

(defun emacs-wisent-grammar-converter--variable (name namespace)
  "Parse variable NAME in NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('ASSIGNMENT
       (let ((formatted-name (concat namespace name)))
         (format
          "(setq %s %s)"
          formatted-name
          (emacs-wisent-grammar-converter--token-value namespace))))
      ('MEMBER_OPERATOR
       (format
        "(put %s%s '%s)"
        namespace
        name
        (emacs-wisent-grammar-converter--member-operator name namespace)))
      (_ (signal 'error (list (format "Unexpected variable token %s" token)))))))

(defun emacs-wisent-grammar-converter--parameter (name namespace)
  "Parse parameter NAME in NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('ASSIGNMENT
       (format
        "(plist-put %s 'value %s)"
        (emacs-wisent-grammar-converter--parameter-to-plist name)
        (emacs-wisent-grammar-converter--token-value namespace)))
      ('MEMBER_OPERATOR
       (format
        "(plist-put %s %s)"
        (emacs-wisent-grammar-converter--parameter-to-plist name)
        (emacs-wisent-grammar-converter--member-operator
         (emacs-wisent-grammar-converter--parameter-to-plist name)
         namespace)))
      (_ (signal 'error (list (format "Unexpected parameter token %s" token)))))))

(defun emacs-wisent-grammar-converter--return (namespace)
  "Parse return in NAMESPACE."
  (let ((return-string "")
        (continue t))
    (while (and
            continue
            emacs-wisent-grammar-converter--lexer-tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('ASSIGNMENT
           (setq
            return-string
            (format
             "(plist-put return-item 'value %s)"
             (emacs-wisent-grammar-converter--token-value namespace)))
           (setq continue nil))
          ('BITWISE_OR_ASSIGNMENT
           (setq
            return-string
            (format
             "(plist-put return-item 'value (logior (plist-get return-item 'value) %s))"
             (emacs-wisent-grammar-converter--token-value namespace)))
           (setq continue nil))
          ('BITWISE_AND_ASSIGNMENT
           (setq
            return-string
            (format
             "(plist-put return-item 'value (logand (plist-get return-item 'value) %s))"
             (emacs-wisent-grammar-converter--token-value namespace)))
           (setq continue nil))
          ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS))
          ('MEMBER_OPERATOR
           (setq
            return-string
            (format
             "(plist-put return-string %s)"
             (emacs-wisent-grammar-converter--member-operator
              "return-item"
              namespace)))
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected variable token %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter--function (name namespace)
  "Parse function NAME and NAMESPACE."
  ;; Skip first OPENING_PARENTHESIS
  (pop emacs-wisent-grammar-converter--lexer-tokens-stack)

  (let ((argument-string nil)
        (continue t)
        (return-string nil)
        (closed nil))
    (while (and continue
                emacs-wisent-grammar-converter--lexer-tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('ASSIGNMENT
           (unless closed
             (signal 'err (list (format "Unexpected function assignment %s" token))))
           (setq continue nil)
           (if argument-string
               (setq
                return-string
                (format
                 "(%s%s '%s %s)"
                 namespace
                 name
                 argument-string
                 (emacs-wisent-grammar-converter--token-value namespace)))
             (setq
              return-string
              (format
               "(%s%s %s)"
               namespace
               name
               (emacs-wisent-grammar-converter--token-value namespace)))))
          ('BITWISE_OR_ASSIGNMENT
           (unless closed
             (signal 'err (list (format "Unexpected function assignment %s" token))))
           (setq continue nil)
           (if argument-string
               (setq
                return-string
                (format
                 "(%s%s '%s (logior (%s%s '%s) %s))"
                 namespace
                 name
                 argument-string
                 namespace
                 name
                 argument-string
                 (emacs-wisent-grammar-converter--token-value namespace)))
             (setq
              return-string
              (format
               "(%s%s (logior (%s%s) %s))"
               namespace
               name
               namespace
               name
               (emacs-wisent-grammar-converter--token-value namespace)))))
          ('BITWISE_AND_ASSIGNMENT
           (unless closed
             (signal 'err (list (format "Unexpected function assignment %s" token))))
           (setq continue nil)
           (if argument-string
               (setq
                return-string
                (format
                 "(%s%s '%s (logand (%s%s '%s) %s))"
                 namespace
                 name
                 argument-string
                 namespace
                 name
                 argument-string
                 (emacs-wisent-grammar-converter--token-value namespace)))
             (setq
              return-string
              (format
               "(%s%s (logand (%s%s) %s))"
               namespace
               name
               namespace
               name
               (emacs-wisent-grammar-converter--token-value namespace)))))
          ('CLOSE_PARENTHESIS
           (setq closed t)
           (if argument-string
               (setq
                return-string
                (format
                 "(%s%s %s)"
                 namespace
                 name
                 argument-string))
             (setq
              return-string
              (format
               "(%s%s)"
               namespace
               name))))
          (_
           (if closed
               (progn
                 (setq continue nil)
                 (push
                  token
                  emacs-wisent-grammar-converter--lexer-tokens-stack))
             (push
              token
              emacs-wisent-grammar-converter--lexer-tokens-stack)
             (setq
              argument-string
              (emacs-wisent-grammar-converter--function-arguments token namespace)))))))
    return-string))

(defun emacs-wisent-grammar-converter--function-arguments (token namespace)
  "Parse function arguments starting at TOKEN with NAMESPACE."
  (let ((return-string "")
        (return-count 0)
        (continue t)
        (bracket-level 1)
        (in-ternary nil))
    (while (and continue
                emacs-wisent-grammar-converter--lexer-tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('COMMA)
          ('OPEN_PARENTHESIS
           (setq bracket-level (1+ bracket-level)))
          ((or 'VARIABLE 'FUNCTION 'PARAMETER 'NULL)
           (let ((parsed-token-value (emacs-wisent-grammar-converter--token-value namespace token)))
             (let ((next-is-bitwise-or
                    (and
                     emacs-wisent-grammar-converter--lexer-tokens-stack
                     (equal (car (car emacs-wisent-grammar-converter--lexer-tokens-stack)) 'BITWISE_OR)))
                   (next-is-ternary
                    (and
                     emacs-wisent-grammar-converter--lexer-tokens-stack
                     (equal (car (car emacs-wisent-grammar-converter--lexer-tokens-stack)) 'QUESTION_MARK)))
                   (next-is-bitwise-and
                    (and
                     emacs-wisent-grammar-converter--lexer-tokens-stack
                     (equal (car (car emacs-wisent-grammar-converter--lexer-tokens-stack)) 'BITWISE_AND))))
               (when (> return-count 0)
                 (setq return-string (concat return-string " ")))
               (cond
                (next-is-ternary
                 (pop emacs-wisent-grammar-converter--lexer-tokens-stack)
                 (let ((ternary-true (emacs-wisent-grammar-converter--token-value namespace))
                       (ternary-false ""))
                   ;; Pop the :
                   (pop emacs-wisent-grammar-converter--lexer-tokens-stack)
                   (setq ternary-false (emacs-wisent-grammar-converter--token-value namespace))

                   (setq
                    return-string
                    (concat
                     return-string
                     (format
                      "(if %s %s %s)"
                      parsed-token-value
                      ternary-true
                      ternary-false)))))
                ((or next-is-bitwise-or
                     next-is-bitwise-and)
                 (pop emacs-wisent-grammar-converter--lexer-tokens-stack)
                 (let ((next-token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
                       (operator "logior"))
                   (when next-is-bitwise-and
                     (setq operator "logand"))
                   (setq
                    return-string
                    (concat
                     return-string
                     (format
                      "(%s %s %s)"
                      operator
                      parsed-token-value
                      (emacs-wisent-grammar-converter--token-value namespace next-token))))))
                (t
                 (setq
                  return-string
                  (concat
                   return-string
                   parsed-token-value))))
               (setq return-count (1+ return-count)))))
          ('CLOSE_PARENTHESIS
           (setq bracket-level (1- bracket-level))
           (when (= bracket-level 0)
             (push
              token
              emacs-wisent-grammar-converter--lexer-tokens-stack)
             (setq continue nil)))
          (_ (signal 'error (list (format "Unexpected function arguments token: %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter--token-value (namespace &optional token)
  "Return TOKEN value."
  (unless token
    (setq
     token
     (pop emacs-wisent-grammar-converter--lexer-tokens-stack)))
  (let* ((token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('VARIABLE
       (format
        "%s%s"
        namespace
        token-value))
      ('PARAMETER
       (emacs-wisent-grammar-converter--parameter-to-plist token-value))
      ('NULL
       "nil")
      ('FUNCTION
       (emacs-wisent-grammar-converter--function token-value namespace))
      (_ (signal 'error (list (format "Unexpected token value token: %s" token)))))))

;; This function supports stuff like ->attr = abc; ->attr) ->attr, ->attr;
(defun emacs-wisent-grammar-converter--member-operator (parent namespace)
  "Parse member-operator of PARENT using NAMESPACE."
  (let ((continue t)
        (return-string "")
        (variable ""))
    (while (and
            continue
            emacs-wisent-grammar-converter--lexer-tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('VARIABLE
           (setq
            return-string
            (concat
             return-string
             "'"
             token-value))
           (setq variable token-value))
          ('BITWISE_OR_ASSIGNMENT
           (setq
            return-string
            (concat
             return-string
             (format
              " (logior (plist-get %s '%s) "
              parent
              variable)
             (emacs-wisent-grammar-converter--token-value namespace)
             ")"))
           (setq continue nil))
          ('BITWISE_AND_ASSIGNMENT
           (setq
            return-string
            (concat
             return-string
             (format
              " (logand (plist-get %s '%s) "
              parent
              variable)
             (emacs-wisent-grammar-converter--token-value namespace)
             ")"))
           (setq continue nil))
          ('ASSIGNMENT
           (setq
            return-string
            (concat
             return-string
             " "
             (emacs-wisent-grammar-converter--token-value namespace)))
           (setq continue nil))
          ('SEMICOLON
           (setq continue nil)
           (push token emacs-wisent-grammar-converter--lexer-tokens-stack))
          ('COMMA
           (setq continue nil)
           (push token emacs-wisent-grammar-converter--lexer-tokens-stack))
          ('CLOSE_PARENTHESIS
           (setq continue nil)
           (push token emacs-wisent-grammar-converter--lexer-tokens-stack))
          (_ (signal 'error (list (format "Unexpected member-operator token: %s" token)))))))
    return-string))


;; NOTE Shift/reduce parser - ENDS here


(defun emacs-wisent-grammar-converter--generate-grammar-from-filename (source destination &optional header prefix)
  "Convert grammar in SOURCE to DESTINATION, prepend HEADER if specified, use PREFIX if specified.  Return the conversion as a string."
  (let* ((buffer (generate-new-buffer destination)))
    (switch-to-buffer buffer)
    (insert-file-contents source)

    ;; Remove unnecessary starting and ending stuff
    (let ((start (point))
          (level "root")
          (continue t)
          (grammar "")
          (block "")
          (rule "")
          (logic-start 0)
          (logic-end 0)
          (logic)
          (parse-stack '())
          (last-was-block-comment nil)
          (rule-token-count 0))

      ;; Iterate through entire buffer starting from start
      (goto-char start)
      (while continue
        (pcase level

          ("root"

           ;; Can we find something like    abc_def:
           (if (search-forward-regexp "\n+\\([a-z_]+\\)[\n\t ]*:" nil t)

               (progn
                 (setq grammar (concat grammar (format "\n%s:\n    " (match-string 1))))
                 (setq block (match-string 1))
                 (setq rule "")
                 (setq rule-token-count 0)
                 (message "Found block '%s'" block)
                 (setq level "block"))

             (progn
               (message "Failed to find block-start")
               (setq continue nil))))

          ("block"

           ;; Can we find a | or { or ; character?
           (if (search-forward-regexp "\\(|\\|{\\|;\\|}\\|\'\\|\"\\|/\\*\\|[a-zA-Z_]+\\)" nil t)

               (cond

                ;; Is it a rule delimiter (| or ;)?
                ((or (string= (match-string 1) "|")
                     (string= (match-string 1) ";"))

                 (let ((matches-delimiter (string= (match-string 1) "|"))
                       (matches-end (string= (match-string 1) ";")))

                   ;; Did last action add a new-line?
                   (setq grammar (concat grammar rule))
                   (setq rule "")

                   ;; Is it the start of a new rule?
                   (when matches-delimiter
                     (setq rule-token-count 0)
                     (setq grammar (concat grammar "\n    | "))
                     (message "Found another rule in block"))

                   ;; Is it the end of a block?
                   (when matches-end
                     (setq grammar (concat grammar "\n    ;\n"))
                     (setq rule-token-count 0)
                     (message "Ended block")
                     (setq level "root"))))

                ;; Is it a logic start delimiter?
                ((string= (match-string 1) "{")
                 (if (and (> rule-token-count 0) (not last-was-block-comment))
                     (setq rule (concat rule " "))
                   (setq rule (concat rule "\n    ")))
                 (setq rule (concat rule "("))
                 (setq last-was-block-comment nil)
                 (setq logic-start (point))
                 (setq level "logic"))

                ;; Is it a single-quote?
                ((string= (match-string 1) "'")
                 (let ((quote-start (point))
                       (quote-end)
                       (quote))
                   (if (search-forward-regexp "\'" nil t)
                       (progn
                         (setq quote-end (point))
                         (setq quote (emacs-wisent-grammar-converter--string-trim (buffer-substring (- quote-start 1) quote-end)))
                         (when (> rule-token-count 0)
                           (setq rule (concat rule " ")))
                         (setq rule (concat rule quote))
                         (setq rule-token-count (+ rule-token-count 1)))
                     (progn
                       (message "Failed to find ending single-quote")
                       (setq continue nil)))))

                ;; Is it a double-quote?
                ((string= (match-string 1) "\"")
                 (let ((quote-start (point))
                       (quote-end)
                       (quote))
                   (if (search-forward-regexp "\"" nil t)
                       (progn
                         (setq quote-end (point))
                         (setq quote (emacs-wisent-grammar-converter--string-trim (buffer-substring (- quote-start 1) quote-end 1)))
                         (when (> rule-token-count 0)
                           (setq rule (concat rule " ")))
                         (setq rule (concat rule quote))
                         (setq rule-token-count (+ rule-token-count 1)))
                     (progn
                       (message "Failed to find ending double-quote")
                       (setq continue nil)))))

                ;; Is it a logic end delimiter?
                ((string= (match-string 1) "}")
                 (message "Invalid grammar, breaking")
                 (setq continue nil))

                ;; Is it a Cdoc comment block start
                ((string= (match-string 1) "/*")
                 (let ((comment-start (point))
                       (comment-end)
                       (comment))
                   (if (search-forward-regexp "*/" nil t)
                       (progn
                         (setq comment-end (point))
                         (setq comment (emacs-wisent-grammar-converter--string-trim (buffer-substring comment-start (- comment-end 2))))
                         (when (> rule-token-count 0)
                           (setq rule (concat rule " ")))
                         (setq rule (concat rule ";; " (emacs-wisent-grammar-converter--string-trim comment)))
                         (setq last-was-block-comment t))
                     (progn
                       (message "Failed to find ending doc comment block")
                       (setq continue nil)))))

                (t (progn
                     (when (> rule-token-count 0)
                       (setq rule (concat rule " ")))
                     (setq rule (concat rule (match-string 1)))
                     (setq last-was-block-comment nil)
                     (setq rule-token-count (+ rule-token-count 1)))))

             (progn
               (message "Failed to find rule delimiter")
               (setq continue nil))))

          ("logic"

           ;; Can we find a { or } character?
           (if (search-forward-regexp "\\({\\|}\\)" nil t)

               (cond

                ((string= (match-string 1) "}")
                 (setq logic-end (point))
                 (setq logic (emacs-wisent-grammar-converter--string-trim (buffer-substring logic-start (- logic-end 1))))

                 (setq logic (emacs-wisent-grammar-converter--reformat-logic-block logic))
                 
                 (setq rule (concat rule logic ")"))

                 (let ((previous (pop parse-stack)))
                   ;; Do we have a parse-stack?
                   (if previous
                       (setq logic-start (point))
                     (progn
                       (setq level "block")))))

                ;; Is it the start of nested logic?
                ((string= (match-string 1) "{")
                 (setq logic-end (point))
                 (setq logic (emacs-wisent-grammar-converter--string-trim (buffer-substring logic-start (- logic-end 1))))
                 (setq rule (concat rule " (" logic))
                 (push (list logic-start logic-end) parse-stack)
                 (setq logic-start (point))))

             (progn
               (message "Failed to find logic end")
               (setq continue nil))))

          (_ (setq continue nil))))

      ;; Clear buffer
      (delete-region (point-min) (point-max))

      ;; Prepend header if specified
      (when header
        (goto-char (point-min))
        (insert-file-contents header)
        (goto-char (point-max))
        (insert "\n\n"))

      (goto-char (point-max))

      (insert ";; NOTE Generated grammar starts here\n\n")
      (insert grammar)
      (insert "\n\n;; NOTE Generated grammar ends here")

      ;; Untabify and clean-up white-spaces
      (untabify (point-min) (point-max))
      (whitespace-cleanup)

      (write-file destination)

      ;; Return current buffer as string
      (buffer-string))))


(provide 'emacs-wisent-grammar-converter)
;;; emacs-wisent-grammar-converter.el ends here
