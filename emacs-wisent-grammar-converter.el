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

(defun emacs-wisent-grammar-converter--reformat-logic-block (logic &optional prefix)
  "Reformat LOGIC from C to elisp, use PREFIX if specified."
  (unless prefix
    (setq prefix ""))

  ;; Replace line-feeds and carriage returns with space
  (setq logic (replace-regexp-in-string "\\(\n\\|\r\\)+" " " logic))

  ;; Replace comma with space
  (setq logic (replace-regexp-in-string ",\\ +" " " logic))

  ;; Replace more than one space with a single space
  (setq logic (replace-regexp-in-string "\\(\\ \\|\t\\)\\(\\ \\|\t\\)+" " " logic))

  ;; Transform statements like    zend(a, b, c)    into    ($PREFIXzend a b c)
  (while (string-match "\\([a-zA-Z_-]+\\)(\\([^)]*\\))" logic)
    (if (string= (match-string 2 logic) "")
        (setq logic (replace-match (format "(%s%s)" prefix (match-string 1 logic)) t t logic))
      (setq logic (replace-match (format "(%s%s %s)" prefix (match-string 1 logic) (match-string 2 logic)) t t logic))))

  ;; Transform statements like    $$ = $1;    into    $1
  (while (string-match "$$ = $\\([0-9]+\\);" logic)
    (setq logic (replace-match (format "$%s" (match-string 1 logic)) t t logic)))

  ;; Transform statements like    $$ = (zend    into    ($PREFIXzend
  (while (string-match "$$ = \\(([a-zA-Z_]\\)" logic)
    (setq logic (replace-match (format "%s%s" prefix (match-string 1 logic)) t t logic)))

  ;; Transform statements like    $$->attr = ZEND_NAME_NOT_FQ;    into    (put $$ 'attr 'ZEND_NAME_NOT_FQ)
  (while (string-match "\\([\$a-zA-Z0-9]+\\)->\\([a-zA-Z0-9]+\\)[\\ ]*=[\\ ]*\\([^;]+\\);" logic)
    (setq logic (replace-match (format "(put %s '%s '%s)" (match-string 1 logic) (match-string 2 logic) (match-string 3 logic)) t t logic)))

  ;; Replace semi-colon with nothing
  (setq logic (replace-regexp-in-string ";" "" logic))

  ;; Transform doc block comments    /* blaha */ to \n;; blaha\n
  (while (string-match "/\\*\\(.+\\)\\*/" logic)
    (setq logic (replace-match (format "\n;;%s\n" (match-string 1 logic)) t t logic)))

  ;; Transform logical or operators    $1|$2    to    (logior $1 $2))
  (while (string-match "\\(\\$[0-9]+\\)[\\ ]*|[\\ ]*\\(\\$[0-9]+\\)" logic)
    (setq logic (replace-match (format "(logior %s %s)" (match-string 1 logic) (match-string 2 logic)) t t logic)))

  ;; Replace NULL with nil
  (setq logic (replace-regexp-in-string "NULL\\(?:[^A-Z]\\)" "nil" logic t t))

  (emacs-wisent-grammar-converter--string-trim logic))

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
          "\\([a-zA-Z0-9_]+\\)[\t\n ]+\\([a-zA-Z0-9_]+\\)"
          string
          start)
         start)
        (push (list 'DECLARATION (match-string 1 string)) tokens)
        (push (list 'VARIABLE (match-string 2 string)) tokens)
        (setq start (match-end 2)))
       ((equal
         (string-match
          "[a-zA-Z0-9_]+"
          string
          start)
         start)
        (push (list 'VARIABLE (match-string 0 string)) tokens)
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
          "\\$\\$"
          string
          start)
         start)
        (push (list 'RETURN (match-string 0 string)) tokens)
        (setq start (match-end 0)))
       ((equal
         (string-match
          "\\$[0-9]"
          string
          start)
         start)
        (push (list 'PARAMETER (match-string 0 string)) tokens)
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

;; NOTE Recursive descent parser - BEGINS here

(defvar emacs-wisent-grammar-converter--lexer-tokens-stack nil
  "A stack of lexer tokens to parse.")

(defun emacs-wisent-grammar-converter--converted-lexer-tokens-to-lisp (tokens &optional namespace)
  "Convert Bison grammar TOKENS into emacs-lisp using parser for each statement."
  (setq emacs-wisent-grammar-converter--lexer-tokens-stack tokens)
  (unless namespace
    (setq namespace ""))
  (let ((return-string nil)
        (return-block nil))
    (while emacs-wisent-grammar-converter--lexer-tokens-stack
      (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        ;; (message "token %s, id: %s, value: %s" token token-id token-value)
        (pcase token-id
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
            return-block
            (emacs-wisent-grammar-converter--return namespace)))
          ('SEMICOLON)
          (_ (signal 'error (list (format "Unexpected token %s" token)))))))

    ;; Return-block is placed last in block
    (when return-block
      (if return-string
          (setq
           return-string
           (concat
            return-string
            " "
            return-block))
        (setq return-string return-block)))

    (format "(%s)" return-string)))

(defun emacs-wisent-grammar-converter--variable (name namespace)
  "Parse variable NAME in NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('ASSIGNMENT
       (format
        "(setq %s%s %s)"
        namespace
        name
        (emacs-wisent-grammar-converter--assignment namespace)))
      ('MEMBER_OPERATOR
       (format
        "(put %s%s '%s)"
        namespace
        name
        (emacs-wisent-grammar-converter--member-operator namespace)))
      (_ (signal 'error (list (format "Unexpected variable token %s" token)))))))

(defun emacs-wisent-grammar-converter--parameter (name namespace)
  "Parse parameter NAME in NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('ASSIGNMENT
       (format
        "(setq %s %s)"
        name
        (emacs-wisent-grammar-converter--assignment namespace)))
      ('MEMBER_OPERATOR
       (format
        "(put %s '%s)"
        name
        (emacs-wisent-grammar-converter--member-operator namespace)))
      (_ (signal 'error (list (format "Unexpected parameter token %s" token)))))))

(defun emacs-wisent-grammar-converter--return (namespace)
  "Parse return in NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('ASSIGNMENT
       (emacs-wisent-grammar-converter--assignment namespace))
      ('MEMBER_OPERATOR
       (format
        "(put '%s)"
        (emacs-wisent-grammar-converter--assignment namespace)))
      (_ (signal 'error (list (format "Unexpected variable token %s" token)))))))

(defun emacs-wisent-grammar-converter--function (name namespace)
  "Parse function NAME and NAMESPACE."
  (pop emacs-wisent-grammar-converter--lexer-tokens-stack)
  (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('CLOSE_PARENTHESIS
       (format
        "(%s%s)"
        namespace
        name))
      (_
       (push token emacs-wisent-grammar-converter--lexer-tokens-stack)
       (format
          "(%s%s %s)"
          namespace
          name
          (emacs-wisent-grammar-converter--function-arguments token namespace))))))

(defun emacs-wisent-grammar-converter--function-arguments (token namespace)
  "Parse function arguments starting at TOKEN with NAMESPACE."
  (let ((return-string "")
        (return-count 0)
        (continue t))
    (while (and continue
                emacs-wisent-grammar-converter--lexer-tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('COMMA)
          ('VARIABLE
           (when (> return-count 0)
             (setq return-string (concat return-string " ")))
           (setq
            return-string
            (concat
             return-string
             (format
              "%s%s"
              namespace
              token-value)))
           (setq return-count (1+ return-count)))
          ('PARAMETER
           (when (> return-count 0)
             (setq return-string (concat return-string " ")))
           (setq return-string (concat return-string token-value))
           (setq return-count (1+ return-count)))
          ('FUNCTION
           (when (> return-count 0)
             (setq return-string (concat return-string " ")))
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter--function token-value namespace)))
           (setq return-count (1+ return-count)))
          ('CLOSE_PARENTHESIS
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected function arguments token: %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter--assignment (namespace)
  "Recursive descent for assignment  using NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter--lexer-tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('PARAMETER
       token-value)
      ('VARIABLE
       (format
        "%s%s"
        namespace
        token-value))
      ('FUNCTION
       (emacs-wisent-grammar-converter--function token-value namespace))
      ('NULL
       "nil")
      (_ (signal 'error (list (format "Unexpected assignment token: %s" token)))))))


;; NOTE Recursive descent parser - ENDS here


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
