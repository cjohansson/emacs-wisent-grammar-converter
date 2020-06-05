;;; emacs-wisent-grammar-converter-parser.el --- Emacs Wisent Grammar Converter Parser  -*- lexical-binding:t -*-

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

;; This file implements a shift/reduce parser on lexer tokens.


;;; Code:

(defvar emacs-wisent-grammar-converter-parser--tokens-stack nil
  "A stack of lexer tokens to parse.")

(defvar emacs-wisent-grammar-converter-parser--root-token-id nil
  "Current root token id.")

(defun emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp (tokens &optional namespace)
  "Convert Bison grammar TOKENS into emacs-lisp using parser for each statement."
  (setq emacs-wisent-grammar-converter-parser--tokens-stack tokens)

  ;; Set default value for namespace
  (unless namespace
    (setq namespace ""))
  
  (let ((return-string nil)
        (declaration-items nil))

    ;; Initialize all parameters and return value as property-lists
    (setq return-string "(let ((r)")

    ;; Add variables that are declared in body
    (let ((in-declaration nil))
      (dolist (item emacs-wisent-grammar-converter-parser--tokens-stack)
        (cond
         (in-declaration
          (let ((token-id (car item))
                (token-value (car (cdr item))))
            (pcase token-id
              ((or 'VARIABLE 'POINTER)
               (unless (string= token-value "")
                 (push token-value declaration-items))
               (setq in-declaration nil))
              ((or 'SEMICOLON)
               (setq in-declaration nil)))))
         ((equal (car item) 'DECLARATION)
          (setq in-declaration t)))))
    (when declaration-items
      (dolist (item declaration-items)
        (setq
         return-string
         (concat
          return-string
          (format
           "(%s)"
           item)))))

    (setq
     return-string
     (concat
      return-string
      ") "))

    (let ((last-token nil))
      (while emacs-wisent-grammar-converter-parser--tokens-stack
        (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
               (token-id (car token))
               (token-value (car (cdr token))))
          ;; (message "token %s, id: %s, value: %s" token token-id token-value)
          (setq
           emacs-wisent-grammar-converter-parser--root-token-id
           token-id)
          (pcase token-id
            ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS 'DECLARATION))
            ('POINTER
             (unless (string= token-value "")
               (setq
                return-string
                (concat
                 return-string
                 (emacs-wisent-grammar-converter-parser--variable token-value namespace)))))
            ('FUNCTION
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter-parser--function token-value namespace))))
            ('IF
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter-parser--if namespace))))
            ('SYMBOL
             (setq
              return-string
              (concat
               return-string
               (format
                "(setq r '%s%s)"
                namespace
                token-value))))
            ('DOC_COMMENT
             (setq
              return-string
              (concat
               return-string
               (format
                ";; %s\n"
                token-value))))
            ('VARIABLE
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter-parser--variable token-value namespace))))
            ('PARAMETER
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter-parser--parameter token-value namespace))))
            ('RETURN
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter-parser--return namespace))))
            ('SEMICOLON)
            (_ (signal 'error (list (format "Unexpected root token %s" token)))))
          (setq last-token token))))

    (format "%s r)" return-string)))

(defun emacs-wisent-grammar-converter-parser--variable (name namespace)
  "Parse variable NAME in NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('ASSIGNMENT
       (let ((formatted-name (concat namespace name)))
         (format
          "(setq %s %s)"
          formatted-name
          (emacs-wisent-grammar-converter-parser--token-value namespace))))
      ('SEMICOLON
       ;; A declaration has no equivalent in Emacs-Lisp
       "")
      ('MEMBER_OPERATOR
       (format
        "(semantic-tag-put-attribute %s%s '%s)"
        namespace
        name
        (emacs-wisent-grammar-converter-parser--member-operator name namespace)))
      (_ (signal 'error (list (format "Unexpected variable token %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))

(defun emacs-wisent-grammar-converter-parser--parameter (name namespace)
  "Parse parameter NAME in NAMESPACE."
  (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
         (token-id (car token))
         (token-value (car (cdr token))))
    (pcase token-id
      ('ASSIGNMENT
       (format
        "(setq %s %s)"
        name
        (emacs-wisent-grammar-converter-parser--token-value namespace)))
      ('MEMBER_OPERATOR
       (format
        "(semantic-tag-put-attribute %s %s)"
        name
        (emacs-wisent-grammar-converter-parser--member-operator
         name
         namespace)))
      (_ (signal 'error (list (format "Unexpected parameter token %s" token)))))))

(defun emacs-wisent-grammar-converter-parser--return (namespace)
  "Parse return in NAMESPACE."
  (let ((return-string "")
        (continue t))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('ASSIGNMENT
           (setq
            return-string
            (format
             "(setq r %s)"
             (emacs-wisent-grammar-converter-parser--token-value namespace)))
           (setq continue nil))
          ('BITWISE_OR_ASSIGNMENT
           (setq
            return-string
            (format
             "(setq r (logior r %s))"
             (emacs-wisent-grammar-converter-parser--token-value namespace)))
           (setq continue nil))
          ('BITWISE_AND_ASSIGNMENT
           (setq
            return-string
            (format
             "(setq r (logand r %s))"
             (emacs-wisent-grammar-converter-parser--token-value namespace)))
           (setq continue nil))
          ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS))
          ('MEMBER_OPERATOR
           (setq
            return-string
            (format
             "(semantic-tag-put-attribute r %s)"
             (emacs-wisent-grammar-converter-parser--member-operator
              "r"
              namespace)))
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected return token %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--function (name namespace)
  "Parse function NAME and NAMESPACE."
  ;; Skip first OPEN_PARENTHESIS token
  (pop emacs-wisent-grammar-converter-parser--tokens-stack)

  (let ((argument-string nil)
        (continue t)
        (return-string nil)
        (closed nil))
    (while (and continue
                emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
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
                 "(%s '%s %s)"
                 (upcase name)
                 argument-string
                 (emacs-wisent-grammar-converter-parser--token-value namespace)))
             (setq
              return-string
              (format
               "(%s %s)"
               (upcase name)
               (emacs-wisent-grammar-converter-parser--token-value namespace)))))
          ('BITWISE_OR_ASSIGNMENT
           (unless closed
             (signal 'err (list (format "Unexpected function assignment %s" token))))
           (setq continue nil)
           (if argument-string
               (setq
                return-string
                (format
                 "(%s '%s (logior (%s%s '%s) %s))"
                 (upcase name)
                 argument-string
                 (upcase namespace)
                 (upcase name)
                 argument-string
                 (emacs-wisent-grammar-converter-parser--token-value namespace)))
             (setq
              return-string
              (format
               "(%s (logior (%s) %s))"
               (upcase name)
               (upcase name)
               (emacs-wisent-grammar-converter-parser--token-value namespace)))))
          ('BITWISE_AND_ASSIGNMENT
           (unless closed
             (signal 'err (list (format "Unexpected function assignment %s" token))))
           (setq continue nil)
           (if argument-string
               (setq
                return-string
                (format
                 "(%s '%s (logand (%s '%s) %s))"
                 (upcase name)
                 argument-string
                 (upcase name)
                 argument-string
                 (emacs-wisent-grammar-converter-parser--token-value namespace)))
             (setq
              return-string
              (format
               "(%s (logand (%s) %s))"
               (upcase name)
               (upcase name)
               (emacs-wisent-grammar-converter-parser--token-value namespace)))))
          ('CLOSE_PARENTHESIS
           (when closed
             (push
              token
              emacs-wisent-grammar-converter-parser--tokens-stack)
             (setq continue nil))
           (unless closed
             (if argument-string
                 (setq
                  return-string
                  (format
                   "(%s %s)"
                   (upcase name)
                   argument-string))
               (setq
                return-string
                (format
                 "(%s)"
                 (upcase name))))
             (setq closed t)))
          (_
           (if closed
               (progn
                 (setq continue nil)
                 (push
                  token
                  emacs-wisent-grammar-converter-parser--tokens-stack))
             (push
              token
              emacs-wisent-grammar-converter-parser--tokens-stack)
             (setq
              argument-string
              (emacs-wisent-grammar-converter-parser--function-arguments token namespace)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--function-arguments (token namespace)
  "Parse function arguments starting at TOKEN with NAMESPACE."
  (let ((return-string "")
        (return-count 0)
        (continue t)
        (bracket-level 1)
        (in-ternary nil))
    (while (and continue
                emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        ;; (message "Function arguments token: %s" token)
        (pcase token-id
          ('COMMA)
          ('OPEN_PARENTHESIS
           (setq bracket-level (1+ bracket-level)))
          ((or 'VARIABLE 'FUNCTION 'PARAMETER 'SYMBOL 'NULL 'REFERENCE)
           (let ((parsed-value (emacs-wisent-grammar-converter-parser--token-value namespace token)))
           (when (> return-count 0)
             (setq
              return-string
              (concat
               return-string
               " ")))
           ;; (message "Parsed token '%s' value '%s'" token parsed-value)
           (setq
            return-string
            (concat
             return-string
             parsed-value))
           (setq return-count (1+ return-count))))
          ('STRING
           (when (> return-count 0)
             (setq
              return-string
              (concat
               return-string
               " ")))
           (setq
            return-string
            (concat
             return-string
             (format
              "\"%s\""
              token-value)))
           (setq return-count (1+ return-count)))
          ('INTEGER
           (when (> return-count 0)
             (setq
              return-string
              (concat
               return-string
               " ")))
           (setq
            return-string
            (concat
             return-string
             token-value))
           (setq return-count (1+ return-count)))
          ('CLOSE_PARENTHESIS
           (setq bracket-level (1- bracket-level))
           (when (= bracket-level 0)
             (push
              token
              emacs-wisent-grammar-converter-parser--tokens-stack)
             (setq continue nil)))
          (_ (signal 'error (list (format "Unexpected function arguments token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--infix-token-value (namespace first-token-value)
  "Return infix value of first-token-value in NAMESPACE."
  (let ((return-string "")
        (continue t)
        (in-subtraction nil)
        (in-addition nil)
        (in-bitwise-or nil)
        (in-bitwise-and nil))
    (while (and continue
                emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('ADDITION
           (when in-subtraction
             (signal 'error (list (format "Cannot combine subctractionwith addition in token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
           (unless in-subtraction
             (setq in-subtraction t)
             (setq
              return-string
              (concat
               "(- "
               first-token-value))))
          ('SUBTRACTION
           (when in-subtraction
             (signal 'error (list (format "Cannot combine subctractionwith addition in token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
           (unless in-subtraction
             (setq in-subtraction t)
             (setq
              return-string
              (concat
               "(- "
               first-token-value))))
          ('BITWISE_OR
           (when in-bitwise-and
             (signal 'error (list (format "Cannot combined bitwise OR with AND in token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
           (unless in-bitwise-or
             (setq in-bitwise-or t)
             (setq
              return-string
              (concat
               "(logior "
               first-token-value))))
          ('BITWISE_AND
           (when in-bitwise-or
             (signal 'error (list (format "Cannot combined bitwise OR with AND in token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
           (unless in-bitwise-and
             (setq in-bitwise-and t)
             (setq
              return-string
              (concat
               "(logand "
               first-token-value))))
          ((or 'VARIABLE 'PARAMETER 'SYMBOL 'FUNCTION 'STRING 'INTEGER)
           (unless (or in-bitwise-or in-bitwise-and in-subtraction in-addition)
             (signal 'error (list (format "Unexpected infix token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
           (let ((parsed-token-value (emacs-wisent-grammar-converter-parser--token-value namespace token)))
             (setq
              return-string
              (concat
               return-string
               " "
               parsed-token-value))))
          ((or 'SEMICOLON 'CLOSE_PARENTHESIS 'COMMA)
           (unless (or in-bitwise-or in-bitwise-and in-subtraction in-addition)
             (signal 'error (list (format "Unexpected infix token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
           (push token emacs-wisent-grammar-converter-parser--tokens-stack)
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected infix token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))
    (setq
     return-string
     (concat return-string ")"))
    return-string))

(defun emacs-wisent-grammar-converter-parser--token-value (namespace &optional token)
  "Return TOKEN value."
  (let ((return-string ""))
    (unless token
      (setq
       token
       (pop emacs-wisent-grammar-converter-parser--tokens-stack)))
    (let* ((token-id (car token))
           (token-value (car (cdr token))))
      (pcase token-id
        ('REFERENCE
         (pcase emacs-wisent-grammar-converter-parser--root-token-id
           ('RETURN
            (setq
             return-string
             token-value))
           ('FUNCTION
            (setq
             return-string
             (format
              "(lambda(return) (setq %s return))"
              token-value)))
           (_ (signal 'error (list (format "Unexpected reference root token id: %s, remaining tokens: %s" emacs-wisent-grammar-converter-parser--root-token-id emacs-wisent-grammar-converter-parser--tokens-stack))))))
        ('VARIABLE
         (setq
          return-string
          (format
           "%s%s"
           namespace
           token-value)))
        ('STRING
         (setq
          return-string
          (format
           "\"%s\""
           token-value)))
        ('INTEGER
         (setq
          return-string
          token-value))
        ('SYMBOL
         (setq
          return-string
          (format
           "'%s%s"
           namespace
           token-value)))
        ('RETURN
         (setq
          return-string
          "r"))
        ('PARAMETER
         (setq
          return-string
          token-value))
        ('NULL
         (setq
          return-string
          "nil"))
        ('FUNCTION
         (setq
          return-string
          (emacs-wisent-grammar-converter-parser--function token-value namespace)))
        (_ (signal 'error (list (format "Unexpected token value token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))))

    ;; Check next token here
    (when emacs-wisent-grammar-converter-parser--tokens-stack
      (let* ((next-token (car emacs-wisent-grammar-converter-parser--tokens-stack))
             (next-token-id (car next-token))
             (next-token-value (car (cdr next-token))))
        (pcase next-token-id
          ((or 'BITWISE_OR 'BITWISE_AND 'ADDITION 'MULTIPLICATION 'SUBTRACTION 'DIVISION)
           (let ((infix-value (emacs-wisent-grammar-converter-parser--infix-token-value namespace return-string)))
             ;; (message "Infix value of token '%s' is '%s'" token infix-value)
             (setq
              return-string
              infix-value)))
          ('QUESTION_MARK
           (pop emacs-wisent-grammar-converter-parser--tokens-stack)
           (let ((ternary-true (emacs-wisent-grammar-converter-parser--token-value namespace))
                 (ternary-false ""))

             ;; Pop the :
             (pop emacs-wisent-grammar-converter-parser--tokens-stack)
             (setq ternary-false (emacs-wisent-grammar-converter-parser--token-value namespace))
             (setq
              return-string
              (format
               "(if %s %s %s)"
               return-string
               ternary-true
               ternary-false)))))))
    return-string))

;; This function supports stuff like ->attr = abc; ->attr) ->attr, ->attr;
(defun emacs-wisent-grammar-converter-parser--member-operator (parent namespace)
  "Parse member-operator of PARENT using NAMESPACE."
  (let ((continue t)
        (return-string "")
        (variable ""))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
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
              " (logior (semantic-tag-get-attribute %s '%s) "
              parent
              variable)
             (emacs-wisent-grammar-converter-parser--token-value namespace)
             ")"))
           (setq continue nil))
          ('BITWISE_AND_ASSIGNMENT
           (setq
            return-string
            (concat
             return-string
             (format
              " (logand (semantic-tag-get-attribute %s '%s) "
              parent
              variable)
             (emacs-wisent-grammar-converter-parser--token-value namespace)
             ")"))
           (setq continue nil))
          ('ASSIGNMENT
           (setq
            return-string
            (concat
             return-string
             " "
             (emacs-wisent-grammar-converter-parser--token-value namespace)))
           (setq continue nil))
          ('SEMICOLON
           (setq continue nil)
           (push token emacs-wisent-grammar-converter-parser--tokens-stack))
          ('COMMA
           (setq continue nil)
           (push token emacs-wisent-grammar-converter-parser--tokens-stack))
          ('CLOSE_PARENTHESIS
           (setq continue nil)
           (push token emacs-wisent-grammar-converter-parser--tokens-stack))
          (_ (signal 'error (list (format "Unexpected member-operator token: %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--if (namespace)
  "Parser for IF statments in NAMESPACE."
  (let ((continue t)
        (condition-string "")
        (condition-subject-string "")
        (body-string "")
        (variable "")
        (parenthesis-level 0))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('LOGICAL_NOT
           (setq
            condition-string
            (concat
             condition-string
             (emacs-wisent-grammar-converter-parser--logical-prefix
              namespace
              token-id))))
          ((or 'LOGICAL_OR 'LOGICAL_AND 'EQUAL 'LOGICAL_NOT_EQUAL 'LOGICAL_GREATER 'LOGICAL_LESSER 'LOGICAL_GREATER_OR_EQUAL 'LOGICAL_LESSER_OR_EQUAL)
           (setq
            condition-string
            (concat
             condition-string
             (emacs-wisent-grammar-converter-parser--logical-infix
              condition-subject-string
              namespace
              token-id))))
          ((or 'PARAMETER 'RETURN)
           (if (= parenthesis-level 0)
               (progn
                 (push
                  token
                  emacs-wisent-grammar-converter-parser--tokens-stack)
                 (setq
                  body-string
                  (emacs-wisent-grammar-converter-parser--if-body-inline namespace))
                 (setq continue nil))
             (let ((next-is-member-operator
                    (and
                     emacs-wisent-grammar-converter-parser--tokens-stack
                     (equal
                      (car (car emacs-wisent-grammar-converter-parser--tokens-stack))
                      'MEMBER_OPERATOR))))
               (if next-is-member-operator
                   (progn
                     ;; Pop member operator
                     (pop emacs-wisent-grammar-converter-parser--tokens-stack)
                     (let ((attribute (pop emacs-wisent-grammar-converter-parser--tokens-stack))
                           (name "r"))
                       (when (equal token-id 'PARAMETER)
                         (setq
                          name
                          token-value))
                       (setq
                        condition-subject-string
                        (format
                         "(semantic-tag-get-attribute %s '%s)"
                         name
                         (car (cdr attribute))))))
                 (setq
                  condition-subject-string
                  (emacs-wisent-grammar-converter-parser--token-value
                   namespace
                   token))))))
          ((or 'VARIABLE 'SYMBOL 'FUNCTION 'STRING 'INTEGER)
           (if (= parenthesis-level 0)
               (progn
                 (push
                  token
                  emacs-wisent-grammar-converter-parser--tokens-stack)
                 (setq
                  body-string
                  (emacs-wisent-grammar-converter-parser--if-body-inline namespace))
                 (setq continue nil))
             (setq
              condition-subject-string
              (emacs-wisent-grammar-converter-parser--token-value
               namespace
               token))))
          ('OPEN_PARENTHESIS
           (setq
            parenthesis-level
            (1+ parenthesis-level)))
          ('CLOSE_PARENTHESIS
           (setq
            parenthesis-level
            (1- parenthesis-level)))
          ('OPEN_SQUARE_BRACKET
           (push
            token
            emacs-wisent-grammar-converter-parser--tokens-stack)
           (setq
            body-string
            (emacs-wisent-grammar-converter-parser--if-body namespace))
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected if-operator token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))
    (format
     "(if %s %s)"
     condition-string
     body-string)))

(defun emacs-wisent-grammar-converter-parser--if-body (namespace)
  "Parse IF-body in NAMESPACE."
  (let ((return-string "")
        (continue t)
        (square-bracket-level 0))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('OPEN_SQUARE_BRACKET
           (setq
            square-bracket-level
            (1+ square-bracket-level)))
          ('CLOSE_SQUARE_BRACKET
           (setq
            square-bracket-level
            (1- square-bracket-level))
           (when (= square-bracket-level 0)
             (setq continue nil)))
          ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS 'DECLARATION))
          ('POINTER
           (unless (string= token-value "")
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter-parser--variable token-value namespace)))))
          ('FUNCTION
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--function token-value namespace))))
          ('IF
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--if namespace))))
          ('SYMBOL
           (setq
            return-string
            (concat
             return-string
             (format
              "(setq r '%s%s)"
              namespace
              token-value))))
          ('VARIABLE
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--variable token-value namespace))))
          ('PARAMETER
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--parameter token-value namespace))))
          ('RETURN
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--return namespace))))
          ('SEMICOLON)
          (_ (signal 'error (list (format "Unexpected if-body token %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--if-body-inline (namespace)
  "Parse IF-body-inline in NAMESPACE."
  (let ((return-string "")
        (continue t))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ('SEMICOLON
           (setq continue nil))
          ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS 'DECLARATION))
          ('POINTER
           (unless (string= token-value "")
             (setq
              return-string
              (concat
               return-string
               (emacs-wisent-grammar-converter-parser--variable token-value namespace)))))
          ('FUNCTION
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--function token-value namespace))))
          ('IF
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--if namespace))))
          ('SYMBOL
           (setq
            return-string
            (concat
             return-string
             (format
              "(setq r '%s%s)"
              namespace
              token-value))))
          ('VARIABLE
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--variable token-value namespace))))
          ('PARAMETER
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--parameter token-value namespace))))
          ('RETURN
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--return namespace))))
          ('SEMICOLON)
          (_ (signal 'error (list (format "Unexpected if-body-inline token %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--logical-infix (subject-string namespace operator)
  "Parse logical infix between SUBJECT with infix in NAMESPACE"
  (let ((continue t)
        (return-string "")
        (variable "")
        (parenthesis-level 0)
        (operator-string ""))
    (pcase operator
      ('EQUAL (setq operator-string "equal"))
      ('LOGICAL_OR (setq operator-string "or"))
      ('LOGICAL_AND (setq operator-string "and"))
      (_ (signal 'error (list (format "Unexpected logical infix operator %s" operator)))))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ((or 'PARAMETER 'VARIABLE 'RETURN 'INTEGER 'STRING 'SYMBOL)
           (setq
            subject2-string
            (emacs-wisent-grammar-converter-parser--token-value
             namespace
             token))
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected logical infix  token %s" token)))))))
    (format
     "(%s %s %s)"
     operator-string
     subject-string
     subject2-string)))

(defun emacs-wisent-grammar-converter-parser--logical-prefix (namespace operator)
  "Parse logical prefix expression OPERATOR in NAMESPACE."
  (let ((continue t)
        (return-string "")
        (variable "")
        (parenthesis-level 0))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token))
             (token-value (car (cdr token))))
        (pcase token-id
          ((or 'VARIABLE RETURN SYMBOL FUNCTION STRING INTEGER RETURN)
           (setq continue nil)
           (setq
            return-string
            (concat
             return-string
             (format
              "(not %s)"
              (emacs-wisent-grammar-converter-parser--token-value
               namespace
               token)))))
          ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS))
          (_ (signal 'error (list (format "Unexpected logical prefix token: %s" token)))))))
    return-string))

(provide 'emacs-wisent-grammar-converter-parser)
;;; emacs-wisent-grammar-converter-parser.el ends here
