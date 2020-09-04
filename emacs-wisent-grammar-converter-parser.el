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

(defun emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp (tokens &optional namespace macro-list)
  "Convert Bison grammar TOKENS into emacs-lisp using parser for each statement.  Use optional NAMESPACE and MACRO-LIST."
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
               (emacs-wisent-grammar-converter-parser--variable
                token-value
                namespace
                macro-list)))))
          ('FUNCTION
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--function
              token-value
              namespace
              macro-list))))
          ('IF
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--if
              namespace
              macro-list))))
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
             (emacs-wisent-grammar-converter-parser--variable
              token-value
              namespace
              macro-list))))
          ('PARAMETER
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--parameter
              token-value
              namespace
              macro-list))))
          ('RETURN
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--return
              namespace
              macro-list))))
          ('SEMICOLON)
          (_ (signal 'error (list (format "Unexpected root token %s" token)))))))

    (format "%s r)" return-string)))

(defun emacs-wisent-grammar-converter-parser--variable (name namespace macro-list)
  "Parse variable NAME in NAMESPACE with MACRO-LIST."
  (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
         (token-id (car token)))
    (pcase token-id
      ('ASSIGNMENT
       (let ((formatted-name (concat namespace name)))
         (format
          "(setq %s %s)"
          formatted-name
          (emacs-wisent-grammar-converter-parser--token-value
           namespace
           nil
           macro-list))))
      ('SEMICOLON
       ;; A declaration has no equivalent in Emacs-Lisp
       "")
      ('MEMBER_OPERATOR
       (format
        "(semantic-tag-put-attribute %s%s '%s)"
        namespace
        name
        (emacs-wisent-grammar-converter-parser--member-operator
         name
         namespace
         macro-list)))
      (_ (signal 'error (list (format "Unexpected variable token %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))

(defun emacs-wisent-grammar-converter-parser--parameter (name namespace macro-list)
  "Parse parameter NAME in NAMESPACE and MACRO-LIST."
  (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
         (token-id (car token)))
    (pcase token-id
      ('ASSIGNMENT
       (format
        "(setq %s %s)"
        name
        (emacs-wisent-grammar-converter-parser--token-value
         namespace
         nil
         macro-list)))
      ('MEMBER_OPERATOR
       (format
        "(semantic-tag-put-attribute %s %s)"
        name
        (emacs-wisent-grammar-converter-parser--member-operator
         name
         namespace
         macro-list)))
      (_ (signal 'error (list (format "Unexpected parameter token %s" token)))))))

(defun emacs-wisent-grammar-converter-parser--return (namespace macro-list)
  "Parse return in NAMESPACE and MACRO-LIST."
  (let ((return-string "")
        (continue t))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token)))
        (pcase token-id
          ('ASSIGNMENT
           (setq
            return-string
            (format
             "(setq r %s)"
             (emacs-wisent-grammar-converter-parser--token-value
              namespace
              nil
              macro-list)))
           (setq continue nil))
          ('BITWISE_OR_ASSIGNMENT
           (setq
            return-string
            (format
             "(setq r (logior r %s))"
             (emacs-wisent-grammar-converter-parser--token-value
              namespace
              nil
              macro-list)))
           (setq continue nil))
          ('BITWISE_AND_ASSIGNMENT
           (setq
            return-string
            (format
             "(setq r (logand r %s))"
             (emacs-wisent-grammar-converter-parser--token-value
              namespace
              nil
              macro-list)))
           (setq continue nil))
          ((or 'OPEN_PARENTHESIS 'CLOSE_PARENTHESIS))
          ('MEMBER_OPERATOR
           (setq
            return-string
            (format
             "(semantic-tag-put-attribute r %s)"
             (emacs-wisent-grammar-converter-parser--member-operator
              "r"
              namespace
              macro-list)))
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected return token %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--function (name namespace macro-list)
  "Parse function NAME and NAMESPACE and MACRO-LIST."
  ;; Skip first OPEN_PARENTHESIS token
  (pop emacs-wisent-grammar-converter-parser--tokens-stack)

  (let ((argument-string nil)
        (continue t)
        (return-string nil)
        (closed nil))
    (while (and continue
                emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token)))
        (pcase token-id
          ('ASSIGNMENT
           (let ((function-name (downcase (concat namespace name))))
             (when (and
                    macro-list
                    (gethash name macro-list))
               (setq function-name (upcase name)))
             (unless closed
               (signal 'err (list (format "Unexpected function assignment %s" token))))
             (setq continue nil)
             (if argument-string
                 (setq
                  return-string
                  (format
                   "(%s '%s %s)"
                   function-name
                   argument-string
                   (emacs-wisent-grammar-converter-parser--token-value
                    namespace
                    nil
                    macro-list)))
               (setq
                return-string
                (format
                 "(%s %s)"
                 function-name
                 (emacs-wisent-grammar-converter-parser--token-value
                  namespace
                  nil
                  macro-list))))))
          ('BITWISE_OR_ASSIGNMENT
           (let ((function-name (downcase (concat namespace name))))
             (when (and
                    macro-list
                    (gethash name macro-list))
               (setq function-name (upcase name)))
             (unless closed
               (signal 'err (list (format "Unexpected function assignment %s" token))))
             (setq continue nil)
             (if argument-string
                 (setq
                  return-string
                  (format
                   "(%s '%s (logior (%s '%s) %s))"
                   function-name
                   argument-string
                   function-name
                   argument-string
                   (emacs-wisent-grammar-converter-parser--token-value
                    namespace
                    nil
                    macro-list)))
               (setq
                return-string
                (format
                 "(%s (logior (%s) %s))"
                 function-name
                 function-name
                 (emacs-wisent-grammar-converter-parser--token-value
                  namespace
                  nil
                  macro-list))))))
          ('BITWISE_AND_ASSIGNMENT
           (let ((function-name (downcase (concat namespace name))))
             (when (and
                    macro-list
                    (gethash name macro-list))
               (setq function-name (upcase name)))
             (unless closed
               (signal 'err (list (format "Unexpected function assignment %s" token))))
             (setq continue nil)
             (if argument-string
                 (setq
                  return-string
                  (format
                   "(%s '%s (logand (%s '%s) %s))"
                   function-name
                   argument-string
                   function-name
                   argument-string
                   (emacs-wisent-grammar-converter-parser--token-value
                    namespace
                    nil
                    macro-list)))
               (setq
                return-string
                (format
                 "(%s (logand (%s) %s))"
                 function-name
                 function-name
                 (emacs-wisent-grammar-converter-parser--token-value
                  namespace
                  nil
                  macro-list))))))
          ('CLOSE_PARENTHESIS
           (let ((function-name (downcase (concat namespace name))))
             (when (and
                    macro-list
                    (gethash name macro-list))
               (setq function-name (upcase name)))
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
                     function-name
                     argument-string))
                 (setq
                  return-string
                  (format
                   "(%s)"
                   function-name)))
               (setq closed t))))
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
              (emacs-wisent-grammar-converter-parser--function-arguments
               namespace
               macro-list)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--function-arguments (namespace macro-list)
  "Parse function arguments starting at TOKEN with NAMESPACE and MACRO-LIST."
  (let ((return-string "")
        (return-count 0)
        (continue t)
        (bracket-level 1))
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
           (let ((parsed-value
                  (emacs-wisent-grammar-converter-parser--token-value
                   namespace
                   token
                   macro-list)))
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

(defun emacs-wisent-grammar-converter-parser--infix-token-value (namespace first-token-value macro-list)
  "Return infix value of first-token-value in NAMESPACE, FIRST-TOKEN-VALUE and MACRO-LIST."
  (let ((return-string "")
        (continue t)
        (in-subtraction nil)
        (in-addition nil)
        (in-bitwise-or nil)
        (in-bitwise-and nil))
    (while (and continue
                emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token)))
        (pcase token-id
          ('ADDITION
           (when in-subtraction
             (signal 'error (list (format "Cannot combine subctraction with addition in token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
           (unless in-subtraction
             (setq in-subtraction t)
             (setq
              return-string
              (concat
               "(- "
               first-token-value))))
          ('SUBTRACTION
           (when in-subtraction
             (signal 'error (list (format "Cannot combine subctraction with addition in token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))
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
           (let ((parsed-token-value
                  (emacs-wisent-grammar-converter-parser--token-value
                   namespace
                   token
                   macro-list)))
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

(defun emacs-wisent-grammar-converter-parser--token-value (namespace &optional token macro-list)
  "Return NAMESAPCE with TOKEN and MACRO-LIST."
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
          (emacs-wisent-grammar-converter-parser--function
           token-value
           namespace
           macro-list)))
        (_ (signal 'error (list (format "Unexpected token value token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack))))))

    ;; Check next token here
    (when emacs-wisent-grammar-converter-parser--tokens-stack
      (let* ((next-token (car emacs-wisent-grammar-converter-parser--tokens-stack))
             (next-token-id (car next-token)))
        (pcase next-token-id
          ((or 'BITWISE_OR 'BITWISE_AND 'ADDITION 'MULTIPLICATION 'SUBTRACTION 'DIVISION)
           (let ((infix-value
                  (emacs-wisent-grammar-converter-parser--infix-token-value
                   namespace
                   return-string
                   macro-list)))
             ;; (message "Infix value of token '%s' is '%s'" token infix-value)
             (setq
              return-string
              infix-value)))
          ('QUESTION_MARK
           (pop emacs-wisent-grammar-converter-parser--tokens-stack)
           (let ((ternary-true
                  (emacs-wisent-grammar-converter-parser--token-value
                   namespace
                   nil
                   macro-list))
                 (ternary-false ""))

             ;; Pop the :
             (pop emacs-wisent-grammar-converter-parser--tokens-stack)
             (setq ternary-false
                   (emacs-wisent-grammar-converter-parser--token-value
                    namespace
                    nil
                    macro-list))
             (setq
              return-string
              (format
               "(if %s %s %s)"
               return-string
               ternary-true
               ternary-false)))))))
    return-string))

;; This function supports stuff like ->attr = abc; ->attr) ->attr, ->attr;
(defun emacs-wisent-grammar-converter-parser--member-operator (parent namespace macro-list)
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
             (emacs-wisent-grammar-converter-parser--token-value
              namespace
              nil
              macro-list)
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
             (emacs-wisent-grammar-converter-parser--token-value
              namespace
              nil
              macro-list)
             ")"))
           (setq continue nil))
          ('ASSIGNMENT
           (setq
            return-string
            (concat
             return-string
             " "
             (emacs-wisent-grammar-converter-parser--token-value
              namespace
              nil
              macro-list)))
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

(defun emacs-wisent-grammar-converter-parser--if (namespace macro-list)
  "Parser for IF statments in NAMESPACE."
  (let ((continue t)
        (condition-string "")
        (condition-subject-string "")
        (body-string "")
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
              macro-list))))
          ((or 'LOGICAL_OR 'LOGICAL_AND 'EQUAL 'LOGICAL_NOT_EQUAL 'LOGICAL_GREATER 'LOGICAL_LESSER 'LOGICAL_GREATER_OR_EQUAL 'LOGICAL_LESSER_OR_EQUAL)
           (setq
            condition-string
            (concat
             condition-string
             (emacs-wisent-grammar-converter-parser--logical-infix
              condition-subject-string
              namespace
              token-id
              macro-list))))
          ((or 'PARAMETER 'RETURN)
           (if (= parenthesis-level 0)
               (progn
                 (push
                  token
                  emacs-wisent-grammar-converter-parser--tokens-stack)
                 (setq
                  body-string
                  (emacs-wisent-grammar-converter-parser--if-body-inline
                   namespace
                   macro-list))
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
                   token
                   macro-list))))))
          ((or 'VARIABLE 'SYMBOL 'FUNCTION 'STRING 'INTEGER)
           (if (= parenthesis-level 0)
               (progn
                 (push
                  token
                  emacs-wisent-grammar-converter-parser--tokens-stack)
                 (setq
                  body-string
                  (emacs-wisent-grammar-converter-parser--if-body-inline
                   namespace
                   macro-list))
                 (setq continue nil))
             (setq
              condition-subject-string
              (emacs-wisent-grammar-converter-parser--token-value
               namespace
               token
               macro-list))))
          ('OPEN_PARENTHESIS
           (setq
            parenthesis-level
            (1+ parenthesis-level)))
          ('CLOSE_PARENTHESIS
           (setq
            parenthesis-level
            (1- parenthesis-level)))
          ('OPEN_CURLY_BRACKET
           (push
            token
            emacs-wisent-grammar-converter-parser--tokens-stack)
           (setq
            body-string
            (emacs-wisent-grammar-converter-parser--if-body
             namespace
             macro-list))
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected if-operator token: %s, remaining tokens: %s" token emacs-wisent-grammar-converter-parser--tokens-stack)))))))
    (format
     "(if %s %s)"
     condition-string
     body-string)))

(defun emacs-wisent-grammar-converter-parser--if-body (namespace macro-list)
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
          ('OPEN_CURLY_BRACKET
           (setq
            square-bracket-level
            (1+ square-bracket-level)))
          ('CLOSE_CURLY_BRACKET
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
               (emacs-wisent-grammar-converter-parser--variable
                token-value
                namespace
                macro-list)))))
          ('FUNCTION
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--function
              token-value
              namespace
              macro-list))))
          ('IF
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--if
              namespace
              macro-list))))
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
             (emacs-wisent-grammar-converter-parser--variable
              token-value
              namespace
              macro-list))))
          ('PARAMETER
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--parameter
              token-value
              namespace
              macro-list))))
          ('RETURN
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--return
              namespace
              macro-list))))
          ('SEMICOLON)
          (_ (signal 'error (list (format "Unexpected if-body token %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--if-body-inline (namespace macro-list)
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
               (emacs-wisent-grammar-converter-parser--variable
                token-value
                namespace
                macro-list)))))
          ('FUNCTION
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--function
              token-value
              namespace
              macro-list))))
          ('IF
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--if
              namespace
              macro-list))))
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
             (emacs-wisent-grammar-converter-parser--variable
              token-value
              namespace
              macro-list))))
          ('PARAMETER
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--parameter
              token-value
              namespace
              macro-list))))
          ('RETURN
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--return
              namespace
              macro-list))))
          ('SEMICOLON)
          (_ (signal 'error (list (format "Unexpected if-body-inline token %s" token)))))))
    return-string))

(defun emacs-wisent-grammar-converter-parser--logical-infix (subject-string namespace operator macro-list)
  "Parse logical infix between SUBJECT with infix in NAMESPACE"
  (let ((subject2-string)
        (continue t)
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
             (token-id (car token)))
        (pcase token-id
          ((or 'PARAMETER 'VARIABLE 'RETURN 'INTEGER 'STRING 'SYMBOL)
           (setq
            subject2-string
            (emacs-wisent-grammar-converter-parser--token-value
             namespace
             token
             macro-list))
           (setq continue nil))
          (_ (signal 'error (list (format "Unexpected logical infix  token %s" token)))))))
    (format
     "(%s %s %s)"
     operator-string
     subject-string
     subject2-string)))

(defun emacs-wisent-grammar-converter-parser--logical-prefix (namespace macro-list)
  "Parse logical prefix expression OPERATOR in NAMESPACE."
  (let ((continue t)
        (return-string "")
        (return-count 0)
        (parenthesis-level 0)
        (previous-token nil))
    (while (and
            continue
            emacs-wisent-grammar-converter-parser--tokens-stack)
      (let* ((token (pop emacs-wisent-grammar-converter-parser--tokens-stack))
             (token-id (car token)))
        (pcase token-id
          ('OPEN_PARENTHESIS
           (setq parenthesis-level (1+ parenthesis-level)))
          ('CLOSE_PARENTHESIS
           (setq parenthesis-level (1- parenthesis-level))
           (when (= parenthesis-level 0)
             (setq continue nil)))
          ((or 'BITWISE_OR 'BITWISE_AND 'ADDITION 'MULTIPLICATION 'SUBTRACTION 'DIVISION)
           (push token emacs-wisent-grammar-converter-parser--tokens-stack)
           (let ((infix-value
                  (emacs-wisent-grammar-converter-parser--infix-token-value
                   namespace
                   previous-token
                   macro-list)))
             (message "Infix value of token '%s' is '%s'" token infix-value)
             (when (= parenthesis-level 0)
               (setq continue nil))
             (unless (= return-count 0)
               (setq
                return-string
                (concat
                 return-string
                 " ")))
             (setq return-count (1+ return-count))
             (setq
              return-string
              (concat
               return-string
               infix-value))))
          ((or 'VARIABLE 'RETURN 'SYMBOL 'FUNCTION 'STRING 'INTEGER)
           (when (= parenthesis-level 0)
             (setq continue nil))
           (unless (= return-count 0)
             (setq
              return-string
              (concat
               return-string
               " ")))
           (setq return-count (1+ return-count))
           (setq
            return-string
            (concat
             return-string
             (emacs-wisent-grammar-converter-parser--token-value
              namespace
              token
              macro-list))))
          (_ (signal 'error (list (format "Unexpected logical prefix token: %s" token)))))
        (setq previous-token token)))
    (format "(not %s)" return-string)))

(provide 'emacs-wisent-grammar-converter-parser)
;;; emacs-wisent-grammar-converter-parser.el ends here
