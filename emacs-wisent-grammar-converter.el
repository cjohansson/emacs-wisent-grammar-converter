;;; emacs-wisent-grammar-converter.el --- Emacs Wisent Grammar Converter  -*- lexical-binding:t -*-

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 9 Aug 2018
;; Modified: 5 Jun 2020
;; Version: 0.2.2
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

(require 'emacs-wisent-grammar-converter-lexer)
(require 'emacs-wisent-grammar-converter-parser)

(defun emacs-wisent-grammar-converter--reformat-logic-block (string &optional namespace)
  "Lex and then parse STRING into Emacs Lisp, use optional NAMESPACE if specified."
  (let ((tokens (emacs-wisent-grammar-converter-lexer--lex-c-string string)))
    (emacs-wisent-grammar-converter-parser--converted-lexer-tokens-to-lisp tokens namespace)))

(defun emacs-wisent-grammar-converter--string-trim (string)
  "Trim STRING from white-space."
  (emacs-wisent-grammar-converter--string-trim-left (emacs-wisent-grammar-converter--string-trim-right string)))

(defun emacs-wisent-grammar-converter--string-trim-left (string)
  "Trim STRING from white-space."
  (replace-regexp-in-string "^[\n\t\\ ]+" "" string))

(defun emacs-wisent-grammar-converter--string-trim-right (string)
  "Trim STRING from white-space."
  (replace-regexp-in-string "[\n\t\\ ]+$" "" string))

(defun emacs-wisent-grammar-converter--generate-grammar-from-filename (source destination &optional header prefix)
  "Convert grammar in SOURCE to DESTINATION, prepend HEADER if specified, use PREFIX if specified.  Return the conversion as a string."
  (let* ((buffer (generate-new-buffer destination)))
    (switch-to-buffer buffer)
    (insert-file-contents source)

    ;; Remove unnecessary starting and ending stuff
    (let ((start (point))
          (level "start")
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

          ("start"
           (if (search-forward-regexp "\n+%%" nil t)
               (progn
                 (message "Found start at '%s'" (point))
                 (setq level "root"))
             (setq continue nil)
             (message "Found no start of grammar")))

          ("root"

           ;; Can we find something like    abc_def:
           (if (search-forward-regexp "\n+\\(\\([a-z_]+\\)[\n\t ]*:\\|%%\n+\\)" nil t)

               (progn
                 (let ((matches-block (string-match-p "\n+[a-z_]+[\n\t ]*:" (match-string 0)))
                       (matches-end (string-match-p "\n+%%\n+" (match-string 0))))
                   (if matches-block
                       (progn
                         (setq grammar (concat grammar (format "\n%s\n    " (match-string 1))))
                         (setq block (match-string 1))
                         (setq rule "")
                         (setq rule-token-count 0)
                         (message "Found block '%s' at '%s'" block (point))
                         (setq level "block"))
                     (message "Found end of rules at '%s', match-string '%s'" (point) (match-string 0))
                     (setq continue nil))))

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
                     (message "Found another rule in block at '%s'" (point)))

                   ;; Is it the end of a block?
                   (when matches-end
                     (setq grammar (concat grammar "\n    ;\n"))
                     (setq rule-token-count 0)
                     (message "Ended block at '%s'" (point))
                     (setq level "root"))))

                ;; Is it a logic start delimiter?
                ((string= (match-string 1) "{")
                 (if (and (> rule-token-count 0) (not last-was-block-comment))
                     (setq rule (concat rule " "))
                   (setq rule (concat rule "\n    ")))
                 (setq rule (concat rule "(progn "))
                 (setq last-was-block-comment nil)
                 (setq logic-start (point))
                 (message "Found logic start at '%s'" (point))
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
           (if (search-forward-regexp "\\({\\|}\\|'\\|\"\\)" nil t)
               (cond

                ((string= (match-string 1) "'")
                 (message "Found starting single quote at '%s'" (point))
                 (if (search-forward-regexp "'" nil t)
                     (message "Found ending single quote at '%s'" (point))
                   (signal 'error (list (format "Found no ending of single quote around %s" (point))))))

                ((string= (match-string 1) "\"")
                 (message "Found starting double quote at '%s'" (point))
                 (if (search-forward-regexp "\"" nil t)
                     (message "Found ending double quote at '%s'" (point))
                   (signal 'error (list (format "Found no ending of double quote around %s" (point))))))

                ((string= (match-string 1) "}")
                 (setq logic-end (point))
                 (setq logic (emacs-wisent-grammar-converter--string-trim (buffer-substring logic-start (- logic-end 1))))

                 (setq logic (emacs-wisent-grammar-converter--reformat-logic-block logic prefix))
                 
                 (setq rule (concat rule logic ")"))

                 (let ((previous (pop parse-stack)))
                   ;; Do we have a parse-stack?
                   (if previous
                       (setq logic-start (point))
                     (progn
                       (message "Ending logic at '%s'" (point))
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
