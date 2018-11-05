;;; emacs-wisent-grammar-converter.el --- Emacs Wisent Grammar Converter  -*- lexical-binding:t -*-

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 9 Aug 2018
;; Modified: .
;; Version: 0.1
;; Keywords: tools, convenience
;; URL: -

;; Copyright (C) 2018 Christian Johansson

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

;;; Code:


(defun emacs-wisent-grammar/reformat-logic-block (logic)
  "Reformat LOGIC from C to elisp."

  ;; Remove line-feeds and carriage returns
  (setq logic (replace-regexp-in-string "\\(\n\\|\r\\)+" " " logic))

  ;; Replace more than one space with single space
  (setq logic (replace-regexp-in-string "\\(\\ \\|\t\\)\\(\\ \\|\t\\)+" " " logic))

  ;; Transform statements like    zend(a, b, c)    into    (zend a b c)
  (while (string-match "\\([a-zA-Z_]+\\)(\\([^)]*\\))" logic)
    (if (string= (match-string 2 logic) "")
        (setq logic (replace-match (format "(%s)" (match-string 1 logic)) t t logic))
      (setq logic (replace-match (format "(%s %s)" (match-string 1 logic) (match-string 2 logic)) t t logic))))

  ;; Transform statements like    $$ = $1;    into    $1
  (while (string-match "$$ = $\\([0-9]+\\);" logic)
    (setq logic (replace-match (format "$%s" (match-string 1 logic)) t t logic)))

  ;; Transform statements like    $$ = (zend    into    (zend
  (while (string-match "$$ = \\(([a-zA-Z_]\\)" logic)
    (setq logic (replace-match (format "%s" (match-string 1 logic)) t t logic)))

  ;; Transform statements like    $$->attr = ZEND_NAME_NOT_FQ;    into    (put $$ 'attr 'ZEND_NAME_NOT_FQ)
  (while (string-match "\\([\$a-zA-Z0-9]+\\)->\\([a-zA-Z0-9]+\\)[\\ ]*=[\\ ]*\\([^;]+\\);" logic)
    (setq logic (replace-match (format "(put %s '%s '%s)" (match-string 1 logic) (match-string 2 logic) (match-string 3 logic)) t t logic)))

  ;; Replace comma with space
  (setq logic (replace-regexp-in-string ",\\ +" " " logic))

  ;; Replace semi-colon with nothing
  (setq logic (replace-regexp-in-string ";" "" logic))

  ;; Transform doc block comments    /* blaha */ to \n;; blaha\n
  (while (string-match "/\\*\\(.+\\)\\*/" logic)
    (setq logic (replace-match (format "\n;;%s\n" (match-string 1 logic)) t t logic)))

  ;; Replace NULL with nil
  (setq logic (replace-regexp-in-string "NULL" "nil" logic t t))

  (emacs-wisent-grammar-converter/string-trim logic))

(defun emacs-wisent-grammar-converter/string-trim (string)
  "Trim STRING from white-space."
  (emacs-wisent-grammar-converter/string-trim-left (emacs-wisent-grammar-converter/string-trim-right string)))

(defun emacs-wisent-grammar-converter/string-trim-left (string)
  "Trim STRING from white-space."
  (replace-regexp-in-string "^[\n\t\\ ]+" "" string))

(defun emacs-wisent-grammar-converter/string-trim-right (string)
  "Trim STRING from white-space."
  (replace-regexp-in-string "[\n\t\\ ]+$" "" string))

(defun emacs-wisent-grammar-converter/generate-grammar-from-filename (source destination &optional header)
  "Convert grammar in SOURCE to DESTINATION, prepend HEADER if specified.  Return the conversion as a string."
  (let* ((buffer (generate-new-buffer destination)))
    (switch-to-buffer buffer)
    (insert-file-contents source)

    ;; Remove unnecessary starting and ending stuff
    (let ((start (point))
          (level "root")
          (continue t)
          (grammar "")
          (block "")
          (rule-start)
          (rule-end)
          (rule "")
          (logic-start 0)
          (logic-end 0)
          (logic)
          (parse-stack '())
          (last-was-block-comment nil)
          (last-was-quote nil)
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
                (setq rule-start (point))
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

                  ;; Collect rule here
                  (setq rule-end (point))

                  ;; Did last action add a new-line?
                  (setq grammar (concat grammar rule))
                  (setq rule "")

                  ;; Is it the start of a new rule?
                  (when matches-delimiter
                    (setq rule-start (point))
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
                (setq rule-end (point))
                (if (> rule-token-count 0)
                    (setq rule (concat rule " "))
                  (setq rule (concat rule "\n    ")))
                (setq rule (concat rule "("))
                (setq last-was-block-comment nil)
                (setq last-was-quote nil)
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
                        (setq rule-start quote-end)
                        (setq quote (emacs-wisent-grammar-converter/string-trim (buffer-substring (- quote-start 1) quote-end)))
                        (when (> rule-token-count 0)
                          (setq rule (concat rule " ")))
                        (setq rule (concat rule quote))
                        (setq last-was-quote t)
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
                        (setq rule-start quote-end)
                        (setq quote (emacs-wisent-grammar-converter/string-trim (buffer-substring (- quote-start 1) quote-end 1)))
                        (when (> rule-token-count 0)
                          (setq rule (concat rule " ")))
                        (setq rule (concat rule quote))
                        (setq last-was-quote t)
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
                        (setq comment (emacs-wisent-grammar-converter/string-trim (buffer-substring comment-start (- comment-end 2))))
                        (setq rule (concat rule " ;; " (emacs-wisent-grammar-converter/string-trim comment) "\n   "))
                        (setq rule-start comment-end)
                        (setq last-was-block-comment t))
                    (progn
                      (message "Failed to find ending doc comment block")
                      (setq continue nil)))))

               (t (progn
                    (when (> rule-token-count 0)
                      (setq rule (concat rule " ")))
                    (setq rule (concat rule (match-string 1)))
                    (setq last-was-quote nil)
                    (setq last-was-block-comment nil)
                    (setq rule-token-count (+ rule-token-count 1))))
               
               )
            

            (progn
              (message "Failed to find rule delimiter")
              (setq continue nil))))

         ("logic"

          ;; Can we find a { or } character?
          (if (search-forward-regexp "\\({\\|}\\)" nil t)

              (cond

               ((string= (match-string 1) "}")
                (setq logic-end (point))
                (setq logic (emacs-wisent-grammar-converter/string-trim (buffer-substring logic-start (- logic-end 1))))

                (setq logic (emacs-wisent-grammar/reformat-logic-block logic))
                
                (setq rule (concat rule logic ")"))

                (let ((previous (pop parse-stack)))
                  ;; Do we have a parse-stack?
                  (if previous
                      (setq logic-start (point))
                    (progn
                      (setq rule-start (point))
                      (setq level "block")))))

               ;; Is it the start of nested logic?
               ((string= (match-string 1) "{")
                (setq logic-end (point))
                (setq logic (emacs-wisent-grammar-converter/string-trim (buffer-substring logic-start (- logic-end 1))))
                (setq rule (concat rule " (" logic))
                (push (list logic-start logic-end) parse-stack)
                (setq logic-start (point)))

               )

            (progn
              (message "Failed to find logic end")
              (setq continue nil))

            ))

         (_ (setq continue nil))

         ))

      ;; Clear buffer
      (delete-region (point-min) (point-max))

      ;; Prepend header if specified
      (when (and (boundp 'header)
                 header)
        (insert "\n\n")
        (insert-file-contents header))

      (goto-char (point-max))

      (insert ";; NOTE Generated grammar starts here\n\n")
      (insert grammar)
      (insert "\n\n;; NOTE Generated grammar ends here")

      ;; Untabify and clean-up white-spaces
      (untabify (point-min) (point-max))
      (whitespace-cleanup)

      ;; Return current buffer as string
      (buffer-string)

      )))


(provide 'emacs-wisent-grammar-converter)
;;; emacs-wisent-grammar-converter.el ends here
