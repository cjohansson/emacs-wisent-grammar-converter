;;; emacs-wisent-grammar-converter.el --- Emacs Wisent Grammar Converter

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

(require 'subr-x)

(defun emacs-wisent-grammar/reformat-logic-block (logic)
      "Reformat LOGIC from C to elisp."

      ;; Remove new-lines
      (setq logic (replace-regexp-in-string "\n" " " logic))

      ;; Replace more than one space with single space
      (setq logic (replace-regexp-in-string "\\(\\ \\|\t\\)\\(\\ \\|\t\\)+" " " logic))

      ;; FIXME
      (while (string-match "$$ = $\\([0-9]+\\);" logic)
        (setq logic (replace-match (format "$%s" (match-string 1 logic)) t t logic)))
      logic)

(defun emacs-wisent-grammar-converter-generate-grammar-from-filename(source destination &optional header)
  "Convert grammar in SOURCE to DESTINATION, prepend HEADER if specified.  Return the conversion as a string."
  (let* ((buffer (generate-new-buffer destination)))
    (switch-to-buffer buffer)
    (insert-file-contents source)

    ;; Remove unnecessary starting and ending stuff
    (let ((start (point))
          (whitespace "[\t\n ]")
          (grammar ""))

      (let ((level "root")
            (continue t)
            (grammar "")
            (block-start 0)
            (block-end 0)
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

        ;; TODO /* ... */
        ;; TODO NULL
        ;; TODO expr.. ;
        ;; TODO $$ =
        ;; TODO $$->... ;
        ;; TODO ... ? ... :

        ;; Iterate through entire buffer starting from start
        (goto-char start)
        (while continue
          (cond

           ((string= level "root")
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

           ((string= level "block")

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
                          (setq quote (string-trim (buffer-substring (- quote-start 1) quote-end)))
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
                          (setq quote (string-trim (buffer-substring (- quote-start 1) quote-end 1)))
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
                          (setq comment (string-trim (buffer-substring comment-start (- comment-end 2))))
                          (setq rule (concat rule ";; " (string-trim comment)))
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

           ((string= level "logic")

            ;; Can we find a { or } character?
            (if (search-forward-regexp "\\({\\|}\\)" nil t)

                (cond

                 ((string= (match-string 1) "}")
                  (setq logic-end (point))
                  (setq logic (string-trim (buffer-substring logic-start (- logic-end 1))))

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
                  (setq logic (string-trim (buffer-substring logic-start (- logic-end 1))))
                  (setq rule (concat rule " (" logic))
                  (push (list logic-start logic-end) parse-stack)
                  (setq logic-start (point)))

                 )

              (progn
                (message "Failed to find logic end")
                (setq continue nil))

              ))

           (t (setq continue nil))

           ))

        ;; Clear buffer
        (delete-region (point-min) (point-max))

        ;; Prepend header if specified
        (when (boundp header)
          (insert-file-contents header))

        (goto-char (point-max))

        (insert "\n\n;; NOTE Generated grammar starts here\n\n")
        (insert grammar)
        (insert "\n\n;; NOTE Generated grammar ends here")

        ;; Untabify and clean-up white-spaces
        (untabify (point-min) (point-max))
        (whitespace-cleanup)

        ;; Return current buffer as string
        (let ((return (buffer-string)))
          (kill-buffer buffer)
          return)

        ))))


(provide 'emacs-wisent-grammar-converter)
;;; emacs-wisent-grammar-converter.el ends here
