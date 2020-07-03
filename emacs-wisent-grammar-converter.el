;;; emacs-wisent-grammar-converter.el --- Emacs Wisent Grammar Converter  -*- lexical-binding:t -*-

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 9 Aug 2018
;; Modified: 3 Juk 2020
;; Version: 0.2.3
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

(defun emacs-wisent-grammar-convert--parse-buffer (buffer &optional header-string prefix terminal-replacements)
  "Convert buffer grammar, prepend HEADER-string if specified, use PREFIX if specified.  Return the conversion as a string."
  (switch-to-buffer buffer)

  ;; Remove unnecessary starting and ending stuff
  (goto-char (point-min))
  (let ((start (point))
        (level 'start)
        (continue t)
        (grammar "")
        (block "")
        (rule "")
        (rule-start 0)
        (rule-end 0)
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

        ('start
         (if (search-forward-regexp "\n+%%" nil t)
             (progn
               (message "Found start at '%s'" (point))
               (setq level 'root))
           (setq continue nil)
           (message "Found no start of grammar")))

        ('root

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
                       (setq rule-start (match-end 0))
                       (setq rule-token-count 0)
                       (message "Found block '%s' at '%s'" block (point))
                       (setq level 'block))
                   (message "Found end of rules at '%s', match-string '%s'" (point) (match-string 0))
                   (setq continue nil))))

           (progn
             (message "Failed to find block-start")
             (setq continue nil))))

        ('block

            ;; Can we find a | or { or ; character?
            (if (search-forward-regexp "\\(|\\|{\\|;\\|}\\|\'\\|\"\\|/\\*\\|[%a-zA-Z_]+\\)" nil t)
                (let ((match-subject (match-string 0)))
                  ;; (message "match-subject: '%s', match-string 0: '%s', match-string 1: '%s'" match-subject (match-string 0) (match-string 1))

                  (cond

                   ;; Is it a rule delimiter (| or ;)?
                   ((or (string= match-subject "|")
                        (string= match-subject  ";"))

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
                        (setq level 'root))))

                   ;; Is it a logic start delimiter?
                   ((string= match-subject "{")
                    (setq logic-start (1+ (match-beginning 0)))
                    (let ((continue t))
                      (while continue
                        (if (search-forward-regexp "\\(}\\|\'\\|\"\\|/\\*\\|// \\)" nil t)
                            (progn
                              (cond
                               ((string= (match-string 1) "}")
                                (setq continue nil))
                               ((string = (match-string 1) "\"")
                                (unless (search-forward-regexp "\"" nil t)
                                  (signal 'error (list "Found no ending double quote"))))
                               ((string = (match-string 1) "'")
                                (unless (search-forward-regexp "'" nil t)
                                  (signal 'error (list "Found no ending single quote"))))
                               ((string = (match-string 1) "/\\*")
                                (unless (search-forward-regexp "\\*/" nil t)
                                  (signal 'error (list "Found no ending doc comment"))))
                               ((string = (match-string 1) "// ")
                                (unless (search-forward-regexp "\n" nil t)
                                  (signal 'error (list "Found no ending comment"))))))
                          (signal 'error (list "Found no ending of logic block")))))

                    (setq logic-end (point))
                    (setq logic (buffer-substring logic-start (- logic-end 1)))

                    (message "Found logic contents: '%s'" logic)
                    (setq logic (emacs-wisent-grammar-converter--reformat-logic-block logic prefix))
                    (message "Reformatted logic into '%s'" logic)
                    (setq rule (concat rule " " logic)))

                   ;; Is it a single-quote?
                   ((string= match-subject "'")
                    (let ((quote-start (point))
                          (quote-end)
                          (quote))
                      (if (search-forward-regexp "\'" nil t)
                          (progn
                            (setq quote-end (point))
                            (setq quote (emacs-wisent-grammar-converter--string-trim (buffer-substring (- quote-start 1) quote-end)))

                            ;; Optionally replace terminal with user-specified non-terminal here
                            (when terminal-replacements
                              (if (gethash quote terminal-replacements)
                                  (setq quote (gethash quote terminal-replacements))
                                (message "Failed to find non-terminal \"%s\" in terminal-replacements table." quote)))

                            (when (> rule-token-count 0)
                              (setq rule (concat rule " ")))
                            (setq rule (concat rule quote))
                            (setq rule-token-count (+ rule-token-count 1)))
                        (progn
                          (message "Failed to find ending single-quote")
                          (setq continue nil)))))

                   ;; Is it a double-quote?
                   ((string= match-subject "\"")
                    (let ((quote-start (point))
                          (quote-end)
                          (quote))
                      (if (search-forward-regexp "\"" nil t)
                          (progn
                            
                            (setq quote-end (point))
                            (setq quote (emacs-wisent-grammar-converter--string-trim (buffer-substring (- quote-start 1) quote-end 1)))

                            ;; Optionally replace terminal with user-specified non-terminal here
                            (when terminal-replacements
                              (if (gethash quote terminal-replacements)
                                  (setq quote (gethash quote terminal-replacements))
                                (message "Failed to find non-terminal \"%s\" in terminal-replacements table." quote)))

                            (when (> rule-token-count 0)
                              (setq rule (concat rule " ")))
                            (setq rule (concat rule quote))
                            (setq rule-token-count (+ rule-token-count 1)))
                        (progn
                          (message "Failed to find ending double-quote")
                          (setq continue nil)))))

                   ;; Is it a logic end delimiter?
                   ((string= match-subject "}")
                    (message "Invalid grammar, breaking")
                    (setq continue nil))

                   ;; Is it a Cdoc comment block start
                   ((string= match-subject "/*")
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
                        (setq rule-token-count (+ rule-token-count 1))))))

              (progn
                (message "Failed to find rule delimiter")
                (setq continue nil))))

        (_ (setq continue nil))))

    ;; Clear entire buffer
    (delete-region (point-min) (point-max))

    (goto-char (point-max))

    ;; Prepend header if specified
    (when header-string
      (goto-char (point-min))
      (insert header-string)
      (goto-char (point-max))
      (insert "\n\n")
      (goto-char (point-max)))

    (insert ";; NOTE Generated grammar starts here\n\n%%\n\n%empty:\n    ()\n    ;\n\n")
    (insert grammar)
    (insert "\n\n%%")
    (insert "\n\n;; NOTE Generated grammar ends here")

    ;; Untabify and clean-up white-spaces
    (untabify (point-min) (point-max))
    (whitespace-cleanup)

    ;; Return current buffer as string
    (buffer-string)))

(defun emacs-wisent-grammar-converter--generate-grammar-from-filename (source destination &optional header-file prefix terminal-replacements)
  "Convert grammar in SOURCE to DESTINATION, prepend HEADER-FILE if specified, use PREFIX if specified.  Return the conversion as a string."
  (let* ((buffer (generate-new-buffer "*Converted Grammar*"))
         (header-string))
    (switch-to-buffer buffer)
    (insert-file-contents source)

    (when header-file
      (with-temp-buffer
        (insert-file-contents header-file)
        (setq header-string (buffer-string))))

    (let ((buffer-string
           (emacs-wisent-grammar-convert--parse-buffer buffer header-string prefix terminal-replacements)))
      (write-file destination)
      (kill-buffer)
      buffer-string)))

(provide 'emacs-wisent-grammar-converter)
;;; emacs-wisent-grammar-converter.el ends here
