;;; test.el --- Tests for Emacs Wisent Grammar Converter -*- lexical-binding:t -*-

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


;; Run from terminal with `make test'


;;; Code:

(require 'emacs-wisent-grammar-converter)
(require 'ert)

(message " ")
(message "Unit tests started")

;; White-space
(should (equal "random-statement" (emacs-wisent-grammar/reformat-logic-block "	random-statement;  	\n\n")))

;; Return a argument
(should (equal "$3" (emacs-wisent-grammar/reformat-logic-block "  $$ = $3;  	\n\n")))

;; Function calls
(should (equal "(zend_ast_create ZEND_AST_EMPTY $3)" (emacs-wisent-grammar/reformat-logic-block "  zend_ast_create(ZEND_AST_EMPTY, $3);  	\n\n")))

;; TODO Assignments

;; TODO Return function call

(message "Unit tests completed")

(provide 'emacs-wisent-grammar-converter-test)
;;; test.el ends here
