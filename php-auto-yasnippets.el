;;; php-auto-yasnippets.el --- Creates snippets for PHP functions
;;
;; Copyright 2013 Eric James Michael Ritz
;;
;; Author: Eric James Michael Ritz
;; URL: https://github.com/ejmr/php-auto-yasnippets
;; Version: 0.1
;;
;;
;;
;;; License:
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;
;;
;;; Commentary:
;;
;; This package provides a function that automatically creates
;; snippets for PHP standard library functions, for use with the
;; YASnippets package, available at:
;;
;;     https://github.com/capitaomorte/yasnippet
;;
;; This package also requires php-mode, available at:
;;
;;     https://github.com/ejmr/php-mode
;;
;; To use this package you need to bind the utility function
;; `yas/create-php-snippet' to a key of your choice.  Since this
;; package requires php-mode, and since it is most useful when writing
;; PHP code, you may want to use a key-binding that only works when
;; using php-mode.  For example:
;;
;;     (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
;;
;; Now if you type the name of a PHP function and press `C-c C-y' it
;; will expand into a snippet containing all of the parameters, their
;; names, any default values, et cetera.

;;; Code:
;;
;; Every function beginning with 'payas/' is internal to this package
;; and no other script should rely on them.  They may change in future
;; versions without warning.

(require 'php-mode)
(require 'yasnippet)

(defconst php-auto-yasnippet-version "0.1"
  "The version number for the php-auto-yasnippet package.")

(defvar php-auto-yasnippet-php-program
  (expand-file-name "~/.emacs.d/php-auto-yasnippets/Create-PHP-YASnippet.php")
  "The path to the program `Create-PHP-YASnippet.php'.")

(defun payas/create-template-string (input)
  "Takes a string of INPUT and returns a string intended for the
function `yas--parse-template'."
  (with-temp-buffer
    (call-process php-executable nil (current-buffer) nil
                  php-auto-yasnippet-php-program input)
    (buffer-string)))

(provide 'php-auto-yasnippets)

;;; php-auto-yasnippets.el ends here
