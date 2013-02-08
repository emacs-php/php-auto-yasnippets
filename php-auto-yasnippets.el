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
;; To use php-auto-yasnippets you need to do three things.  First,
;; place the package in your load-path (`C-h v load-path' for help)
;; and load it from your Emacs configuration file by adding:
;;
;;     (require 'php-auto-yasnippets)
;;
;; Second, make sure the variable `php-auto-yasnippet-php-program'
;; points to the program `Create-PHP-YASnippet.php'.  That PHP program
;; should have come with this package; if you do not have it then you
;; can get it from the project GitHub URL at the top of this file.
;; You can use `setq' in your configuration file to set the variable
;; to the proper path, e.g:
;;
;;     (require 'php-auto-yasnippets)
;;     (setq php-auto-yasnippet-php-program "~/path/to/Create-PHP-YASnippet.php")
;;
;; Finally, bind the function `yas/create-php-snippet' to a key of
;; your choice.  Since this package requires php-mode, and since it is
;; most useful when writing PHP code, you may want to use a
;; key-binding that only works when using php-mode.  For example:
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

(defun payas/create-template (input)
  "Creates a snippet for INPUT string in the current buffer.

Because this function sends output to the current buffer always
wrap `with-temp-buffer' around calls to it, because the output
this function creates should go directly to the function
`yas--parse-template', and it expects the template definition to
be in the current buffer."
  (call-process php-executable nil (current-buffer) nil
                php-auto-yasnippet-php-program input))

(defun payas/define-template (input)
  "Create a snippet for INPUT.

The INPUT must be the name of a PHP standard library function.
This function creates a snippet for that function and associates
it with `php-mode'."
  (with-temp-buffer
    (payas/create-template input)
    (yas-define-snippets
     'php-mode
     (list (yas--parse-template)))))

(provide 'php-auto-yasnippets)

;;; php-auto-yasnippets.el ends here
