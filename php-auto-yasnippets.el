;;; php-auto-yasnippets.el --- Creates snippets for PHP functions
;;
;; Copyright 2013 Eric James Michael Ritz
;;
;; Author: Eric James Michael Ritz
;; URL: https://github.com/ejmr/php-auto-yasnippets
;; Version: 0.6.0
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

(require 'thingatpt)
(require 'php-mode)
(require 'yasnippet)


;;; This section defines constants the package uses as well as any
;;; global variables which the user may wish to change.

(defconst php-auto-yasnippet-version "0.6.0"
  "The version number for the php-auto-yasnippet package.")

(defvar php-auto-yasnippet-php-program
  (expand-file-name "~/.emacs.d/php-auto-yasnippets/Create-PHP-YASnippet.php")
  "The path to the program `Create-PHP-YASnippet.php'.")


;;; Below are all of the internal functions.  No code outside of this
;;; file should rely on any of these functions.

(defun payas/create-template (input)
  "Creates a snippet for INPUT string in the current buffer.

INPUT should be either the name of a PHP function, or the name of
a PHP method followed by the name of the class implementing it,
separated by a space.  For example, \"json_decode\" for a
function or \"push SplQueue\" for a method and class.

Because this function sends output to the current buffer always
wrap `with-temp-buffer' around calls to it, because the output
this function creates should go directly to the function
`yas--parse-template', and it expects the template definition to
be in the current buffer.

This function runs `php-auto-yasnippet-php-program' to generate
the snippet.  The return value is the exit code of that program."
  (save-match-data
    (let* ((input-chunks (split-string input))
           (function-or-method-name (first input-chunks))
           (class-name (or (second input-chunks) "")))
      (call-process php-executable nil (current-buffer) nil
                    php-auto-yasnippet-php-program
                    function-or-method-name
                    class-name))))

(defun payas/report-error (error-code &optional user-input)
  "Reports an error based on the given ERROR-CODE.

The ERROR-CODE is an integer representing the exit status of the
program `php-auto-yasnippet-php-program'.  That program exits
with zero for success and non-zero for any errors.  This function
shows an error message based on the possible exit codes that
program may return.  See the commentary in that program for a
description of possible ERROR-CODE values and their meaning.

The optional value USER-INPUT, if provided, must be the string
given to `payas/create-template' that caused the PHP program to
return ERROR-CODE.

If there is nothing to do for the ERROR-CODE then the function
returns nil.  However, the function may not return at all if it
signals an error."
  (cond ((= error-code 1)
         (error "Cannot run the program %s" php-auto-yasnippet-php-program))
        ((= error-code 2)
         (error "No function name given to %s" php-auto-yasnippet-php-program))
        ;; We get this error code when the PHP program exits with the
        ;; value ERROR_UNKNOWN_FUNCTION.  That means the user tried to
        ;; create a snippet for a function the program does not
        ;; recognize as a standard PHP function.  So arguably we
        ;; should report this via user-error since we could say the
        ;; fault is on the user.  However, if we do that then we are
        ;; making the assumption that php-auto-yasnippets made no
        ;; mistake in selecting the function name from the buffer.  It
        ;; is possible that the function is not recognized because we
        ;; screwed up and did not send the complete function name.  So
        ;; until we are completely confident about that aspect of the
        ;; code we will treat this as an error on our part and not as
        ;; a mistake by the user.
        ((= error-code 3)
         (error "%s is not a recognized PHP function" user-input))
        ;; If we get this error code, ERROR_UNKNOWN_METHOD, then we
        ;; can reformat user-input to use PHP's notation for a
        ;; better-looking error message.
        ((= error-code 4)
         (let* ((input-chunks (split-string user-input))
                (method-name (concat (second input-chunks)
                                     "::"
                                     (first input-chunks))))
           (error "%s is not a recognized PHP method" method-name)))
        (t nil)))

(defun payas/define-template (input)
  "Create a snippet for INPUT.

The INPUT must be the name of a PHP standard library function.
This function creates a snippet for that function and associates
it with `php-mode'."
  (unless (yas--get-template-by-uuid 'php-mode input)
    (with-temp-buffer
      (let ((exit-code (payas/create-template input)))
        (if (/= exit-code 0)
            (payas/report-error exit-code input))
        (yas-define-snippets
         'php-mode
         (list (yas--parse-template)))))))


;;; This section contains the public API.

(defun yas/create-php-snippet ()
  "Creates and expands a snippet for the PHP function at point."
  (interactive)
  (let ((function (thing-at-point 'symbol)))
    (payas/define-template function)
    (yas-expand)))

(provide 'php-auto-yasnippets)

;;; php-auto-yasnippets.el ends here
