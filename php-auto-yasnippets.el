;;; php-auto-yasnippets.el --- Creates snippets for PHP functions
;;
;; Copyright 2013 Eric James Michael Ritz
;;
;; Author: Eric James Michael Ritz
;; URL: https://github.com/ejmr/php-auto-yasnippets
;; Version: 2.0.0
;; Package-Requires: ((php-mode "1.11") (yasnippet "0.8.0"))
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
;; can get it from the project GitHub URL at the top of this file.  By
;; default this package looks for the PHP program in the same
;; directory as this Elisp file.  You can use `setq' in your
;; configuration file to set the variable to the proper path if the
;; PHP program is in a different directory, e.g:
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
;; names, any default values, et cetera.  If you type the name of a
;; method then you need to tell the package the name of the class that
;; implements that method, otherwise it will not be able to create the
;; snippet.  Using the prefix command, e.g. `C-u C-c C-y', prompts for
;; the class name in the minibuffer.



;;; Code:

(require 'thingatpt)
(require 'php-mode)
(require 'yasnippet)


;;; This section defines constants the package uses as well as any
;;; global variables which the user may wish to change.

(defconst php-auto-yasnippet-version "2.0.0"
  "The version number for the php-auto-yasnippet package.")

(defvar php-auto-yasnippet-php-program
  (let ((current (or load-file-name (buffer-file-name))))
    (expand-file-name "Create-PHP-YASnippet.php" (file-name-directory current)))
  "The path to the program `Create-PHP-YASnippet.php'.")

;;; We use this variable to restrict the effects of the
;;; payas/remove-extra-whitespace function.  YASnippets lets us assign
;;; a hook to run after the expansion of each snippet.  We use that
;;; payas/remove-extra-whitespace hook to get rid of extra whitespace
;;; from the PHP snippets this package creates.
;;;
;;; However, the problem is we install payas/remove-extra-whitespace
;;; as a hook for YASnippets to execute after expanding *any* snippet.
;;; That means our function runs for snippets that have absolutely
;;; nothing to do with this package or PHP.  This is undesirable
;;; because our hook could mess up the behavior of other snippets by
;;; erroneously 'cleaning up' their whitespace.
;;;
;;; Ideally we only want YASnippets to run our hook if we are
;;; expanding a snippet created by this package.  To achieve this we
;;; use this php-auto-yasnippet-executing variable.  We set the
;;; variable to true inside of the primary public API function:
;;; yas/create-php-snippet, i.e. when the user generates a snippet
;;; with this package.  Then the payas/remove-extra-whitespace hook
;;; will test for this variable; if it has a true value, meaning we
;;; just ran yas/create-php-snippet, then the hook will perform its
;;; clean-up and then set the variable back to a nil value so that our
;;; hook only takes effect once after each call to
;;; yas/create-php-snippet.
;;;
;;; The ultimate effect is that payas/remove-extra-whitespace only
;;; affects snippets expanding via yas/create-php-snippet, limiting
;;; the hook's behavior to snippets this package creates.
(defvar php-auto-yasnippet-executing nil
  "Non-nil means `yas/create-php-snippet' is now working.")

(defvar php-auto-yasnippet-required-files nil
  "List of files on disk to include when creating a PHP snippet.
This makes it possible to generate snippets for user code.
It's probably best to set this per-project via .dir-locals.")


;;; Below are all of the internal functions.  All of these functions
;;; begin with the 'payas' prefix in their name, short for 'PHP Auto
;;; YASnippets'.  No code outside of this file should rely on any of
;;; these functions.

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
           (class-name (or (second input-chunks) ""))
           (args (list php-executable nil (current-buffer) nil php-auto-yasnippet-php-program)))

      (setq command-args (list function-or-method-name class-name))
      (dolist (elt php-auto-yasnippet-required-files command-args)
        (setq command-args (cons elt command-args))
        (setq command-args (cons "--require-once" command-args)))

      (setq args (append args command-args))
      (apply 'call-process args))))

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
        ;; value ERROR_UNKNOWN_FUNCTION. That means the user tried to
        ;; create a snippet for a function the program does not
        ;; recognize. So arguably we should report this via user-error
        ;; since we could say the fault is on the user. However, if we
        ;; do that then we are making the assumption that
        ;; php-auto-yasnippets made no mistake in selecting the function
        ;; name from the buffer. It is possible that the function is not
        ;; recognized because we screwed up and did not send the
        ;; complete function name. So until we are completely confident
        ;; about that aspect of the code we will treat this as an error
        ;; on our part and not as a mistake by the user.
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

The INPUT must be the name of an available PHP function. This
function creates a snippet for that function and associates it
with `php-mode'."
  (unless (gethash 'php-mode yas--tables)
    (yas--table-get-create 'php-mode))
  (unless (yas--get-template-by-uuid 'php-mode input)
    (with-temp-buffer
      (let ((exit-code (payas/create-template input)))
        (if (/= exit-code 0)
            (payas/report-error exit-code input))
        (yas-define-snippets
         'php-mode
         (list (yas--parse-template)))))))

(defun payas/remove-extra-whitespace ()
  "Remove whitespace before a function's closing parenthesis.

After expanding a snippet the user can press `C-d' to the
parameter at the point.  This is most useful for optional
parameters in PHP functions.  But this behavior leaves too much
whitespace for each parameter the user deletes.  This function
cleans up that whitespace so that the PHP code looks better."
  ;; After we're done with a snippet we move in front of the closing
  ;; bracket and remove any whitespace between here and the final
  ;; parameter. If a trailing comma is left it is also deleted.
  (when php-auto-yasnippet-executing
    (save-excursion
      (backward-char 1)
      (delete-horizontal-space)
      (backward-char 1)
      (if (looking-at-p ",")
          (delete-char 1)))
    (setq php-auto-yasnippet-executing nil)))


;;; This section contains the public API.

;;;###autoload
(defun yas/initialize ()
  "Setup yasnippet hook for php-auto-yasnippet."
  (add-hook 'yas-after-exit-snippet-hook #'payas/remove-extra-whitespace))

;;;###autoload
(eval-after-load 'yasnippet '(yas/initialize))

;;;###autoload
(defun yas/create-php-snippet (prefix)
  "Creates and expands a snippet for the PHP function at point.

If called with the universal prefix then it prompts the user for
the name of a PHP class and treats the name at point as the name
of a method for that class."
  (interactive "P")
  (let ((function (thing-at-point 'symbol))
        (class
         (if prefix
             (read-from-minibuffer "Class: "))))
    (if class
        (payas/define-template (concat function " " class))
      (payas/define-template function))
    (setq php-auto-yasnippet-executing t)
    (yas-expand)))

(provide 'php-auto-yasnippets)

;;; php-auto-yasnippets.el ends here
