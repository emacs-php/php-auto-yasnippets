Automatic YASnippets for PHP in Emacs
=====================================

The php-auto-yasnippets package for [GNU Emacs][emacs] will
automatically create ‘snippets’ for [standard PHP functions][php].  It
defines these snippets using the [YASnippet package][yas].  For
example, if you type the PHP function

```php
implode
```

and press `C-c C-y` then php-auto-yasnippets will expand that into

```php
implode($glue, $pieces)
```

with the cursor ready to overwrite `$glue` with the string you want to
use.  And then pressing Tab will skip over to `$pieces` to do the
same.  This way you can be sure you not only have the correct number
of arguments, but also that you have them in the correct order.  PHP
comes with a large standard library and functions that sound similar
sometimes require arguments in contrasting orders.  This package will
help you avoid having to remember those corner cases.

If a function has any optional parameters then php-auto-yasnippets
will wrap them in square braces.  This is the same convention the PHP
manual uses to indicate optional parameters.  For example,
php-auto-yasnippets will expand `filter_input` into this:

```php
filter_input($type, $variable_name, [$filter], [$options])
```

If you do not need the optional parameters you can delete them by
pressing `C-d` when you Tab over to them.

You can use the prefix command to expand method names.  When you use
the prefix, u.g. `C-u C-c C-y`, the package will ask you for the name
of the class which implements that method.  This information is
necessary in order to generate the correct snippet.


Requirements
------------

* PHP 5.3 or later
* [YASnippet][yas]
* [php-mode][php-mode]


Installation
------------

To use php-auto-yasnippets you need to do three things.  First, place
the package in your load-path (`C-h v load-path` for help) and load it
from your Emacs configuration file by adding:

```lisp
(require 'php-auto-yasnippets)
```

Second, make sure the variable `php-auto-yasnippet-php-program` points
to the program `Create-PHP-YASnippet.php`.  That PHP program should
have come with this package; if you do not have it then you can get it
from the [project website][home].  By default php-auto-yasnippets will
search for the PHP program in the same directory as the Elisp code,
i.e. the `php-auto-yasnippets.el`.  If you want to put the PHP program
in another place then use `setq` in your configuration file to set the
variable to the proper path, e.g:

```lisp
(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/path/to/Create-PHP-YASnippet.php")
```

Finally, bind the function `yas/create-php-snippet` to a key of your
choice.  You *must* do this because php-auto-yasnippets defines no
key-bindings.  And since the package requires php-mode, and is most
useful when writing PHP code, you may want to use a key-binding that
only works when using php-mode.  For example:

```lisp
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
```

Now if you type the name of a PHP function and press `C-c C-y` it will
expand into a snippet containing all of the parameters, their names,
any default values, et cetera.


Loading User Code
-----------------

This package can also generate snippets for user-defined functions and methods.

You can tell a buffer what files to load for function and class definitions by
setting `php-auto-yasnippet-required-files` to a list of required paths.

If you use [Composer](http://getcomposer.org/), you might put something like
this in `~/project/.dir-locals.el`:

```lisp
(php-mode . ((php-auto-yasnippet-required-files (list "~/project/vendor/autoload.php"))))
```

Now you can generate snippets for any classes Composer autoloads, in any PHP
file in the project.


Contributors
------------

* [Glynn Forrest](http://glynnforrest.com)
* [Yasuyuki Oka](http://yasuyk.github.io/)
* [Steve Purcell](http://www.sanityinc.com/)
* [Nate Eagleson](http://www.nateeag.com/)


Miscellaneous
-------------

This package uses [Semantic Versioning][semver].


License
-------

[GNU General Public License][gpl]

Copyright 2013 Eric James Michael Ritz



[emacs]: http://www.gnu.org/software/emacs/
[php]: http://php.net/
[yas]: https://github.com/capitaomorte/yasnippet
[php-mode]: https://github.com/ejmr/php-mode
[gpl]: http://www.gnu.org/copyleft/gpl.html
[home]: https://github.com/ejmr/php-auto-yasnippets
[semver]: http://semver.org/
