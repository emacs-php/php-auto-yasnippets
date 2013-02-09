Automatic YASnippets for PHP in Emacs
=====================================

**Note:** *This package is usable but still under heavy, rapid
  development.  You may want to wait for the 1.0.0 release.*

The php-auto-yasnippets package for [GNU Emacs][emacs] will
automatically create ‘snippets’ for [standard PHP functions][php].  It
defines these snippets using the [YASnippet package][yas].  For
example, if you type the PHP function

```php
implode
```

and press `C-c C-y` php-auto-yasnippets will expand that into

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


Requirements
------------

* [YASnippet][yas]
* [php-mode][php-mode]


Installation
------------

To use php-auto-yasnippets you need to do three things.  First, place
the package in your load-path (`C-h v load-path` for help) and load it
from your Emacs configuration file by adding:

```elisp
(require 'php-auto-yasnippets)
```

Second, make sure the variable `php-auto-yasnippet-php-program` points
to the program `Create-PHP-YASnippet.php`.  That PHP program should
have come with this package; if you do not have it then you can get it
from the [project website][home].  You can use `setq` in your
configuration file to set the variable to the proper path, e.g:

```elisp
(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/path/to/Create-PHP-YASnippet.php")
```

Finally, bind the function `yas/create-php-snippet` to a key of your
choice.  You *must* do this because php-auto-yasnippets defines no
key-bindings.  And since the package requires php-mode, and is most
useful when writing PHP code, you may want to use a key-binding that
only works when using php-mode.  For example:

```elisp
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
```

Now if you type the name of a PHP function and press `C-c C-y` it will
expand into a snippet containing all of the parameters, their names,
any default values, et cetera.


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
