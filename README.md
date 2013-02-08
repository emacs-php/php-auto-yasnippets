Automatic YASnippets for PHP in Emacs
=====================================

**NOTE:** *This program is still in development and currently unusable!*

This package for [GNU Emacs][emacs] will automatically create
‘snippets’ for [PHP][php] via the [YASnippet package][yas].  For
example, if you type the PHP function

```php
implode
```

this package will expand that into

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


License
-------

[GNU General Public License][gpl]

Copyright 2013 Eric James Michael Ritz



[emacs]: http://www.gnu.org/software/emacs/
[php]: http://php.net/
[yas]: https://github.com/capitaomorte/yasnippet
[php-mode]: https://github.com/ejmr/php-mode
[gpl]: http://www.gnu.org/copyleft/gpl.html
