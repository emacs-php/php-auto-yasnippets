<?php

/*********************************************************************
 *
 * This program accepts the name of a PHP function as a command-line
 * argument and returns a 'snippet' representing that function and its
 * parameters for use with the php-auto-yasnippets Emacs package:
 *
 *     https://github.com/ejmr/php-auto-yasnippets
 *
 * Copyright 2013 Eric James Michael Ritz
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/.
 *
 ********************************************************************/

/* The error codes we use.  These follow the common practice of using
 * zero for success and various non-zero values for errors.  However,
 * we do not create any output for errors, such as error messages.
 * Because the output of this program is intended to appear in an
 * Emacs buffer we do not want to clutter that buffer with things like
 * error messages.  If the program cannot produce useful output then
 * it exits silently with an error code.
 */
define("SUCCESS", 0);
define("ERROR_NOT_CLI", 1);
define("ERROR_MISSING_ARGUMENT", 2);
define("ERROR_UNKNOWN_FUNCTION", 3);

/* We only want to be able to run this from the command-line.  It
 * should be fine to run as part of another SAPI as well, but honestly
 * who knows.  Better to lock this up as tightly as possible than to
 * find out later that it creates an obscure security hole.
 */
if (PHP_SAPI !== "cli")
{
        exit(ERROR_NOT_CLI);
}

/* We have the right number of arguments?  We should have two at a
 * minimum: the name of the program itself, and the function name.
 */
if ($argc < 2)
{
        exit(ERROR_MISSING_ARGUMENT);
}

/* If we get to here then we have a name on the command-line.  It may
 * not actually be a proper function name though.  So when we create
 * the ReflectionFunction object we need to check for the exception it
 * may throw if the function is unrecognized.
 */
$function_name = (string) $argv[1];
try
{
        $function = new ReflectionFunction($function_name);
}
catch(ReflectionException $error)
{
        exit(ERROR_UNKNOWN_FUNCTION);
}

/* Snippets can have 'directives', documented here:
 *
 *     http://capitaomorte.github.com/yasnippet/snippet-development.html
 *
 * We need to create two directives: '#key' and '#name'.  They tell
 * YASnippet what to look for to trigger the expansion of the snippet
 * (#key) and what to show in the menu of available snippets (#name).
 * The name of the function suffices for both of these.
 *
 * We put all of the directives together into a single string that we
 * will attach to the rest of the output later.
 */
$snippet_directives = sprintf(
        "#key: %s\n#name: %s\n# --\n",
        $function_name,
        $function_name
);

/* We assume the name of the function is already in the buffer and
 * that Emacs will append any output to that.  So we create an array
 * of strings, each representing a parameter for the function, and
 * then combine them in the end to create our output.
 */
$snippet_chunks = [];

foreach ($function->getParameters() as $parameter)
{
        $snippet_chunks[] = sprintf(
                '${%d:%s%s}',
                // We must add one to the position because PHP starts
                // from zero, but for the snippet we want parameter
                // numbering to start from one.
                $parameter->getPosition() + 1,
                $parameter->isPassedByReference() === true ? "&" : "",
                $parameter->getName()
        );
}

/* Now that we have built all the pieces of the snippet we can combine
 * them, wrap the parameter chunks in parentheses, and be done.
 */
printf("%s%s(%s)",
       $snippet_directives,
       $function_name,
       implode(", ", $snippet_chunks));

exit(SUCCESS);
