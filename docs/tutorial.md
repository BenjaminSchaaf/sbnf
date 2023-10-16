# Tutorial

## Prerequisites

* [Sublime Text](https://sublimetext.com) (build 4077 or later)
* You'll need to have SBNF installed. Instructions are in the
  [README](https://github.com/BenjaminSchaaf/sbnf).
* Familiarity with
  [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression)

## Introduction & Setup

SBNF is a language for writing syntax definitions for Sublime Text. Syntax
definitions drive syntax highlighting, goto-definition and auto-complete. It's
usually desirable to have fairly accurate syntax definitions, resulting in a
lot of complexity. Unfortunately sublime-syntax - the format in which syntax
definitions are usually written - is fairly low level.

SBNF's purpose is to simplify making syntax definitions. It does so by providing
a higher level language that is compiled down to the sublime-syntax format that
Sublime Text can read.

SBNF syntax definitions are contained within a single file with a `.sbnf`
extension. Lets start out by making one of these files. In order for Sublime
Text to load the resulting sublime-syntax we'll need to place it in the
`Packages` folder. This can be found in the following locations:

* **Windows**: `%APPDATA%\Sublime Text`
* **Mac**: `~/Library/Application Support/Sublime Text`
* **Linux**: `~/.config/sublime-text`

Within the `Packages` folder we want to enter/create the folders
`User/My-Syntax`. Within there we'll create a file called `my-syntax.sbnf`. So
on Linux this would be
`~/.config/sublime-text/Packages/User/My-Syntax/my-syntax.sbnf`.

Opening this file with Sublime Text we can compile the syntax using the *sbnf*
build system by going to *Tools > Build*. This should produce a panel that says
something like `[Finished in 40ms]` and produce a `my-syntax.sublime-syntax`
file next to the `.sbnf` file.

Alternatively you can use the command line tool and run:

```
$ sbnf my-syntax.sbnf
```

Now that we have a sublime-syntax in the right location Sublime Text should have
already loaded it, so we can make a new tab (*File > New File*) and select our
syntax by going to *View > Syntax > my-syntax*.

Whenever you make changes to the sbnf file you will have to build it again, but
after that you should see it apply immediately.

## Hello SBNF

We like to call the syntax definition a "grammar". The basic components of a
grammar are rules. Rules consist of an expression of rules and terminals.
Terminals are regular expressions.

Lets make a very simple grammar to explain each part:

```sbnf
# this is a comment
main : '[a-zA-Z0-9]' ;
```

Here we're defining one rule called `main`. This name is special, as it
signifies the entry point of the grammar. There must always be a `main` rule.

The punctuation `:` signifies the beginning of the definition of the rule. This
reads as "`main` is defined as".

Next we have a terminal `'[a-zA-Z0-9]'`. This is a regex matching the english
alphabet, lower and upper case as well as the numbers 0-9.

Finally the `;` signals the end of the rule.

Building this grammar and applying it to a file we can try out the following
text:

```
let a = 1;
```

You can see that the parts that match the regex (`let`, `a` and `1`) are valid,
whereas the symbols `=` and `;` are highlighted as invalid. The white-space
between each part is also valid. SBNF always makes the assumption that
white-space between terminals is valid.

Lets add some extra color in there, we don't just want the invalid stuff to be
highlighted. In Sublime Text syntax-definitions assign color through a
color-scheme. Instead of directly specifying a color we instead need to specify
a "scope". These have semantic names like "keyword" or "punctuation".

In SBNF we can assign scopes by annotating terminals, for example:

```
main : '[a-zA-Z0-9]'{keyword} ;
```

Now the text previously highlighted as regular text is instead the color of
a "keyword".

Scope naming is a whole separate thing to SBNF so it won't be covered in this
tutorial. You can find documentation on common scopes and conventions in the
Sublime Text documentation
[here](https://www.sublimetext.com/docs/scope_naming.html).

## Expressions

Just having a single terminal isn't all that useful. The most common ways to
combine terminals are concatenation, alternation, repetition and optional.
These work in a similar way and have the same syntax as regular expressions.

### Concatenation

If you want to match a terminal, but only after another terminal was matched,
you use concatenation. This is done by simply separating terminals by
whitespace:

```sbnf
main : 'let' '[a-zA-Z]+' ;
```

This grammar only matches when `[a-zA-Z]+` follows directly after a `let`,
optionally separated by whitespace. See if you can predict the outcome of each
of these with this grammar applied:

```
let a

let a = 1;

let
aaa

let let

aleta ale ta
```

Concatenation can chain as much as needed:

```sbnf
main : '\blet\b' '\b[a-zA-Z]+\b' '=' '[0-9]+' ';' ;
```

### Alternation

If you want to match a terminal or a different terminal you use an alternation.
This is done by separating terminals by a `|`:

```sbnf
main : 'let' | 'const' ;
```

This grammar matches both `let` and `const` in any order. See if you can predict
the outcome of each of these with this grammar applied:

```
let const
const let
let let
const const

aconstaleta

letconstlet
```

Alternation can be chained as much as needed. It also has a lower priority than
concatenation, so you can alternate over various concatenations:

```sbnf
main : '\bconst\b' '\b[a-zA-Z]+\b' ';'
     | '\blet\b' '\b[a-zA-Z]+\b' ';'
     ;
```

### Repetition

If you want to match a terminal many times you use a repetition. This is done by
putting a `*` after the terminal:

```sbnf
main : 'let' '[a-zA-Z]'* ';' ;
```

This grammar matches any number of `[a-zA-Z]` after a `let`, until a `;` is
reached. See if you can predict the outcome of each of these with this grammar
applied:

```
let;

let a aa ;

let
a
;

let a . a

letletlet;
```

One thing you may have noticed is that there's an implicit repetition around the
grammar. In proper grammars this shouldn't be relied upon due to some edge-case
behavior. Instead we properly put our whole grammar in a repetition by using
`()` to group our expressions:

```sbnf
main : ( '\bconst\b' '\b[a-zA-Z]+\b' ';'
       | '\blet\b' '\b[a-zA-Z]+\b' ';'
       )*
     ;
```

### Optional

If you want to maybe match something, allowing it to be matched if it's there
but ignoring it if it isn't you use an optional. This is done by putting a `?`
after the terminal:

```sbnf
main : 'let' '[a-zA-Z]+' ( '=' '[0-9]+' )? ';' ;
```

This grammar matches `let`, followed by `[a-zA-Z]+` and then either a `;` or `=`
followed by `[0-9]+` and ending with a `;`. See if you can predict the outcome
of each of these with this grammar applied:

```
let a;

let a = 6;

let;

let a = 4, b = 2;
```

## Multiple Rules

So far we've only been using a single rule, but in order to make more complex
grammars we'll want to use multiple. The names of rules must be lower-case and
can contain otherwise `[\-0-9]`. Rules can be used like terminals:

```sbnf
main : ( const-def | let-def )* ;

const-def : '\bconst\b' '\b[a-zA-Z]+\b' ';' ;
let-def : '\blet\b' '\b[a-zA-Z]+\b' ';' ;
```

Rules can also be used recursively:

```sbnf
main : expression* ;

expression : number
           | expression operator expression
           | '\(' expression '\)'
           ;

operator : '[\-\+\*/]'{keyword.operator} ;

number : '[0-9]+'{constant.numeric} ;
```

This grammar parses basic mathematic operations, for example:

```
1 * 2 + 4 / (12 - 5)
```

Hopefully this shows how you can make complex syntax definitions with these
fairly simple building blocks.

## Why not just use a regex?

A question you may be asking is what the real difference between regexes and
grammars are. Without extensions regexes can't be recursive, but even then a
lot of the previous examples could be expressed through simple regexes.

Take for example the following grammar:

```sbnf
main : '\blet\b' '\b[a-zA-Z]+\b' '=' '[0-9]+' ';' ;
```

This should be equivalent to the following regex:

```regex
\blet\b\s*\b[a-zA-Z]+\b\s*=\s*[0-9]+\s*;
```

The issue with doing this is unfortunately that Sublime Text's syntax
highlighting engine works on a line-by-line basis. So regexes can't span
multiple lines.

You need to take this into account when writing your grammars. Ideally as much
should be placed into regexes as possible, but whenever a newline can appear
you'll need to use separate terminals.

## Advanced Terminals

So far we've covered all the basics of terminals, but they have a few more
important features that may be useful.

### Literals

Instead of `'` if you use `` ` `` for regexes the contents are automatically
escaped. Often you don't really need any regex features so using a literal
instead results in an easier to read and write grammar.

For example:

```sbnf
main : '[a-zA-Z]' ( `.` '[a-zA-Z]' )* ;
```

### Regex Captures

When specifying the scope to assign a terminal (eg: `'let'{keyword}`) you can
also specify scopes to assign to regex capture groups.

For example:

```sbnf
main : '([a-zA-Z]+)(?:(\.)([a-zA-Z]+))*'
       {meta.path, 1: entity.name, 2: punctuation, 3: entity.name}
     ;
```

This will have `meta.path` assigned to the whole regex, `entity.name` assigned
to each of the `[a-zA-Z]+` and `punctuation` assigned to `\.`.

### Embed/Include

TODO

## Clauses

Aside from rules there's another top level structure available in SBNF called
clauses. These are equivalent to what you might call constants or functions in
a programming language.

Clauses map from a name to a terminal, with the limitation of not having any
scope annotations. The name must be upper-case or use `_`. Clauses can be used
like a terminal and are thus useful for avoiding repetition. For example:

```sbnf
IDENTIFIER = '\b[A-Za-z]+\b'

main : '\blet\b' IDENTIFIER ';'
     | '\bconst\b' IDENTIFIER{entity.name} ';'
     ;
```

Clauses can also be interpolated from regex terminals, within a rule or another
clause. This is done by referring to the clause from within `#[]`. This can
make composing terminals easy:

```sbnf
IDENTIFIER = '\b[A-Za-z]+\b'

main : '#[IDENTIFIER](\.#[IDENTIFIER])*' ;
```

Clauses can also be interpolated into scope annotations:

```sbnf
ID = '\b[A-Za-z]+\b'
ID_SCOPE = `entity.name`

main : '(#[ID])(?:\.(#[ID]))*'{1: #[ID_SCOPE], 2: #[ID_SCOPE]} ;
```

### Special Clauses

Clauses are also used by SBNF to specify metadata for the grammar. These special
clauses can be used just like any other, but affects the resulting
sublime-syntax as well:

* `NAME`: The name of the syntax shown by Sublime Text. This defaults to the
  base-name of the sbnf file. For example `my-syntax.sbnf` will have
  `my-syntax` as the default name.
* `EXTENSIONS`: A space-separated list of file extensions Sublime Text uses to
  automatically assign the syntax.
* `FIRST_LINE`: A regex for matching the first line of a file. Sublime Text uses
  this regex to automatically assign the syntax when a file is loaded.
* `SCOPE`: The default scope for the grammar. This defaults to `source.`
  followed by the lowercased name of the syntax. For example `my-syntax.sbnf`
  will have `source.my-syntax` as the default scope.
* `SCOPE_POSTFIX`: A postfix appended to all scopes in the grammar (excluding
  the `SCOPE` clause). This defaults to the name lowercased. Can be left empty
  to leave out the postfix.

  It's common for syntax definitions to have every scope ending with the name of
  the syntax, allowing color schemes to specify rules for specific syntaxes.
  This does so automatically, so a terminal like `'foo'{keyword}` will get
  compiled to a scope `keyword.my-syntax`.
* `HIDDEN`: Whether the syntax will be shown in the menu in Sublime Text. Must
  be either `'true'` or `'false'`.

Here's an example of each special clause:

```
NAME = `C++`
EXTENSIONS = 'cc cpp h'
FIRST_LINE = '#!/.*(gcc|clang)'
SCOPE = 'source.cpp'
SCOPE_POSTFIX = 'cpp'
HIDDEN = 'false'
```

### Parametrization

Clauses can have parameters, allowing you to transform one or more other
clauses. These are equivalent to functions in other programming languages, but
they transform the clauses during building.

```sbnf
POSTFIXED[NUMBER, AFTER] = '#[NUMBER][lLhHfF]#[AFTER]' ;

integer = POSTFIXED['[0-9]+', ''] ;
real = POSTFIXED['[0-9]+', '\.[0-9]+']{constant.numeric} ;
```

This is equivalent to:

```sbnf
integer : '[0-9]+[lLhHfF]' ;
real : '[0-9]+[lLhHfF]\.[0-9]+' ;
```

## Advanced Rules

So far we've covered all the basics of rules, but they have a few more important
features that may be useful.

### Rule Scopes

Like terminals, rules can also be assigned a scope. This scope is applied to
everything within that rule. Importantly this includes white-space. For example:

```sbnf
path{meta.path} : '[a-zA-Z]+'{entity} (`.` '[a-zA-Z]+'{entity})* ;
```

Is is mostly equivalent to:

```sbnf
path : '[a-zA-Z]+'{meta.path entity}
       (`.`{meta.path} '[a-zA-Z]+'{meta.path entity})*
     ;
```

Note that capture groups do not work on rules.

### `prototype` rule

There's a second special rule aside from `main` called `prototype`. The grammar
from this rule is effectively applied "between" every terminal. This is useful
for things like comments which can appear anywhere. For example:

```sbnf
prototype : ( ~'#.*\n?' )* ;
main : ( 'a' 'b' )* ;
```

The `~` is a passive expression. This is explained later, but necessary for
prototype to work as expected.

This grammar will match the following:

```
# a b with spaces
a b # comment after ab
# ab without spaces
ab
a # comment directly after a
b
```

### Parametrization

Like clauses, rules can also be parametrized.

Note that special rules like `main` can't be parametrized.

## Ambiguity

TODO

## Passive Expression

TODO

## Pattern Matching

When using parameters it may be useful to have different expressions depending
on the parameters supplied. In SBNF this is done by pattern-matching
parameters. Instead of defining a clause as an input you can provide a
terminal:

```sbnf
branch['a'] : 'a' ;
branch['b'] : 'b' ;

main : branch['a'] branch['b'] ;
```

## Grammar Parametrization

Grammars can also be parametrized. This is done using the same syntax as for
rules and clauses but at the start of the file.

This can be used with pattern matching to build variations of a grammar without
having to write largely identical but separate files. For example:

```sbnf
[COMMENT]

prototype : ( ~'#[COMMENT].*\n?' )* ;
```

This grammar lets you choose which characters to use for single-line comments
when building.

To build this grammar the parameters need to be provided on the command line.
For example:

```bash
$ sbnf my-syntax.sbnf '#' -o my-syntax-with-hash.sublime-syntax
$ sbnf my-syntax.sbnf '//' -o my-syntax-with-forward-slash.sublime-syntax
```