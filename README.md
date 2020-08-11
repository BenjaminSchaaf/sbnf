# SBNF

[![Build Status](https://travis-ci.org/BenjaminSchaaf/sbnf.svg?branch=master)](https://travis-ci.org/BenjaminSchaaf/sbnf) [![Crate](http://meritbadge.herokuapp.com/sbnf)](https://crates.io/crates/sbnf)

A BNF-style language for writing sublime-syntax files.

Try it out now on the [Live Playground!](https://benjaminschaaf.github.io/sbnf/playground.html)

SBNF is currently used for
[SWI-Prolog](https://github.com/BenjaminSchaaf/swi-prolog-sublime-syntax).

## Motivation & Goals

Writing syntax definitions is error prone and the result is hard to maintain.
The addition of `branch_point`, while a great feature, dramatically increases
complexity and duplication when used.

SBNF attempts do the following:
* Provide a maintainable, declarative language for writing sublime syntax
  definitions
* Compile quickly for fast iteration
* Compile to an efficient syntax, comparable to hand-made ones

## Installation

With [rust installed](https://www.rust-lang.org/tools/install) you can download,
build and install the latest released version of SBNF using:

```bash
$ cargo install sbnfc
```

Or if you want the latest features, clone this repository, then build and
install using:

```bash
$ cargo install --path cli
```

Note that in order to use the generated syntax you'll need at minimum Sublime
Text build 4077 with support for version 2 of Sublime Syntax.

## Example

The following is a sbnf grammar for a cut-down version of C. It only allows
global/local variable declarations, function definitions and simple function
calls. Even this cut down version is extremely difficult to parse correctly with
the required `meta.function` and `meta.function-call` scopes, as both function
definitions and function calls require branch points.

```sbnf
name: simplec

prototype = ( ~comment )* ;

comment = '(//+).*\n?'{comment.line, 1: punctuation.definition.comment} ;

main = ( variable-declaration | function-definition )* ;

function-definition{meta.function}
= type
  '\b[A-Za-z_]+\b'{entity.name.function}
  `(`
  `)`
  block
;

block{meta.block} = '{' statement* '}' ;

statement = variable-declaration
          | value ';'
          | block
          ;

variable-declaration = type '\b[A-Za-z_]+\b'{variable} ( '=' value )? ';' ;

type = '\b[A-Za-z_]+\b'{storage.type} ;

value = '[0-9]+'{constant.numeric}
      | function-call
      ;

# Function calls don't have arguments :)
function-call{meta.function-call}
= '\b[A-Za-z_]+\b'{variable.function meta.path} `(` `)` ;
```

The above grammar compiles to the following:

```yaml
%YAML 1.2
---
# http://www.sublimetext.com/docs/3/syntax.html
version: 2
name: simplec
scope: source.simplec
contexts:
  block|0:
    - meta_content_scope: meta.block.simplec
    - include: include!block@1
    - match: '[0-9]+'
      scope: meta.block.simplec constant.numeric.simplec
      push: [block|meta, statement|0]
    - match: '{'
      scope: meta.block.simplec meta.block.simplec
      push: [block|meta, block|0]
    - match: '}'
      scope: meta.block.simplec
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  block|1|block@1:
    - match: '\b[A-Za-z_]+\b'
      scope: meta.block.simplec variable.simplec
      set: [block|meta, variable-declaration|2]
    - match: '\S'
      fail: block@1
  block|2|block@1:
    - match: '\('
      scope: meta.block.simplec meta.function-call.simplec
      set: [block|meta, statement|0, function-call|1]
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  block|meta:
    - meta_content_scope: meta.block.simplec
    - match: ''
      pop: true
  function-call|0:
    - meta_content_scope: meta.function-call.simplec
    - match: '\('
      scope: meta.function-call.simplec
      set: function-call|1
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-call|1:
    - meta_content_scope: meta.function-call.simplec
    - match: '\)'
      scope: meta.function-call.simplec
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-call|2|block@1:
    - meta_include_prototype: false
    - match: '\b[A-Za-z_]+\b'
      scope: meta.function-call.simplec variable.function.simplec meta.path.simplec
      push: block|2|block@1
      pop: true
  function-definition|0:
    - meta_content_scope: meta.function.simplec
    - match: '\)'
      scope: meta.function.simplec
      set: function-definition|1
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-definition|1:
    - meta_content_scope: meta.function.simplec
    - match: '{'
      scope: meta.function.simplec meta.block.simplec
      set: [function-definition|meta, block|0]
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-definition|meta:
    - meta_content_scope: meta.function.simplec
    - match: ''
      pop: true
  include!block@1:
    - match: '(?=\b[A-Za-z_]+\b)'
      branch_point: block@1
      branch:
        - type|2|block@1
        - function-call|2|block@1
  include!main@1:
    - match: '(?=\b[A-Za-z_]+\b)'
      branch_point: main@1
      branch:
        - type|0|main@1
        - type|1|main@1
  main:
    - include: include!main@1
    - match: '\S'
      scope: invalid.illegal.simplec
  main|0|main@1:
    - match: '\b[A-Za-z_]+\b'
      scope: variable.simplec
      push: main|2|main@1
      pop: true
    - match: '\S'
      fail: main@1
  main|1|main@1:
    - match: '\b[A-Za-z_]+\b'
      scope: meta.function.simplec entity.name.function.simplec
      push: main|3|main@1
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  main|2|main@1:
    - match: '='
      set: variable-declaration|0
    - match: ';'
      pop: true
    - match: '\S'
      fail: main@1
  main|3|main@1:
    - match: '\('
      scope: meta.function.simplec
      set: function-definition|0
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  prototype:
    - match: '(//+).*\n?'
      scope: comment.line.simplec
      captures:
        1: punctuation.definition.comment.simplec
  statement|0:
    - match: ';'
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  type|0|main@1:
    - meta_include_prototype: false
    - match: '\b[A-Za-z_]+\b'
      scope: storage.type.simplec
      push: main|0|main@1
      pop: true
  type|1|main@1:
    - meta_include_prototype: false
    - match: '\b[A-Za-z_]+\b'
      scope: meta.function.simplec storage.type.simplec
      push: main|1|main@1
      pop: true
  type|2|block@1:
    - meta_include_prototype: false
    - match: '\b[A-Za-z_]+\b'
      scope: storage.type.simplec
      push: block|1|block@1
      pop: true
  variable-declaration|0:
    - match: '[0-9]+'
      scope: constant.numeric.simplec
      set: variable-declaration|1
    - match: '\b[A-Za-z_]+\b'
      scope: meta.function-call.simplec variable.function.simplec meta.path.simplec
      set: [variable-declaration|1, function-call|0]
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  variable-declaration|1:
    - match: ';'
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  variable-declaration|2:
    - match: '='
      set: variable-declaration|0
    - match: ';'
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
```

## Usage

A SBNF file contains two types of elements: headers and rules. Headers provide
meta-data for the syntax, such as it's name, while rules are the bnf-style rules
that define the parsing and scoping of the grammar.

Comments in SBNF start with a `#` and end at the next newline.

See `sbnf.sbnf` for a full example grammar.

### Headers

Headers are in the form `<head>: <value>`, ending at the end of the line. The
following headers are allowed:

* `name`: The name of the syntax. This defaults to the base-name of the sbnf
  file.
* `extensions`: A space-separated list of file extensions. Equivalent to
  `file_extensions` in sublime-syntax.
* `first-line`: A regex for matching the first line of a file. Equivalent to
  `first_line_match` in sublime-syntax.
* `scope`: The default scope for the grammar. This defaults to `source.`
  followed by the name lowercased.
* `scope-postfix`: A postfix appended to all scopes in the grammar (excluding
  the `scope` header). This defaults to the name lowercased. Can be left empty
  to leave out the postfix.
* `hidden`: Whether the syntax will be shown in the menu in Sublime Text.

Example:

```
name: SBNF
extensions: sbnf
# Don't need this, as this is already the default
# scope: source.sbnf
```

### Rules

Rules are in the form `<identifier> <arguments> <options> = <expression> ;`.
Identifiers may contain any alphanumeric character as well as `-`, `_` and `.`.

Like sublime-syntax files, SBNF grammars have two entry points: `main`,
`prototype`. They behave identically to those in sublime-syntax files. Only
rules used directly or indirectly from an entry point are compiled.

Rules have two sets of optional parameters: arguments and options. Arguments are
used for meta-programming and options are used for sublime-syntax specific
options.

Examples:

```sbnf
a = 'a' ;
b{source.b} = 'b' ;
c[S] = 'c'{#[S]} ;
d[S]{text.d} = a b c[S] ;
```

#### Expressions

Expressions may take any of the following forms:

* `` `<literal>` <options>``: A terminal matching text literally.
* `'<regex>' <options>`: A terminal matching text according to a regex.
* `<identifier> <arguments>`: A non-terminal matching another rule.
* `<expr> | <expr>`: An alternation of expressions. The grammar matches either
  the left or right expression. This can be used as a list, eg:
  `'a' | 'b' | 'c'`.
* `<expr> <expr>`: A concatenation of expressions. The grammar matches the left
  expression followed by the right expression. This can be used as a list, eg:
  `'a' 'b' 'c'`.
* `(<expr>)`: A grouping.
* `<expr>?`: An optional expression. The grammar matches nothing or the
  expression.
* `<expr>*`: A repeating expression. The grammar matches the expression any
  number of times, including 0.
* `~<expr>`: A passive expression. The grammar matches *any text* until the
  expression matches.

#### Options

Options come in the following form: `{<param>, <key>: <value>}`. `<param>`,
`<key>` or `<value>` may contain any text except `,`, `:` or `}`. There may be
any number of options given, as allowed by whatever the options are for.
When there are no options the `{}` are optional.

The following options are allowed for rules:

* `<meta-scope>`: The meta-scope of the rule. Equivalent to `meta_scope` or
  `meta_content_scope` in sublime-syntax.

Literal and regex terminals are allowed the following arguments:

* `<scope>`: The scope of the terminal.
* `<capture>: <scope>`: The scope for a regex capture group. `<capture>` must be
  an integer.

#### Arguments

Arguments for rules and non-terminals take the form: `[<value>, <value>]`.
`<value>` may be either a regex terminal, a literal terminal or an identifier.
The same name may be used for rules with different sets of arguments, though the
number of arguments must be the same.

A rule with arguments is instantiated when it is used as a non-terminal with
matching arguments. Argument matching is based on the type and value of the
argument. Terminal arguments are matched based on regex equivalence, while rule
arguments are matched on same-name. Terminals and rules never match.

An identifier that does not reference a rule is a free variable unique to the
rule's scope. It matches any argument and may be passed on and or interpolated.

A variable may be interpolated using the following syntax: `#[]`. This can be
done inside any terminal or inside options.

Examples:

```sbnf
main
= a['a'] # instantiates rule 1
| a[a]   # instantiates rule 2
| a['b'] # instantiates rule 3
| b['b'] # error: Ambiguous instantiation
;

# Rule 1.
a['a'] = 'a' ;

# Rule 2.
a[a] = 'a' ;

# Rule 3.
a[A] = 'a' ;

b[A] = 'a' ;
b[B] = 'b' ;
```

#### Include/Embed

SBNF also has support for including/embedding other sublime syntaxes. This can
only be done on a literal or regex terminal expression with a postfix of
`%include[<with_prototype>]{<syntax>}` for including a syntax or
`%embed[<regex>]{<syntax>}` for an embed.

Note that these translate directly to the sublime syntax include/embed
functionality and thus have the same limitations.

Examples:

```sbnf
# This is a basic implementation of the html script tag embedding the javascript
# syntax.
script
= '<script>'{tag.begin.script}
  %embed['</script>']{scope:source.js, embedded.js, 0: tag.end.script}
;
```

```yaml
# The above translates to the following context
script:
  - match: '<script>'
    scope: tag.begin.script.example
    embed: scope:source.js
    embed_scope: embedded.js.example
    escape: '</script>'
    escape_captures:
      0: tag.end.script.example
    pop: true
  - match: '\S'
    scope: invalid.illegal.example
```

```sbnf
# This is a basic implementation of a regex string. It has a prototype rule that
# extends the regex syntax with an escape sequence for the string.

regex-prototype{include-prototype: false}
= ( ~`\'`{constant.character.escape} )*
  # A lookahead is required here, as otherwise we would only pop one context
  # The same is required in a sublime-syntax file
  ~'(?=\')'
;

regex-string{string.quoted}
= `'`{punctuation.definition.string.begin}
  %include[regex-prototype]{scope:source.regexp}
  `'`{punctuation.definition.string.end}
;
```

```yaml
# The above translates to the following contexts
regex-string:
  - meta_content_scope: string.quoted.example
  - match: ''''
    scope: string.quoted.example punctuation.definition.string.begin.example
    set: [regex-string|0, regex-string|1]
  - match: '\S'
    scope: invalid.illegal.example
regex-string|0:
  - meta_content_scope: string.quoted.example
  - match: ''''
    scope: string.quoted.example punctuation.definition.string.end.example
    pop: true
  - match: '\S'
    scope: invalid.illegal.example
    pop: true
regex-string|1:
  - meta_include_prototype: false
  - match: ''
    set: scope:source.regexp
    with_prototype:
      - include: regex-prototype|0
regex-prototype|0:
  - meta_include_prototype: false
  - match: '\\'''
    scope: constant.character.escape.example
  - match: '(?='')'
    pop: true
```

### Command Line

```bash
$ sbnf --help
SBNF compiler 0.1.0

USAGE:
    sbnf [FLAGS] <INPUT> [OUTPUT]

FLAGS:
    -g               Compile with debug scopes
    -h, --help       Prints help information
    -q               Do not display warnings
    -V, --version    Prints version information

ARGS:
    <INPUT>     The SBNF file to compile
    <OUTPUT>    The file to write the compiled sublime-syntax to. Leaving
                this out will instead write to stdout
```

## Limitations

### Regex Equivalence

When determining whether to create a branch point in the sublime-syntax, SBNF
has to consider whether regexes overlap. Take the following example:

```
main = 'aa?'{scope1} 'b'
     | 'a'{scope2} 'c'
     ;
```

The regexes `'aa?'` and `'a'` both match `a`, meaning a branch point would be
required to correctly parse this syntax. SBNF *does not* create a branch point
here. Due to the complexities of regex, a branch point is only created with
equivalent regexes. Rewriting the example to work as expected with SBNF yields
the following:

```
main = 'aa'{scope1} 'b'
     | 'a'{scope1} 'b'
     | 'a'{scope2} 'c'
     ;
```

This is unlikely to change in the future, as SBNF does not make any attempt to
understand any regexes.

## TODO

* Fix known edge cases in compiler. In a couple places we panic!() instead of
  providing an implementation.
* Add warnings for when branches are used in non-popping loops.
* Fix infinite loop/recursion when rule refers to itself