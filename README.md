# SBNF

[![Build Status](https://travis-ci.org/BenjaminSchaaf/sbnf.svg?branch=master)](https://travis-ci.org/BenjaminSchaaf/sbnf) [![Crate](http://meritbadge.herokuapp.com/sbnf)](https://crates.io/crates/sbnf)

A BNF-style language for writing sublime-syntax files.

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

Currently you'll need to [install rust](https://www.rust-lang.org/tools/install)
to use SBNF. With rust installed you can download, build and install the latest
released version of SBNF using:

```bash
$ cargo install sbnf
```

Or if you want the latest features, clone this repository, then build and
install using:

```bash
$ cargo install --path .
```

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

function-definition{meta.function} = type
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
name: simplec
scope: source.simplec
contexts:
  block|0:
    - meta_content_scope: meta.block.simplec
    - match: '(?=\b[A-Za-z_]+\b)'
      branch_point: block@1
      branch:
        - type|4|block@1
        - function-call|2|block@1
    - match: '[0-9]+'
      scope: constant.numeric.simplec
      push: statement|0
    - match: '{'
      scope: meta.block.simplec
      push: block|0
    - match: '}'
      scope: meta.block.simplec
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-call|0:
    - meta_content_scope: meta.function-call.simplec
    - match: '\('
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
      push: [pop-2, function-call|3|block@1]
  function-call|3|block@1:
    - match: '\('
      scope: meta.function-call.simplec
      set: [statement|0, function-call|1]
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-definition|0|main@0:
    - match: '\('
      scope: meta.function.simplec
      set: function-definition|1
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-definition|1:
    - meta_content_scope: meta.function.simplec
    - match: '\)'
      set: function-definition|2
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-definition|2:
    - meta_content_scope: meta.function.simplec
    - match: '{'
      scope: meta.block.simplec
      set: [function-definition|meta, block|0]
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  function-definition|meta:
    - meta_content_scope: meta.function.simplec
    - match: ''
      pop: true
  main:
    - match: '(?=\b[A-Za-z_]+\b)'
      branch_point: main@0
      branch:
        - type|0|main@0
        - type|2|main@0
    - match: '\S'
      scope: invalid.illegal.simplec
  pop-2:
    - meta_include_prototype: false
    - match: ''
      pop: 2
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
  type|0|main@0:
    - meta_include_prototype: false
    - match: '\b[A-Za-z_]+\b'
      scope: storage.type.simplec
      push: [pop-2, type|1|main@0]
  type|1|main@0:
    - match: '\b[A-Za-z_]+\b'
      scope: variable.simplec
      set: variable-declaration|0|main@0
    - match: '\S'
      fail: main@0
  type|2|main@0:
    - meta_include_prototype: false
    - match: '\b[A-Za-z_]+\b'
      scope: meta.function.simplec storage.type.simplec
      push: [pop-2, type|3|main@0]
  type|3|main@0:
    - match: '\b[A-Za-z_]+\b'
      scope: meta.function.simplec entity.name.function.simplec
      set: function-definition|0|main@0
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  type|4|block@1:
    - meta_include_prototype: false
    - match: '\b[A-Za-z_]+\b'
      scope: storage.type.simplec
      push: [pop-2, type|5|block@1]
  type|5|block@1:
    - match: '\b[A-Za-z_]+\b'
      scope: variable.simplec
      set: variable-declaration|3
    - match: '\S'
      fail: block@1
  variable-declaration|0|main@0:
    - match: '='
      set: variable-declaration|1
    - match: ';'
      pop: true
    - match: '\S'
      fail: main@0
  variable-declaration|1:
    - match: '[0-9]+'
      scope: constant.numeric.simplec
      set: variable-declaration|2
    - match: '\b[A-Za-z_]+\b'
      scope: meta.function-call.simplec variable.function.simplec meta.path.simplec
      set: [variable-declaration|2, function-call|0]
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  variable-declaration|2:
    - match: ';'
      pop: true
    - match: '\S'
      scope: invalid.illegal.simplec
      pop: true
  variable-declaration|3:
    - match: '='
      set: variable-declaration|1
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

See `sbnf.sbnf` for a full example grammar.

### Headers

Headers are in the form `<head>: <value>`, ending at the end of the line. The
following values are allowed:

* `name`: The name of the syntax. This defaults to the name of the sbnf file.
* `extensions`: A space-separated list of file extensions. Equivalent to
  `file_extensions` in sublime-syntax.
* `first-line`: A regex for matching the first line of a file. Equivalent to
  `first_line_match` in sublime-syntax.
* `scope`: The default scope for the grammar. This defaults to `source.`
  followed by the name lowercased.
* `scope-postfix`: A postfix appended to all scopes in the grammar (excluding
  the `scope` header). This defaults to the name lowercased. Leave empty to
  leave out the postfix.
* `hidden`: Whether the syntax will be shown in the menu in Sublime Text.

Example:

```
name: SBNF
extensions: sbnf
# Don't need this, as this is already the default
# scope: source.sbnf
```

### Rules

Rules are in the form `<identifier> <arguments> = <expression> ;`.
Identifiers may contain any alphanumeric character as well as `-`, `_` and `.`.

Like sublime-syntax files, SBNF grammars have two entry points: `main`,
`prototype`. They behave identically to those in sublime-syntax files. Only
rules used directly or indirectly from an entry point are compiled.

Arguments come in the following form: `{<arg>, <key>: <value>}`. `<arg>`,
`<key>` or `<value>` may contain any text except `,`, `:` or `}`. There may be
any number of arguments given, as allowed by whatever the arguments are for.
When there are no arguments the `{}` are optional.

The following arguments are allowed for rules:

* `<meta-scope>`: The meta-scope of the rule. Equivalent to `meta_scope` or
  `meta_content_scope` in sublime-syntax.
* `include-prototype: <bool>`: Whether to include the `prototype` rule in this
  rule. Equivalent to `meta_include_prototype` in sublime-syntax. `<bool>` must
  be either `true` or `false`. Defaults to `true`.

Expressions may take any of the following forms:

* `` `<literal>` ``: A terminal matching text literally.
* `'<regex>'`: A terminal matching text according to a regex.
* `<identifier>`: A non-terminal matching another rule.
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

Literal and regex terminals are allowed the following arguments:

* `<scope>`: The scope of the terminal.
* `<capture>: <scope>`: The scope for a regex capture group. `<capture>` must be
  an integer.

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