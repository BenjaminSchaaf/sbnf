# SBNF

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
* Additional tests for compilation
* Fix illegal scopes missing language specific scoping
* Prevent popping from main
* Add warnings for when branches are used in non-popping loops.