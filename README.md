# js-optimizer

A lightweight JavaScript minifier written in Java. Zero dependencies, single-method API, comprehensive ES6+ support.

## Usage

```java
String minified = JsMinifier.minify(sourceCode);
```

## Optimizations

The minifier performs 14 optimization passes in a single call:

**Lexical pass** (streaming, single-pass):
- Whitespace and newline removal (ASI-aware)
- Comment stripping (`//` and `/* */`)
- String quote optimization (`"it's"` &rarr; `'it\'s'` when shorter)
- Literal shortening (`true` &rarr; `!0`, `false` &rarr; `!1`, `undefined` &rarr; `void 0`, `Infinity` &rarr; `(1/0)`)
- Decimal number shortening (`0.5` &rarr; `.5`, `1.0` &rarr; `1`)
- Scientific notation conversion (`1000` &rarr; `1e3`)
- Redundant semicolon removal before `}`
- Regex/division disambiguation

**Post-passes** (applied after the lexical pass):
- Consecutive declaration merging (`var a=1;var b=2` &rarr; `var a=1,b=2`)
- String concatenation folding (`"a"+"b"` &rarr; `"ab"`)
- Bracket-to-dot conversion (`obj["prop"]` &rarr; `obj.prop`)
- ES6 object property shorthand (`{x:x}` &rarr; `{x}`)
- Redundant return removal (trailing `return` / `return void 0` in function bodies)
- Arrow body shortening (`()=>{return x}` &rarr; `()=>x`)

## Building

Requires Java 25+ and Maven.

```bash
mvn clean package
```

## Testing

```bash
mvn clean test
```

The test suite contains 1,100+ tests across 34 test classes covering correctness, edge cases, idempotency, cross-pass interactions, and performance.
