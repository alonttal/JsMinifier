package com.jsoptimizer;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JsMinifierTest {

    // ── Null / Empty / Whitespace ────────────────────────────────────────

    @Nested
    class NullAndEmpty {
        @Test
        void nullInput() {
            assertEquals("", JsMinifier.minify(null));
        }

        @Test
        void emptyInput() {
            assertEquals("", JsMinifier.minify(""));
        }

        @Test
        void whitespaceOnly() {
            assertEquals("", JsMinifier.minify("   \t\n  \n  "));
        }

        @Test
        void commentsOnly() {
            assertEquals("", JsMinifier.minify("// comment\n/* block */"));
        }
    }

    // ── Basic Whitespace Removal ─────────────────────────────────────────

    @Nested
    class WhitespaceRemoval {
        @Test
        void collapseSpaces() {
            assertEquals("var x=1;", JsMinifier.minify("var   x  =  1 ;"));
        }

        @Test
        void preserveSpaceBetweenIdentifiers() {
            assertEquals("var x", JsMinifier.minify("var x"));
        }

        @Test
        void noSpaceBetweenOperatorAndIdentifier() {
            assertEquals("x=1", JsMinifier.minify("x = 1"));
        }

        @Test
        void removeLeadingTrailingWhitespace() {
            assertEquals("x=1;", JsMinifier.minify("  x = 1;  "));
        }

        @Test
        void removeNewlines() {
            assertEquals("var x=1,y=2;", JsMinifier.minify("var x = 1;\nvar y = 2;"));
        }

        @Test
        void windowsLineEndings() {
            assertEquals("var x=1,y=2;", JsMinifier.minify("var x = 1;\r\nvar y = 2;"));
        }
    }

    // ── Comment Removal ──────────────────────────────────────────────────

    @Nested
    class CommentRemoval {
        @Test
        void singleLineComment() {
            assertEquals("var x=1;", JsMinifier.minify("var x = 1; // comment"));
        }

        @Test
        void multiLineComment() {
            assertEquals("var x=1;", JsMinifier.minify("var x = 1; /* comment */"));
        }

        @Test
        void multiLineCommentSpanning() {
            assertEquals("var x=1,y=2;",
                    JsMinifier.minify("var x = 1; /* comment\nspanning\nlines */ var y = 2;"));
        }

        @Test
        void consecutiveComments() {
            assertEquals("a;b;",
                    JsMinifier.minify("a; // comment\n// another\nb;"));
        }

        @Test
        void commentInsideString_notRemoved() {
            assertEquals("var x=\"// not a comment\";",
                    JsMinifier.minify("var x = \"// not a comment\";"));
        }

        @Test
        void blockCommentInsideString_notRemoved() {
            assertEquals("var x='/* not a comment */';",
                    JsMinifier.minify("var x = '/* not a comment */';"));
        }

        @Test
        void stringInsideComment_ignored() {
            assertEquals("", JsMinifier.minify("// \"this is not a string\""));
        }

        @Test
        void stringInsideBlockComment_ignored() {
            assertEquals("", JsMinifier.minify("/* \"this is not a string\" */"));
        }
    }

    // ── String Preservation ──────────────────────────────────────────────

    @Nested
    class StringPreservation {
        @Test
        void singleQuoteString() {
            assertEquals("var x='hello world';",
                    JsMinifier.minify("var x = 'hello world';"));
        }

        @Test
        void doubleQuoteString() {
            assertEquals("var x=\"hello world\";",
                    JsMinifier.minify("var x = \"hello world\";"));
        }

        @Test
        void escapedQuotes() {
            assertEquals("var x='he said \"hi\"';",
                    JsMinifier.minify("var x = \"he said \\\"hi\\\"\";"));
        }

        @Test
        void escapedBackslash() {
            assertEquals("var x='a\\\\b';",
                    JsMinifier.minify("var x = 'a\\\\b';"));
        }

        @Test
        void whitespaceInsideString_preserved() {
            assertEquals("var x=\"  spaces  \";",
                    JsMinifier.minify("var x = \"  spaces  \";"));
        }
    }

    // ── Template Literals ────────────────────────────────────────────────

    @Nested
    class TemplateLiterals {
        @Test
        void simpleTemplate() {
            assertEquals("var x=`hello`;",
                    JsMinifier.minify("var x = `hello`;"));
        }

        @Test
        void templateWithExpression() {
            assertEquals("var x=`hello ${name}`;",
                    JsMinifier.minify("var x = `hello ${  name  }`;"));
        }

        @Test
        void templateWithComplexExpression() {
            assertEquals("var x=`val: ${a+b}`;",
                    JsMinifier.minify("var x = `val: ${  a + b  }`;"));
        }

        @Test
        void nestedTemplateLiterals() {
            assertEquals("var x=`a${`b${c}`}d`;",
                    JsMinifier.minify("var x = `a${ `b${ c }` }d`;"));
        }

        @Test
        void templateWithObjectLiteral() {
            assertEquals("var x=`${({a:1})}`;",
                    JsMinifier.minify("var x = `${ ({a: 1}) }`;"));
        }

        @Test
        void templatePreservesWhitespaceInText() {
            assertEquals("var x=`  spaces  `;",
                    JsMinifier.minify("var x = `  spaces  `;"));
        }

        @Test
        void templateWithEscape() {
            assertEquals("var x=`\\${notExpr}`;",
                    JsMinifier.minify("var x = `\\${notExpr}`;"));
        }
    }

    // ── Regex Literals ───────────────────────────────────────────────────

    @Nested
    class RegexLiterals {
        @Test
        void simpleRegex() {
            assertEquals("var x=/abc/;",
                    JsMinifier.minify("var x = /abc/;"));
        }

        @Test
        void regexWithFlags() {
            assertEquals("var x=/abc/gi;",
                    JsMinifier.minify("var x = /abc/gi;"));
        }

        @Test
        void regexWithEscape() {
            assertEquals("var x=/a\\/b/;",
                    JsMinifier.minify("var x = /a\\/b/;"));
        }

        @Test
        void regexWithCharacterClass() {
            assertEquals("var x=/[a-z]/;",
                    JsMinifier.minify("var x = /[a-z]/;"));
        }

        @Test
        void regexWithSlashInCharClass() {
            assertEquals("var x=/[/]/;",
                    JsMinifier.minify("var x = /[/]/;"));
        }

        @Test
        void regexWithEscapedBracketInCharClass() {
            assertEquals("var x=/[\\]]/;",
                    JsMinifier.minify("var x = /[\\]]/;"));
        }

        @Test
        void regexAfterReturn() {
            assertEquals("return/abc/gi;",
                    JsMinifier.minify("return /abc/gi;"));
        }

        @Test
        void divisionAfterIdentifier() {
            assertEquals("a/b", JsMinifier.minify("a / b"));
        }

        @Test
        void divisionAfterNumber() {
            assertEquals("10/2", JsMinifier.minify("10 / 2"));
        }

        @Test
        void divisionAfterCloseParen() {
            assertEquals("(a)/b", JsMinifier.minify("(a) / b"));
        }

        @Test
        void regexAfterEquals() {
            assertEquals("x=/pattern/;",
                    JsMinifier.minify("x = /pattern/;"));
        }

        @Test
        void regexAfterOpenParen() {
            assertEquals("foo(/bar/)",
                    JsMinifier.minify("foo( /bar/ )"));
        }

        @Test
        void regexAfterComma() {
            assertEquals("[/a/,/b/]",
                    JsMinifier.minify("[ /a/, /b/ ]"));
        }

        @Test
        void regexAfterSemicolon() {
            assertEquals("x;/abc/;",
                    JsMinifier.minify("x; /abc/;"));
        }

        @Test
        void regexAfterOpenBrace() {
            assertEquals("{/abc/}",
                    JsMinifier.minify("{ /abc/ }"));
        }
    }

    // ── ASI-Sensitive Cases ──────────────────────────────────────────────

    @Nested
    class AsiSensitive {
        @Test
        void returnWithValue() {
            assertEquals("return\nvalue;",
                    JsMinifier.minify("return\n  value;"));
        }

        @Test
        void returnOnSameLine() {
            assertEquals("return value;",
                    JsMinifier.minify("return value;"));
        }

        @Test
        void throwWithExpression() {
            assertEquals("throw\nnew Error();",
                    JsMinifier.minify("throw\n  new Error();"));
        }

        @Test
        void continueStatement() {
            assertEquals("continue\nlabel;",
                    JsMinifier.minify("continue\n  label;"));
        }

        @Test
        void breakStatement() {
            assertEquals("break\nlabel;",
                    JsMinifier.minify("break\n  label;"));
        }

        @Test
        void postfixIncrementNewline() {
            assertEquals("a++\nb",
                    JsMinifier.minify("a++\nb"));
        }

        @Test
        void prefixIncrementAfterNewline() {
            assertEquals("a\n++b",
                    JsMinifier.minify("a\n++b"));
        }

        @Test
        void returnWithComment() {
            assertEquals("return\nvalue;",
                    JsMinifier.minify("return // comment\n  value;"));
        }

        @Test
        void yieldNewline() {
            assertEquals("yield\nvalue",
                    JsMinifier.minify("yield\n  value"));
        }
    }

    // ── Modern JS Features ───────────────────────────────────────────────

    @Nested
    class ModernJsFeatures {
        @Test
        void arrowFunction() {
            assertEquals("const f=(a,b)=>a+b;",
                    JsMinifier.minify("const f = (a, b) => a + b;"));
        }

        @Test
        void optionalChaining() {
            assertEquals("a?.b?.c",
                    JsMinifier.minify("a?.b?.c"));
        }

        @Test
        void nullishCoalescing() {
            assertEquals("a??b",
                    JsMinifier.minify("a ?? b"));
        }

        @Test
        void asyncAwait() {
            assertEquals("async function f(){await fetch(url)}",
                    JsMinifier.minify("async function f() { await fetch(url); }"));
        }

        @Test
        void classDeclaration() {
            assertEquals("class Foo extends Bar{constructor(){super()}}",
                    JsMinifier.minify("class Foo extends Bar {\n  constructor() {\n    super();\n  }\n}"));
        }

        @Test
        void destructuring() {
            assertEquals("const{a,b}=obj,[c,d]=arr;",
                    JsMinifier.minify("const { a, b } = obj; const [c, d] = arr;"));
        }

        @Test
        void spreadRest() {
            assertEquals("const arr=[...a,...b];",
                    JsMinifier.minify("const arr = [...a, ...b];"));
        }

        @Test
        void generatorFunction() {
            assertEquals("function*gen(){yield 1;yield 2}",
                    JsMinifier.minify("function* gen() {\n  yield 1;\n  yield 2;\n}"));
        }

        @Test
        void importExport() {
            assertEquals("import{foo}from'bar';export default baz;",
                    JsMinifier.minify("import { foo } from 'bar'; export default baz;"));
        }

        @Test
        void privateFields() {
            assertEquals("class A{#x=1;get x(){return this.#x}}",
                    JsMinifier.minify("class A {\n  #x = 1;\n  get x() { return this.#x; }\n}"));
        }

        @Test
        void logicalAssignment() {
            assertEquals("a??=b;a||=c;a&&=d;",
                    JsMinifier.minify("a ??= b; a ||= c; a &&= d;"));
        }

        @Test
        void numericSeparators() {
            assertEquals("var x=1e6;",
                    JsMinifier.minify("var x = 1_000_000;"));
        }
    }

    // ── Function Minification ────────────────────────────────────────────

    @Nested
    class FunctionMinification {
        @Test
        void simpleFunction() {
            assertEquals("function foo(a,b){return a+b}",
                    JsMinifier.minify("function foo(a, b) {\n  return a + b;\n}"));
        }

        @Test
        void iife() {
            assertEquals("(function(){var x=1})();",
                    JsMinifier.minify("(function() {\n  var x = 1;\n})();"));
        }

        @Test
        void nestedFunctions() {
            assertEquals("function outer(){function inner(){return 1}return inner()}",
                    JsMinifier.minify(
                            "function outer() {\n  function inner() {\n    return 1;\n  }\n  return inner();\n}"));
        }
    }

    // ── Idempotency ──────────────────────────────────────────────────────

    @Nested
    class Idempotency {
        @Test
        void alreadyMinified() {
            String minified = "var x=1,y=2;function f(a,b){return a+b}";
            assertEquals(minified, JsMinifier.minify(minified));
        }

        @Test
        void doubleMinify() {
            String input = "var  x =  1;\n// comment\nvar y = 2;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }
    }

    // ── Unicode ──────────────────────────────────────────────────────────

    @Nested
    class Unicode {
        @Test
        void unicodeIdentifiers() {
            assertEquals("var café=1;",
                    JsMinifier.minify("var café = 1;"));
        }

        @Test
        void unicodeInString() {
            assertEquals("var x=\"日本語\";",
                    JsMinifier.minify("var x = \"日本語\";"));
        }
    }

    // ── Operator Merging Prevention ──────────────────────────────────────

    @Nested
    class OperatorMergingPrevention {
        @Test
        void plusPlusMustNotMerge() {
            // a + +b must not become a++b (changes meaning entirely)
            assertEquals("a+ +b", JsMinifier.minify("a + +b"));
        }

        @Test
        void minusMinusMustNotMerge() {
            // a - -b must not become a--b
            assertEquals("a- -b", JsMinifier.minify("a - -b"));
        }

        @Test
        void plusBeforeIncrement() {
            // a + ++b must not become a+++b (ambiguous)
            assertEquals("a+ ++b", JsMinifier.minify("a + ++b"));
        }

        @Test
        void minusBeforeDecrement() {
            // a - --b must not become a---b (ambiguous)
            assertEquals("a- --b", JsMinifier.minify("a - --b"));
        }

        @Test
        void divisionThenRegex() {
            // a / /regex/ must not become a//regex/ (creates comment)
            assertEquals("a/ /regex/", JsMinifier.minify("a / /regex/"));
        }

        @Test
        void divisionThenMultiply() {
            // Prevent / * from becoming /* (comment start)
            // a / *b is unusual but syntactically: division then deref... not valid JS
            // but the minifier shouldn't create comment syntax
            String result = JsMinifier.minify("a / *b");
            assertFalse(result.contains("/*"), "Created block comment syntax: " + result);
        }

        @Test
        void plusPlusAcrossNewline() {
            // a +\n+b: newline between + operators
            assertEquals("a+ +b", JsMinifier.minify("a +\n+b"));
        }

        @Test
        void minusMinusAcrossNewline() {
            assertEquals("a- -b", JsMinifier.minify("a -\n-b"));
        }
    }

    // ── Comment Edge Cases ──────────────────────────────────────────────

    @Nested
    class CommentEdgeCases {
        @Test
        void emptyBlockComment() {
            assertEquals("a;b;", JsMinifier.minify("a; /**/ b;"));
        }

        @Test
        void singleLineCommentAtEofNoNewline() {
            assertEquals("var x=1;", JsMinifier.minify("var x = 1; // comment"));
        }

        @Test
        void blockCommentAtEof() {
            assertEquals("var x=1;", JsMinifier.minify("var x = 1; /* comment */"));
        }

        @Test
        void commentBetweenIdentifiers() {
            // var/* comment */x should become var x (not varx)
            assertEquals("var x", JsMinifier.minify("var/* comment */x"));
        }

        @Test
        void commentBetweenSameOperators() {
            // a +/* comment */+ b should become a+ +b (not a++b)
            assertEquals("a+ +b", JsMinifier.minify("a +/* comment */+ b"));
        }

        @Test
        void blockCommentWithNewlineBetweenOperators() {
            // Comment with newline between + and + should still prevent merging
            assertEquals("a+ +b", JsMinifier.minify("a +/* \n */+ b"));
        }

        @Test
        void singleLineCommentBetweenSameOperators() {
            // a +// comment\n+ b should become a+ +b (not a++b)
            assertEquals("a+ +b", JsMinifier.minify("a +// comment\n+ b"));
        }

        @Test
        void multiLineCommentWithNewlineTrigersAsi() {
            // return /* \n */ value is like return\nvalue — ASI applies
            assertEquals("return\nvalue", JsMinifier.minify("return /* \n */ value"));
        }

        @Test
        void multiLineCommentWithoutNewlineNoAsi() {
            // return /* comment */ value — no newline in comment, no ASI
            assertEquals("return value", JsMinifier.minify("return /* comment */ value"));
        }

        @Test
        void divisionSlashAfterBlockComment() {
            // a /* comment */ / b — after comment, '/' comes, prev output is 'a'
            // since 'a' is identifier, '/' is division
            assertEquals("a/b", JsMinifier.minify("a /* comment */ / b"));
        }
    }

    // ── Identifier Spacing Across Newlines ───────────────────────────────

    @Nested
    class IdentifierSpacingAcrossNewlines {
        @Test
        void keywordAndIdentAcrossNewline() {
            // var\n  x = 1; — must keep space between var and x
            assertEquals("var x=1;", JsMinifier.minify("var\n  x = 1;"));
        }

        @Test
        void functionNameAcrossNewline() {
            assertEquals("function foo(){}", JsMinifier.minify("function\nfoo() {}"));
        }

        @Test
        void twoIdentifiersAcrossNewline() {
            // instanceof\nFoo — must keep space
            assertEquals("a instanceof Foo", JsMinifier.minify("a instanceof\nFoo"));
        }

        @Test
        void identAfterNumberAcrossNewline() {
            // A number followed by an identifier across a newline — need space
            // (e.g., in property access: 1\n.toString() is different but 1\nin is "1 in")
            assertEquals("x in y", JsMinifier.minify("x\nin\ny"));
        }
    }

    // ── Division Assignment /= ──────────────────────────────────────────

    @Nested
    class DivisionAssignment {
        @Test
        void divisionAssignment() {
            assertEquals("x/=2;", JsMinifier.minify("x /= 2;"));
        }

        @Test
        void divisionAssignmentNoSpaces() {
            assertEquals("x/=2;", JsMinifier.minify("x/=2;"));
        }
    }

    // ── Regex in More Contexts ──────────────────────────────────────────

    @Nested
    class RegexInMoreContexts {
        @Test
        void regexAfterBang() {
            assertEquals("!/pattern/.test(x)", JsMinifier.minify("! /pattern/.test(x)"));
        }

        @Test
        void regexAfterTypeof() {
            assertEquals("typeof/regex/", JsMinifier.minify("typeof /regex/"));
        }

        @Test
        void regexAfterVoid() {
            assertEquals("void/regex/", JsMinifier.minify("void /regex/"));
        }

        @Test
        void regexAfterDelete() {
            assertEquals("delete/regex/", JsMinifier.minify("delete /regex/"));
        }

        @Test
        void regexAfterColon() {
            assertEquals("{a:/regex/}", JsMinifier.minify("{ a: /regex/ }"));
        }

        @Test
        void regexAfterTernaryQuestion() {
            assertEquals("x?/a/:/b/", JsMinifier.minify("x ? /a/ : /b/"));
        }

        @Test
        void regexAfterNew() {
            // new /regex/ isn't idiomatic, but 'new' is in REGEX_PRECEDING_KEYWORDS
            assertEquals("new/regex/", JsMinifier.minify("new /regex/"));
        }

        @Test
        void divisionAfterCloseSquareBracket() {
            assertEquals("a[0]/b", JsMinifier.minify("a[0] / b"));
        }

        @Test
        void regexAfterCase() {
            assertEquals("case/pattern/:break;",
                    JsMinifier.minify("case /pattern/: break;"));
        }

        @Test
        void regexAfterIn() {
            // 'in' is in REGEX_PRECEDING_KEYWORDS
            assertEquals("x in/regex/", JsMinifier.minify("x in /regex/"));
        }

        @Test
        void regexAfterInstanceof() {
            assertEquals("x instanceof/regex/", JsMinifier.minify("x instanceof /regex/"));
        }
    }

    // ── Number Edge Cases ───────────────────────────────────────────────

    @Nested
    class NumberEdgeCases {
        @Test
        void hexNumber() {
            assertEquals("var x=0xFF;", JsMinifier.minify("var x = 0xFF;"));
        }

        @Test
        void octalNumber() {
            assertEquals("var x=0o77;", JsMinifier.minify("var x = 0o77;"));
        }

        @Test
        void binaryNumber() {
            assertEquals("var x=0b1010;", JsMinifier.minify("var x = 0b1010;"));
        }

        @Test
        void bigInt() {
            assertEquals("var x=123n;", JsMinifier.minify("var x = 123n;"));
        }

        @Test
        void floatNumber() {
            assertEquals("var x=1.5;", JsMinifier.minify("var x = 1.5;"));
        }

        @Test
        void scientificNotation() {
            assertEquals("var x=1e10;", JsMinifier.minify("var x = 1e10;"));
        }

        @Test
        void scientificNotationWithSign() {
            assertEquals("var x=1e+10;", JsMinifier.minify("var x = 1e+10;"));
        }

        @Test
        void doubleDotPropertyAccess() {
            // 1..toString() — number 1. then .toString()
            // The number parser should not consume the second dot
            assertEquals("1..toString()", JsMinifier.minify("1..toString()"));
        }

        @Test
        void numberThenDotPropertyAccess() {
            // 1.0.toString() — number 1.0 then .toString(), shortened to 1..toString()
            assertEquals("1..toString()", JsMinifier.minify("1.0.toString()"));
        }

        @Test
        void leadingDotNumber() {
            assertEquals("var x=.5;", JsMinifier.minify("var x = .5;"));
        }
    }

    // ── Numeric Literal Shortening ─────────────────────────────────────

    @Nested
    class NumericLiteralShortening {

        // ── Leading zero removal ────────────────────────────────────────

        @Test
        void leadingZero() {
            assertEquals(".5", JsMinifier.minify("0.5"));
        }

        @Test
        void leadingZeroWithTrailingZero() {
            assertEquals(".5", JsMinifier.minify("0.50"));
        }

        // ── Trailing zero removal ───────────────────────────────────────

        @Test
        void trailingZero() {
            assertEquals("1.5", JsMinifier.minify("1.50"));
        }

        @Test
        void multipleTrailingZeros() {
            assertEquals("1.5", JsMinifier.minify("1.500"));
        }

        // ── Trailing dot+zeros (remove dot entirely) ───────────────────

        @Test
        void trailingDotZero() {
            assertEquals("1", JsMinifier.minify("1.0"));
        }

        @Test
        void trailingDotZeros() {
            assertEquals("100", JsMinifier.minify("100.00"));
        }

        @Test
        void zeroPointZero() {
            assertEquals("0", JsMinifier.minify("0.0"));
        }

        // ── Property access safety ──────────────────────────────────────

        @Test
        void propertyAccessKeepsDot() {
            assertEquals("1..toString()", JsMinifier.minify("1.0.toString()"));
        }

        @Test
        void zeroPropertyAccessKeepsDot() {
            assertEquals("0..toString()", JsMinifier.minify("0.0.toString()"));
        }

        @Test
        void leadingZeroPropertyAccess() {
            assertEquals(".5.toFixed()", JsMinifier.minify("0.5.toFixed()"));
        }

        // ── Exponents ───────────────────────────────────────────────────

        @Test
        void exponentTrailingZero() {
            assertEquals("1e5", JsMinifier.minify("1.0e5"));
        }

        @Test
        void exponentLeadingZero() {
            assertEquals(".5e2", JsMinifier.minify("0.5e2"));
        }

        @Test
        void exponentTrailingZeros() {
            assertEquals("1.5e3", JsMinifier.minify("1.50e3"));
        }

        @Test
        void exponentPropertyAccess() {
            assertEquals("1.e5.toString()", JsMinifier.minify("1.0e5.toString()"));
        }

        // ── Special formats NOT shortened ───────────────────────────────

        @Test
        void hexUnchanged() {
            assertEquals("0xFF", JsMinifier.minify("0xFF"));
        }

        @Test
        void octalUnchanged() {
            assertEquals("0o77", JsMinifier.minify("0o77"));
        }

        @Test
        void binaryUnchanged() {
            assertEquals("0b1010", JsMinifier.minify("0b1010"));
        }

        @Test
        void bigIntUnchanged() {
            assertEquals("123n", JsMinifier.minify("123n"));
        }

        // ── Numeric separators ──────────────────────────────────────────

        @Test
        void separatorInIntegerPartDotZero() {
            assertEquals("1e3", JsMinifier.minify("1_000.0"));
        }

        @Test
        void separatorInFractionalPart() {
            assertEquals("1.5", JsMinifier.minify("1.5_0"));
        }

        // ── Regression: already double-dot ──────────────────────────────

        @Test
        void doubleDotUnchanged() {
            assertEquals("1..toString()", JsMinifier.minify("1..toString()"));
        }

        // ── Idempotency ────────────────────────────────────────────────

        @Test
        void leadingDotIdempotent() {
            assertEquals(".5", JsMinifier.minify(".5"));
        }

        @Test
        void doubleMinifyStability() {
            String input = "var x = 1.0; var y = 0.5; var z = 1.50;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        // ── Already minimal ────────────────────────────────────────────

        @Test
        void alreadyMinimalFloat() {
            assertEquals("1.5", JsMinifier.minify("1.5"));
        }

        @Test
        void alreadyMinimalInteger() {
            assertEquals("42", JsMinifier.minify("42"));
        }

        // ── Exponent with signs ─────────────────────────────────────────

        @Test
        void exponentPlusSignTrailingZero() {
            assertEquals("1e+5", JsMinifier.minify("1.0e+5"));
        }

        @Test
        void exponentMinusSignTrailingZero() {
            assertEquals("1e-5", JsMinifier.minify("1.0e-5"));
        }

        @Test
        void exponentPlusSignLeadingZero() {
            assertEquals(".5e+2", JsMinifier.minify("0.5e+2"));
        }

        @Test
        void exponentMinusSignLeadingZero() {
            assertEquals(".5e-2", JsMinifier.minify("0.5e-2"));
        }

        @Test
        void exponentPlusSignTrailingZeros() {
            assertEquals("1.5e+3", JsMinifier.minify("1.50e+3"));
        }

        @Test
        void exponentMinusSignTrailingZeros() {
            assertEquals("1.5e-3", JsMinifier.minify("1.50e-3"));
        }

        // ── Uppercase exponent ──────────────────────────────────────────

        @Test
        void uppercaseExponentTrailingZero() {
            assertEquals("1E5", JsMinifier.minify("1.0E5"));
        }

        @Test
        void uppercaseExponentLeadingZero() {
            assertEquals(".5E2", JsMinifier.minify("0.5E2"));
        }

        @Test
        void uppercaseExponentPlusSign() {
            assertEquals("1E+5", JsMinifier.minify("1.0E+5"));
        }

        // ── Zero with exponent ──────────────────────────────────────────

        @Test
        void zeroWithExponent() {
            assertEquals("0e5", JsMinifier.minify("0.0e5"));
        }

        @Test
        void zeroWithExponentPropertyAccess() {
            assertEquals("0.e5.toString()", JsMinifier.minify("0.0e5.toString()"));
        }

        @Test
        void zeroWithExponentAndSign() {
            assertEquals("0e-3", JsMinifier.minify("0.0e-3"));
        }

        // ── Multiple zeros in fractional part ───────────────────────────

        @Test
        void doubleTrailingZeros() {
            assertEquals("0", JsMinifier.minify("0.00"));
        }

        @Test
        void tripleTrailingZeros() {
            assertEquals("0", JsMinifier.minify("0.000"));
        }

        @Test
        void trailingZeroAfterNonZeroDigit() {
            assertEquals(".1", JsMinifier.minify("0.10"));
        }

        @Test
        void leadingZerosInFraction() {
            assertEquals(".01", JsMinifier.minify("0.010"));
        }

        @Test
        void multipleTrailingZerosAfterNonZero() {
            assertEquals(".1", JsMinifier.minify("0.100"));
        }

        @Test
        void mixedFractionalDigitsTrailingZero() {
            assertEquals("1.23", JsMinifier.minify("1.230"));
        }

        @Test
        void manyTrailingZeros() {
            assertEquals("1.5", JsMinifier.minify("1.50000"));
        }

        // ── Dot then exponent (valid input) ─────────────────────────────

        @Test
        void dotThenExponent() {
            // 1.e5 is valid JS (= 1e5), should be shortened
            assertEquals("1e5", JsMinifier.minify("1.e5"));
        }

        @Test
        void dotThenExponentPropertyAccess() {
            // 1.e5.toString() — already minimal, dot needed for property access
            assertEquals("1.e5.toString()", JsMinifier.minify("1.e5.toString()"));
        }

        // ── Numbers in expression contexts ──────────────────────────────

        @Test
        void arrayOfShortenedNumbers() {
            assertEquals("[.5,1,1.5]", JsMinifier.minify("[0.5, 1.0, 1.50]"));
        }

        @Test
        void additionOfShortenedNumbers() {
            assertEquals(".5+1", JsMinifier.minify("0.5 + 1.0"));
        }

        @Test
        void multipleOperands() {
            assertEquals(".5+1-0*1.5", JsMinifier.minify("0.5 + 1.0 - 0.0 * 1.50"));
        }

        @Test
        void comparison() {
            assertEquals("x===1", JsMinifier.minify("x === 1.0"));
        }

        @Test
        void bothSidesComparison() {
            assertEquals("1===1", JsMinifier.minify("1.0 === 1.0"));
        }

        @Test
        void ternaryWithNumbers() {
            assertEquals("a?1:.5", JsMinifier.minify("a ? 1.0 : 0.5"));
        }

        // ── Numbers in statement contexts ───────────────────────────────

        @Test
        void varDeclarationMultiple() {
            assertEquals("var x=1,y=.5;", JsMinifier.minify("var x = 1.0, y = 0.5;"));
        }

        @Test
        void returnShortenedInteger() {
            assertEquals("return 1;", JsMinifier.minify("return 1.0;"));
        }

        @Test
        void returnShortenedLeadingDot() {
            assertEquals("return .5;", JsMinifier.minify("return 0.5;"));
        }

        @Test
        void forLoopWithShortenedNumbers() {
            assertEquals("for(var i=0;i<1;i+=.5){}",
                    JsMinifier.minify("for (var i = 0.0; i < 1.0; i += 0.5) {}"));
        }

        @Test
        void switchCaseWithShortenedNumber() {
            assertEquals("switch(x){case 1:break}",
                    JsMinifier.minify("switch (x) { case 1.0: break; }"));
        }

        // ── Numbers followed by different tokens ────────────────────────

        @Test
        void followedBySemicolon() {
            assertEquals("1;", JsMinifier.minify("1.0;"));
        }

        @Test
        void followedByCloseParen() {
            assertEquals("(1)", JsMinifier.minify("(1.0)"));
        }

        @Test
        void followedByCloseBracket() {
            assertEquals("a[1]", JsMinifier.minify("a[1.0]"));
        }

        @Test
        void followedByComma() {
            assertEquals("f(1,2)", JsMinifier.minify("f(1.0, 2.0)"));
        }

        @Test
        void followedByPlus() {
            assertEquals("1+2", JsMinifier.minify("1.0 + 2"));
        }

        @Test
        void followedByMinus() {
            assertEquals("1-2", JsMinifier.minify("1.0 - 2"));
        }

        @Test
        void followedByMultiply() {
            assertEquals("1*2", JsMinifier.minify("1.0 * 2"));
        }

        @Test
        void followedByDivide() {
            assertEquals("1/2", JsMinifier.minify("1.0 / 2"));
        }

        @Test
        void followedByCloseBrace() {
            assertEquals("{x=1}", JsMinifier.minify("{ x = 1.0; }"));
        }

        @Test
        void atEndOfInput() {
            assertEquals("1", JsMinifier.minify("1.0"));
        }

        @Test
        void leadingZeroAtEndOfInput() {
            assertEquals(".5", JsMinifier.minify("0.5"));
        }

        // ── Unary operators before number ───────────────────────────────

        @Test
        void negativeLeadingZero() {
            assertEquals("-.5", JsMinifier.minify("-0.5"));
        }

        @Test
        void negativeTrailingZero() {
            assertEquals("-1", JsMinifier.minify("-1.0"));
        }

        @Test
        void unaryPlusLeadingZero() {
            assertEquals("+.5", JsMinifier.minify("+0.5"));
        }

        @Test
        void bitwiseNotLeadingZero() {
            assertEquals("~.5", JsMinifier.minify("~0.5"));
        }

        @Test
        void logicalNotLeadingZero() {
            assertEquals("!.5", JsMinifier.minify("!0.5"));
        }

        // ── Property access chains ──────────────────────────────────────

        @Test
        void propertyAccessChain() {
            assertEquals("1..toString().length", JsMinifier.minify("1.0.toString().length"));
        }

        @Test
        void propertyAccessWithArgument() {
            assertEquals("1..toFixed(2)", JsMinifier.minify("1.0.toFixed(2)"));
        }

        @Test
        void propertyAccessWithShortenedArgument() {
            assertEquals("1..toFixed(0)", JsMinifier.minify("1.0.toFixed(0.0)"));
        }

        @Test
        void leadingZeroPropertyAccessChain() {
            assertEquals(".5.toFixed(2).length", JsMinifier.minify("0.5.toFixed(2).length"));
        }

        @Test
        void multipleZerosPropertyAccess() {
            assertEquals("0..valueOf()", JsMinifier.minify("0.00.valueOf()"));
        }

        // ── Interaction with literal shortening ─────────────────────────

        @Test
        void truePlusShortened() {
            assertEquals("!0+1", JsMinifier.minify("true + 1.0"));
        }

        @Test
        void undefinedComparedToShortened() {
            assertEquals("void 0===.5", JsMinifier.minify("undefined === 0.5"));
        }

        @Test
        void falsePlusShortenedFloat() {
            assertEquals("!1+.5", JsMinifier.minify("false + 0.5"));
        }

        // ── Template expression ─────────────────────────────────────────

        @Test
        void templateWithShortenedNumber() {
            assertEquals("`${.5}`", JsMinifier.minify("`${0.5}`"));
        }

        @Test
        void templateWithShortenedInteger() {
            assertEquals("`${1}`", JsMinifier.minify("`${1.0}`"));
        }

        @Test
        void templateExpressionMultipleNumbers() {
            assertEquals("`${.5+1}`", JsMinifier.minify("`${0.5 + 1.0}`"));
        }

        // ── Numeric separators (more cases) ─────────────────────────────

        @Test
        void separatorInFractionalAllZeros() {
            assertEquals("1", JsMinifier.minify("1.0_0"));
        }

        @Test
        void separatorInFractionalMultipleZeros() {
            assertEquals("1e3", JsMinifier.minify("1_000.0_0_0"));
        }

        @Test
        void separatorBeforeTrailingZeros() {
            assertEquals("1.2_3", JsMinifier.minify("1.2_3_0_0"));
        }

        @Test
        void separatorWithExponent() {
            assertEquals("1_000e5", JsMinifier.minify("1_000.0e5"));
        }

        // ── Large / unusual numbers ─────────────────────────────────────

        @Test
        void largeNumberTrailingZeros() {
            assertEquals("123456.789", JsMinifier.minify("123456.789000"));
        }

        @Test
        void largeIntegerDotZero() {
            assertEquals("999999", JsMinifier.minify("999999.0"));
        }

        @Test
        void smallFraction() {
            assertEquals(".000001", JsMinifier.minify("0.000001"));
        }

        @Test
        void manyDecimalPlaces() {
            assertEquals("3.14159", JsMinifier.minify("3.141590"));
        }

        // ── Already-shortened idempotency ───────────────────────────────

        @Test
        void alreadyShortenedExponent() {
            assertEquals("1e5", JsMinifier.minify("1e5"));
        }

        @Test
        void plainZeroUnchanged() {
            assertEquals("0", JsMinifier.minify("0"));
        }

        @Test
        void leadingDotWithMultipleDigitsIdempotent() {
            assertEquals(".123", JsMinifier.minify(".123"));
        }

        @Test
        void alreadyMinimalMultiDigitFloat() {
            assertEquals("1.23", JsMinifier.minify("1.23"));
        }

        @Test
        void doubleDotPropertyIdempotent() {
            // Already minimal: 1..toString() must not be changed
            String input = "1..toString()";
            assertEquals(input, JsMinifier.minify(input));
        }

        @Test
        void doubleMinifyAllTransforms() {
            String input = "var a=1.0, b=0.5, c=1.50, d=0.0, e=100.00, f=1.0e5, g=0.5e2;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyPropertyAccess() {
            String input = "1.0.toString() + 0.0.valueOf() + 0.5.toFixed()";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyExponents() {
            String input = "var a = 1.0e5, b = 0.5e2, c = 1.50e+3, d = 0.0e-1;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        // ── Semicolon-before-brace interaction ──────────────────────────

        @Test
        void shortenedNumberBeforeBrace() {
            assertEquals("function f(){return 1}",
                    JsMinifier.minify("function f() { return 1.0; }"));
        }

        @Test
        void shortenedLeadingZeroBeforeBrace() {
            assertEquals("function f(){return .5}",
                    JsMinifier.minify("function f() { return 0.5; }"));
        }

        // ── ASI interaction ─────────────────────────────────────────────

        @Test
        void returnNewlineShortenedNumber() {
            assertEquals("return\n1", JsMinifier.minify("return\n1.0"));
        }

        @Test
        void returnNewlineLeadingDot() {
            assertEquals("return\n.5", JsMinifier.minify("return\n0.5"));
        }

        // ── Multiple numbers in same expression ─────────────────────────

        @Test
        void objectLiteral() {
            assertEquals("({a:1,b:.5,c:0})",
                    JsMinifier.minify("({a: 1.0, b: 0.5, c: 0.0})"));
        }

        @Test
        void nestedArrays() {
            assertEquals("[[1,.5],[0,1.5]]",
                    JsMinifier.minify("[[1.0, 0.5], [0.0, 1.50]]"));
        }

        // ── Scientific notation for large integers ────────────────────────

        @Test
        void scientificBasic1000() {
            assertEquals("1e3", JsMinifier.minify("1000"));
        }

        @Test
        void scientificBasic10000() {
            assertEquals("1e4", JsMinifier.minify("10000"));
        }

        @Test
        void scientificBasic1000000() {
            assertEquals("1e6", JsMinifier.minify("1000000"));
        }

        @Test
        void scientificMultiDigitPrefix2000() {
            assertEquals("2e3", JsMinifier.minify("2000"));
        }

        @Test
        void scientificMultiDigitPrefix15000() {
            assertEquals("15e3", JsMinifier.minify("15000"));
        }

        @Test
        void scientificMultiDigitPrefix123000() {
            assertEquals("123e3", JsMinifier.minify("123000"));
        }

        @Test
        void scientificNotShortened100() {
            // 100 → "1e2" is same length (3 chars), not shorter
            assertEquals("100", JsMinifier.minify("100"));
        }

        @Test
        void scientificNotShortened200() {
            assertEquals("200", JsMinifier.minify("200"));
        }

        @Test
        void scientificNotShortened10() {
            // 10 → "1e1" is same length (2 vs 3), would be longer
            assertEquals("10", JsMinifier.minify("10"));
        }

        @Test
        void scientificNotShortened42() {
            assertEquals("42", JsMinifier.minify("42"));
        }

        @Test
        void scientificNotShortenedZero() {
            assertEquals("0", JsMinifier.minify("0"));
        }

        @Test
        void scientificAfterDotRemoval1000Point0() {
            assertEquals("1e3", JsMinifier.minify("1000.0"));
        }

        @Test
        void scientificAfterDotRemoval2000Point00() {
            assertEquals("2e3", JsMinifier.minify("2000.00"));
        }

        @Test
        void scientificTrailingDotPropertyAccess() {
            assertEquals("1e3.toString()", JsMinifier.minify("1000.0.toString()"));
        }

        @Test
        void scientificTrailingDotPropertyAccess100() {
            assertEquals("1e2.valueOf()", JsMinifier.minify("100.0.valueOf()"));
        }

        @Test
        void scientificWithSeparators1000000() {
            assertEquals("1e6", JsMinifier.minify("1_000_000"));
        }

        @Test
        void scientificWithSeparators10000() {
            assertEquals("1e4", JsMinifier.minify("10_000"));
        }

        @Test
        void scientificWithSeparators1000() {
            assertEquals("1e3", JsMinifier.minify("1_000"));
        }

        @Test
        void scientificHexNotConverted() {
            assertEquals("0xff", JsMinifier.minify("0xff"));
        }

        @Test
        void scientificOctalNotConverted() {
            assertEquals("0o777", JsMinifier.minify("0o777"));
        }

        @Test
        void scientificBinaryNotConverted() {
            assertEquals("0b1010", JsMinifier.minify("0b1010"));
        }

        @Test
        void scientificBigIntNotConverted() {
            assertEquals("1000n", JsMinifier.minify("1000n"));
        }

        @Test
        void scientificAlreadyHasExponent() {
            assertEquals("1e5", JsMinifier.minify("1e5"));
        }

        @Test
        void scientificUnaryMinus() {
            assertEquals("-1e3", JsMinifier.minify("-1000"));
        }

        @Test
        void scientificInArray() {
            assertEquals("[1e3,2e3]", JsMinifier.minify("[1000, 2000]"));
        }

        @Test
        void scientificInVarDecl() {
            assertEquals("var x=1e6", JsMinifier.minify("var x = 1000000"));
        }

        @Test
        void scientificIdempotent() {
            assertEquals("1e3", JsMinifier.minify("1e3"));
        }

        @Test
        void scientificDoubleMinifyStable() {
            String once = JsMinifier.minify("1000000");
            assertEquals("1e6", once);
            assertEquals("1e6", JsMinifier.minify(once));
        }

        @Test
        void scientificWithBooleanCoercion() {
            assertEquals("!0+1e3", JsMinifier.minify("true + 1000"));
        }

        // ── Double-dot property access on plain integers ─────────────────

        @Test
        void doubleDotScientificPropertyAccess() {
            // 1000..toString() — emitNumber sees ".." and stops before consuming
            // the dot, yielding token "1000". shortenToScientific converts to 1e3.
            // The first dot was a trailing decimal dot; after scientific notation
            // it's redundant, so output should be 1e3.toString() (single dot).
            assertEquals("1e3.toString()", JsMinifier.minify("1000..toString()"));
        }

        @Test
        void doubleDotScientificPropertyAccessChain() {
            assertEquals("1e3.toString().length", JsMinifier.minify("1000..toString().length"));
        }

        @Test
        void doubleDotNoConversion() {
            // 100..toString() — 1e2 is same length (3 chars), no conversion
            // keeps double-dot as-is
            assertEquals("100..toString()", JsMinifier.minify("100..toString()"));
        }

        // ── Breakeven boundaries ─────────────────────────────────────────

        @Test
        void scientificBreakeven500() {
            // 500 → "5e2" is same 3 chars, not shorter
            assertEquals("500", JsMinifier.minify("500"));
        }

        @Test
        void scientificBreakeven5000() {
            // 5000 → "5e3" saves 1 char
            assertEquals("5e3", JsMinifier.minify("5000"));
        }

        @Test
        void scientificBreakeven12300() {
            // 12300 → "123e2" is same 5 chars, not shorter
            assertEquals("12300", JsMinifier.minify("12300"));
        }

        @Test
        void scientificBreakeven1230000() {
            // 1230000 (7 chars) → "123e4" (5 chars), saves 2
            assertEquals("123e4", JsMinifier.minify("1230000"));
        }

        // ── Very large numbers ───────────────────────────────────────────

        @Test
        void scientificDoubleDigitExponent() {
            assertEquals("1e10", JsMinifier.minify("10000000000"));
        }

        @Test
        void scientificLargePrefix() {
            assertEquals("9e12", JsMinifier.minify("9000000000000"));
        }

        // ── Property access on non-convertible ──────────────────────────

        @Test
        void propertyAccessNonConvertible() {
            // 10 → 1e1 would be longer (3 > 2), keeps 10.
            assertEquals("10..toString()", JsMinifier.minify("10.0.toString()"));
        }

        // ── Zero edge cases ─────────────────────────────────────────────

        @Test
        void zeroDoubleDotPropertyAccess() {
            // 0.0.toString() → 0..toString() (all zeros, can't convert to scientific)
            assertEquals("0..toString()", JsMinifier.minify("0.0.toString()"));
        }

        // ── Fractional parts prevent conversion ─────────────────────────

        @Test
        void fractionalPartSkipped() {
            // Has real fractional part → can't convert
            assertEquals("2000.5", JsMinifier.minify("2000.5"));
        }

        @Test
        void fractionalZerosStrippedThenConverted() {
            // 20000.0 → strip zeros → 20000 → 2e4
            assertEquals("2e4", JsMinifier.minify("20000.0"));
        }

        // ── Expression contexts ──────────────────────────────────────────

        @Test
        void scientificInTernary() {
            assertEquals("x?1e3:2e3", JsMinifier.minify("x ? 1000 : 2000"));
        }

        @Test
        void scientificInFunctionCall() {
            assertEquals("f(1e3,2e3,3e3)", JsMinifier.minify("f(1000, 2000, 3000)"));
        }

        @Test
        void scientificInBracketAccess() {
            assertEquals("a[1e3]", JsMinifier.minify("a[1000]"));
        }

        @Test
        void scientificInStringConcat() {
            assertEquals("\"\"+1e3", JsMinifier.minify("\"\" + 1000"));
        }

        @Test
        void scientificInReturn() {
            assertEquals("return 1e3", JsMinifier.minify("return 1000"));
        }

        @Test
        void scientificInCase() {
            assertEquals("case 1e3:", JsMinifier.minify("case 1000:"));
        }

        // ── Separator edge cases ─────────────────────────────────────────

        @Test
        void separatorUnusualPlacement() {
            // 1_0_0_0 → separators stripped → 1000 → 1e3
            assertEquals("1e3", JsMinifier.minify("1_0_0_0"));
        }

        @Test
        void separatorInMiddleOfZeros() {
            assertEquals("1e3", JsMinifier.minify("10_00"));
        }

        // ── Number followed by identifier starting with 'e' ─────────────

        @Test
        void scientificBeforeSemicolonAndEval() {
            // Semicolon separates number from eval, no ambiguity
            assertEquals("1e3;eval()", JsMinifier.minify("1000; eval()"));
        }

        // ── Multiple scientific in one statement ─────────────────────────

        @Test
        void multipleScientificInVarDecl() {
            assertEquals("var a=1e3,b=2e6,c=300",
                    JsMinifier.minify("var a = 1000, b = 2000000, c = 300"));
        }
    }

    // ── Line Ending Edge Cases ──────────────────────────────────────────

    @Nested
    class LineEndingEdgeCases {
        @Test
        void carriageReturnOnly() {
            // Old Mac-style \r only line endings
            assertEquals("var x=1,y=2;", JsMinifier.minify("var x = 1;\rvar y = 2;"));
        }

        @Test
        void mixedLineEndings() {
            assertEquals("a;b;c;", JsMinifier.minify("a;\nb;\r\nc;"));
        }
    }

    // ── String Edge Cases ───────────────────────────────────────────────

    @Nested
    class StringEdgeCases {
        @Test
        void emptyDoubleQuoteString() {
            assertEquals("var x=\"\";", JsMinifier.minify("var x = \"\";"));
        }

        @Test
        void emptySingleQuoteString() {
            assertEquals("var x='';", JsMinifier.minify("var x = '';"));
        }

        @Test
        void stringWithEscapedNewline() {
            assertEquals("var x=\"line1\\nline2\";",
                    JsMinifier.minify("var x = \"line1\\nline2\";"));
        }

        @Test
        void adjacentStrings() {
            assertEquals("\"a\"+\"b\"", JsMinifier.minify("\"a\" + \"b\""));
        }

        @Test
        void stringThenRegex() {
            // After string, / is division (string acts like IDENTIFIER)
            assertEquals("\"a\"/b", JsMinifier.minify("\"a\" / b"));
        }
    }

    // ── String Quote Optimization ──────────────────────────────────────

    @Nested
    class StringQuoteOptimization {
        @Test
        void noOpDouble() {
            assertEquals("\"hello\"", JsMinifier.minify("\"hello\""));
        }

        @Test
        void noOpSingle() {
            assertEquals("'hello'", JsMinifier.minify("'hello'"));
        }

        @Test
        void swapDoubleToSingle() {
            assertEquals("'he said \"hi\"'",
                    JsMinifier.minify("\"he said \\\"hi\\\"\""));
        }

        @Test
        void swapSingleToDouble() {
            assertEquals("\"it's\"", JsMinifier.minify("'it\\'s'"));
        }

        @Test
        void tieNoSwap() {
            // 1 escaped same, 1 unescaped other — no savings
            assertEquals("\"a\\\"b'c\"", JsMinifier.minify("\"a\\\"b'c\""));
        }

        @Test
        void multipleEscapes() {
            assertEquals("'a\"b\"c'", JsMinifier.minify("\"a\\\"b\\\"c\""));
        }

        @Test
        void noSwapWhenWorse() {
            // No escaped quotes in original, swapping would add escapes
            assertEquals("\"it's\"", JsMinifier.minify("\"it's\""));
        }

        @Test
        void escapedBackslashNotConfused() {
            // 'a\\\'b' — the \\\\ is escaped backslash, \\' is escaped quote
            assertEquals("\"a\\\\'b\"", JsMinifier.minify("'a\\\\\\'b'"));
        }

        @Test
        void emptyDoubleUnchanged() {
            assertEquals("\"\"", JsMinifier.minify("\"\""));
        }

        @Test
        void emptySingleUnchanged() {
            assertEquals("''", JsMinifier.minify("''"));
        }

        @Test
        void singleEscapedQuote() {
            assertEquals("'\"'", JsMinifier.minify("\"\\\"\""));
        }

        @Test
        void idempotency() {
            String input = "\"he said \\\"hi\\\"\"";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void stringWithBothEscapes() {
            // "she said \"it's\"" — 2 escaped same, 1 unescaped other → net +1, swap
            assertEquals("'she said \"it\\'s\"'",
                    JsMinifier.minify("\"she said \\\"it's\\\"\""));
        }

        // ── Additional edge cases (bug hunting) ─────────────────────────

        @Test
        void singleQuoteOnlyEscapedQuote() {
            // '\'' → "'"
            assertEquals("\"'\"", JsMinifier.minify("'\\''"));
        }

        @Test
        void singleCharOtherQuote() {
            // "'" — no escapes to remove, swapping would add one → no swap
            assertEquals("\"'\"", JsMinifier.minify("\"'\""));
        }

        @Test
        void escapedBackslashThenQuoteChar() {
            // "a\\\"b" — \\=escaped backslash, \"=escaped quote
            // escapedSame=1, swap to 'a\\"b'
            assertEquals("'a\\\\\"b'", JsMinifier.minify("\"a\\\\\\\"b\""));
        }

        @Test
        void escapedBackslashThenQuoteCharIdempotent() {
            // Verify 'a\\"b' → no swap (0 escapedSame, 1 unescapedOther)
            String input = "\"a\\\\\\\"b\"";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void tripleEscapedQuotes() {
            // "\"\"\""  — 3 escaped same, 0 unescaped other → swap to '"""'
            assertEquals("'\"\"\"'", JsMinifier.minify("\"\\\"\\\"\\\"\""));
        }

        @Test
        void preserveNewlineEscape() {
            // "\n\"" — \n preserved, \" unescaped
            assertEquals("'\\n\"'", JsMinifier.minify("\"\\n\\\"\""));
        }

        @Test
        void preserveTabEscape() {
            // "\t\"" — \t preserved, \" unescaped
            assertEquals("'\\t\"'", JsMinifier.minify("\"\\t\\\"\""));
        }

        @Test
        void preserveUnicodeEscape() {
            // "\u0041\"" — \u0041 preserved, \" unescaped
            assertEquals("'\\u0041\"'", JsMinifier.minify("\"\\u0041\\\"\""));
        }

        @Test
        void preserveHexEscape() {
            // "\x41\"" — \x41 preserved, \" unescaped
            assertEquals("'\\x41\"'", JsMinifier.minify("\"\\x41\\\"\""));
        }

        @Test
        void preserveNullEscape() {
            // "\0\"" — \0 preserved, \" unescaped
            assertEquals("'\\0\"'", JsMinifier.minify("\"\\0\\\"\""));
        }

        @Test
        void onlyEscapedBackslash() {
            // "\\\\" — just escaped backslash, no quote escapes → no swap
            assertEquals("\"\\\\\"", JsMinifier.minify("\"\\\\\""));
        }

        @Test
        void escapedBackslashAtEnd() {
            // "a\\\\" — content is a\ , no quote escapes → no swap
            assertEquals("\"a\\\\\"", JsMinifier.minify("\"a\\\\\""));
        }

        @Test
        void twoStringsBackToBack() {
            // First string swaps (2 escaped quotes), second stays (0 escapes, has unescaped ')
            assertEquals("'he said \"hi\"'+\"it's\"",
                    JsMinifier.minify("\"he said \\\"hi\\\"\"+\"it's\""));
        }

        @Test
        void twoStringsWithDifferentSwapDirections() {
            // First swaps "→', second swaps '→"
            assertEquals("'a\"b'+\"c'd\"",
                    JsMinifier.minify("\"a\\\"b\"+'c\\'d'"));
        }

        @Test
        void stringInTemplateExpression() {
            // String inside template expression gets optimized
            assertEquals("`${'a\"b'}`",
                    JsMinifier.minify("`${\"a\\\"b\"}`"));
        }

        @Test
        void closingQuoteAtEndOfInput() {
            // String is the very last thing in input
            assertEquals("'a\"b'", JsMinifier.minify("\"a\\\"b\""));
        }

        @Test
        void manyEscapedQuotesSingleQuoted() {
            // '\'\'\'\'' — 4 escaped quotes → "''''"
            assertEquals("\"''''\"", JsMinifier.minify("'\\'\\'\\'\\''"));
        }

        @Test
        void escapedBackslashBeforeClosingQuote() {
            // 'a\\' — escaped backslash right before closing quote
            // No escaped quotes → no swap
            assertEquals("'a\\\\'", JsMinifier.minify("'a\\\\'"));
        }

        @Test
        void mixedEscapesThenQuoteEscape() {
            // "\n\t\"" — \n, \t preserved, \" unescaped when swapping
            assertEquals("'\\n\\t\"'", JsMinifier.minify("\"\\n\\t\\\"\""));
        }

        @Test
        void quoteOptimizationDoesNotAffectRegex() {
            // Regex with quotes should not be touched
            assertEquals("var x=/[\"']/;", JsMinifier.minify("var x = /[\"']/;"));
        }

        @Test
        void quoteOptimizationDoesNotAffectTemplateLiteral() {
            // Template literal with quotes should not be touched
            assertEquals("var x=`he said \"hi\"`;",
                    JsMinifier.minify("var x = `he said \"hi\"`;"));
        }

        @Test
        void doubleMinifyAllStringTypes() {
            String input = "var a = \"he said \\\"hi\\\"\", b = 'it\\'s', c = \"plain\";";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void stringFollowedByDivision() {
            // After string quote swap, division should still be recognized
            assertEquals("'a\"b'/c", JsMinifier.minify("\"a\\\"b\" / c"));
        }

        @Test
        void stringInCondition() {
            assertEquals("if('he said \"hi\"')x();",
                    JsMinifier.minify("if (\"he said \\\"hi\\\"\") x();"));
        }

        @Test
        void escapedBackslashThenUnescapedOtherQuote() {
            // "a\\'b" — \\ is escaped backslash, then ' is unescaped other
            // escapedSame=0, unescapedOther=1 → no swap (would make it worse)
            assertEquals("\"a\\\\'b\"", JsMinifier.minify("\"a\\\\'b\""));
        }
    }

    // ── Template Literal Edge Cases ─────────────────────────────────────

    @Nested
    class TemplateLiteralEdgeCases {
        @Test
        void multipleExpressions() {
            assertEquals("`${a}${b}${c}`", JsMinifier.minify("`${ a }${ b }${ c }`"));
        }

        @Test
        void templateExpressionOnly() {
            assertEquals("`${x}`", JsMinifier.minify("`${ x }`"));
        }

        @Test
        void templateWithTernary() {
            assertEquals("`${a?b:c}`", JsMinifier.minify("`${ a ? b : c }`"));
        }

        @Test
        void tripleNestedTemplate() {
            assertEquals("`a${`b${`c${d}`}`}`",
                    JsMinifier.minify("`a${ `b${ `c${ d }` }` }`"));
        }

        @Test
        void templateExpressionWithFunction() {
            assertEquals("`${fn(a,b)}`", JsMinifier.minify("`${ fn(a, b) }`"));
        }
    }

    // ── Miscellaneous Edge Cases ─────────────────────────────────────────

    @Nested
    class MiscEdgeCases {
        @Test
        void semicolonAfterReturn() {
            // return; on same line — no ASI issues, no newline needed
            assertEquals("return;foo;", JsMinifier.minify("return;\nfoo;"));
        }

        @Test
        void emptyStatements() {
            assertEquals(";;;", JsMinifier.minify("  ;  ;  ;  "));
        }

        @Test
        void forLoop() {
            assertEquals("for(var i=0;i<10;i++){}", JsMinifier.minify("for (var i = 0; i < 10; i++) { }"));
        }

        @Test
        void doWhile() {
            assertEquals("do{}while(!0);", JsMinifier.minify("do { } while (true);"));
        }

        @Test
        void ternaryOperator() {
            assertEquals("a?b:c", JsMinifier.minify("a ? b : c"));
        }

        @Test
        void chainedPropertyAccess() {
            assertEquals("a.b.c.d", JsMinifier.minify("a.b.c.d"));
        }

        @Test
        void arrayAccess() {
            assertEquals("a[0].b[1]", JsMinifier.minify("a[0].b[1]"));
        }

        @Test
        void labelStatement() {
            assertEquals("label:for(;;)break label;",
                    JsMinifier.minify("label: for (;;) break label;"));
        }

        @Test
        void commaOperator() {
            assertEquals("(a,b,c)", JsMinifier.minify("(a, b, c)"));
        }

        @Test
        void nestedParens() {
            assertEquals("((a+b)*(c-d))", JsMinifier.minify("((a + b) * (c - d))"));
        }

        @Test
        void switchCase() {
            assertEquals("switch(x){case 1:break;default:break}",
                    JsMinifier.minify("switch (x) {\n  case 1:\n    break;\n  default:\n    break;\n}"));
        }

        @Test
        void tryCatchFinally() {
            assertEquals("try{}catch(e){}finally{}",
                    JsMinifier.minify("try {\n} catch (e) {\n} finally {\n}"));
        }

        @Test
        void yieldStar() {
            assertEquals("yield*gen();", JsMinifier.minify("yield* gen();"));
        }

        @Test
        void asyncArrow() {
            assertEquals("const f=async(x)=>await x;",
                    JsMinifier.minify("const f = async (x) => await x;"));
        }

        @Test
        void negativeNumber() {
            assertEquals("var x=-1;", JsMinifier.minify("var x = -1;"));
        }

        @Test
        void unaryPlus() {
            assertEquals("var x=+y;", JsMinifier.minify("var x = +y;"));
        }

        @Test
        void bitwiseNot() {
            assertEquals("var x=~y;", JsMinifier.minify("var x = ~y;"));
        }

        @Test
        void logicalNot() {
            assertEquals("var x=!y;", JsMinifier.minify("var x = !y;"));
        }
    }

    // ── Literal Shortening ────────────────────────────────────────────────

    @Nested
    class LiteralShortening {
        @Test
        void trueBecomesNot0() {
            assertEquals("!0", JsMinifier.minify("true"));
        }

        @Test
        void falseBecomesNot1() {
            assertEquals("!1", JsMinifier.minify("false"));
        }

        @Test
        void undefinedBecomesVoid0() {
            assertEquals("void 0", JsMinifier.minify("undefined"));
        }

        @Test
        void inAssignment() {
            assertEquals("var x=!0;", JsMinifier.minify("var x = true;"));
        }

        @Test
        void afterReturn() {
            assertEquals("return!0;", JsMinifier.minify("return true;"));
        }

        @Test
        void returnUndefined() {
            assertEquals("return void 0;", JsMinifier.minify("return undefined;"));
        }

        @Test
        void inCondition() {
            assertEquals("if(!1)", JsMinifier.minify("if (false)"));
        }

        @Test
        void negation() {
            assertEquals("!!0", JsMinifier.minify("!true"));
        }

        @Test
        void comparison() {
            assertEquals("x===void 0", JsMinifier.minify("x === undefined"));
        }

        @Test
        void propertyAccessNotReplaced() {
            assertEquals("obj.undefined", JsMinifier.minify("obj.undefined"));
        }

        @Test
        void insideStringNotReplaced() {
            assertEquals("\"true\"", JsMinifier.minify("\"true\""));
        }

        @Test
        void idempotencyNotZero() {
            assertEquals("!0", JsMinifier.minify("!0"));
        }

        // ── String contexts (not replaced) ──────────────────────────────

        @Test
        void insideSingleQuoteStringNotReplaced() {
            assertEquals("'false'", JsMinifier.minify("'false'"));
        }

        @Test
        void insideTemplateLiteralTextNotReplaced() {
            assertEquals("`true`", JsMinifier.minify("`true`"));
        }

        @Test
        void insideTemplateExpressionReplaced() {
            assertEquals("`${!0}`", JsMinifier.minify("`${true}`"));
        }

        // ── Property access (not replaced) ──────────────────────────────

        @Test
        void dotAccessTrue() {
            assertEquals("obj.true", JsMinifier.minify("obj.true"));
        }

        @Test
        void dotAccessFalse() {
            assertEquals("obj.false", JsMinifier.minify("obj.false"));
        }

        @Test
        void optionalChainingNotReplaced() {
            assertEquals("obj?.undefined", JsMinifier.minify("obj?.undefined"));
        }

        // ── Similar identifiers (not replaced) ─────────────────────────

        @Test
        void trueValueIdentNotReplaced() {
            assertEquals("var trueValue=!0;", JsMinifier.minify("var trueValue = true;"));
        }

        @Test
        void prefixedUnderscoredNotReplaced() {
            assertEquals("var _undefined=1;", JsMinifier.minify("var _undefined = 1;"));
        }

        @Test
        void suffixedNumberNotReplaced() {
            assertEquals("var undefined2=1;", JsMinifier.minify("var undefined2 = 1;"));
        }

        // ── Multiple replacements ───────────────────────────────────────

        @Test
        void allThreeInOneStatement() {
            assertEquals("var a=!0,b=!1,c=void 0;",
                    JsMinifier.minify("var a = true, b = false, c = undefined;"));
        }

        @Test
        void arrayLiteral() {
            assertEquals("[!0,!1,void 0]",
                    JsMinifier.minify("[true, false, undefined]"));
        }

        @Test
        void functionArguments() {
            assertEquals("fn(!0,!1)", JsMinifier.minify("fn(true, false)"));
        }

        @Test
        void ternaryAllReplaced() {
            assertEquals("!0?!1:void 0",
                    JsMinifier.minify("true ? false : undefined"));
        }

        @Test
        void logicalOperators() {
            assertEquals("!0&&!1||void 0",
                    JsMinifier.minify("true && false || undefined"));
        }

        // ── Operator combinations ───────────────────────────────────────

        @Test
        void notFalse() {
            assertEquals("!!1", JsMinifier.minify("!false"));
        }

        @Test
        void doubleNotTrue() {
            assertEquals("!!!0", JsMinifier.minify("!!true"));
        }

        @Test
        void typeofUndefined() {
            assertEquals("typeof void 0===\"undefined\"",
                    JsMinifier.minify("typeof undefined === \"undefined\""));
        }

        @Test
        void voidExpressionNotConfused() {
            // void followed by 0 is already valid — stays as-is
            assertEquals("void 0", JsMinifier.minify("void 0"));
        }

        // ── ASI interaction ─────────────────────────────────────────────

        @Test
        void returnNewlineTrue() {
            assertEquals("return\n!0", JsMinifier.minify("return\ntrue"));
        }

        @Test
        void returnNewlineUndefined() {
            assertEquals("return\nvoid 0", JsMinifier.minify("return\nundefined"));
        }

        // ── Idempotency ────────────────────────────────────────────────

        @Test
        void idempotencyNot1() {
            assertEquals("!1", JsMinifier.minify("!1"));
        }

        @Test
        void idempotencyVoid0() {
            assertEquals("void 0", JsMinifier.minify("void 0"));
        }

        @Test
        void doubleMinifyWithLiterals() {
            String input = "var x = true; var y = false; var z = undefined;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        // ── Infinity shortening ─────────────────────────────────────────

        @Test
        void infinityBasic() {
            assertEquals("(1/0)", JsMinifier.minify("Infinity"));
        }

        @Test
        void infinityAssignment() {
            assertEquals("var x=(1/0);", JsMinifier.minify("var x = Infinity;"));
        }

        @Test
        void infinityUnaryMinus() {
            assertEquals("-(1/0)", JsMinifier.minify("-Infinity"));
        }

        @Test
        void infinityPropertyAccessNotReplaced() {
            assertEquals("obj.Infinity", JsMinifier.minify("obj.Infinity"));
        }

        @Test
        void infinityAddition() {
            assertEquals("a+(1/0)", JsMinifier.minify("a + Infinity"));
        }

        @Test
        void infinityInArray() {
            assertEquals("[(1/0)]", JsMinifier.minify("[Infinity]"));
        }

        @Test
        void infinityFunctionArg() {
            assertEquals("f((1/0))", JsMinifier.minify("f(Infinity)"));
        }

        @Test
        void infinityReturn() {
            assertEquals("return(1/0)", JsMinifier.minify("return Infinity"));
        }

        @Test
        void infinityTypeof() {
            assertEquals("typeof(1/0)", JsMinifier.minify("typeof Infinity"));
        }

        @Test
        void infinityComparison() {
            assertEquals("x===(1/0)", JsMinifier.minify("x === Infinity"));
        }

        @Test
        void infinityWithOtherLiterals() {
            assertEquals("[!0,(1/0)]", JsMinifier.minify("[true, Infinity]"));
        }

        @Test
        void infinityIdempotency() {
            assertEquals("(1/0)", JsMinifier.minify("(1/0)"));
        }

        @Test
        void infinityDoubleMinifyStability() {
            String input = "var x = Infinity;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void infinityInStringNotReplaced() {
            assertEquals("\"Infinity\"", JsMinifier.minify("\"Infinity\""));
        }

        @Test
        void infinityOptionalChainingNotReplaced() {
            assertEquals("obj?.Infinity", JsMinifier.minify("obj?.Infinity"));
        }

        @Test
        void infinityXIdentifierNotReplaced() {
            assertEquals("InfinityX", JsMinifier.minify("InfinityX"));
        }

        @Test
        void infinityMultiplication() {
            // Parens ensure a*(1/0) is not misinterpreted as (a*1)/0
            assertEquals("a*(1/0)", JsMinifier.minify("a * Infinity"));
        }

        @Test
        void infinityExponentiation() {
            // Parens ensure a**(1/0) is not misinterpreted as (a**1)/0
            assertEquals("a**(1/0)", JsMinifier.minify("a ** Infinity"));
        }

        @Test
        void infinityDivision() {
            assertEquals("a/(1/0)", JsMinifier.minify("a / Infinity"));
        }

        @Test
        void infinityPlusInfinity() {
            assertEquals("(1/0)+(1/0)", JsMinifier.minify("Infinity + Infinity"));
        }

        @Test
        void infinityTernary() {
            assertEquals("x?(1/0):0", JsMinifier.minify("x ? Infinity : 0"));
        }

        @Test
        void infinityLogicalNot() {
            assertEquals("!(1/0)", JsMinifier.minify("!Infinity"));
        }

        @Test
        void infinityUnaryPlus() {
            assertEquals("+(1/0)", JsMinifier.minify("+Infinity"));
        }

        @Test
        void infinityBitwiseNot() {
            assertEquals("~(1/0)", JsMinifier.minify("~Infinity"));
        }

        @Test
        void infinityInForOf() {
            assertEquals("for(const x of(1/0));",
                    JsMinifier.minify("for (const x of Infinity);"));
        }

        @Test
        void infinityAfterComma() {
            assertEquals("(a,(1/0))", JsMinifier.minify("(a, Infinity)"));
        }

        @Test
        void infinityInBracketAccess() {
            assertEquals("a[(1/0)]", JsMinifier.minify("a[Infinity]"));
        }

        @Test
        void infinityInSingleQuoteString() {
            assertEquals("'Infinity'", JsMinifier.minify("'Infinity'"));
        }

        @Test
        void infinityInTemplateLiteral() {
            assertEquals("`Infinity`", JsMinifier.minify("`Infinity`"));
        }

        @Test
        void infinityInTemplateExpression() {
            assertEquals("`${(1/0)}`", JsMinifier.minify("`${Infinity}`"));
        }

        @Test
        void infinityVoid() {
            assertEquals("void(1/0)", JsMinifier.minify("void Infinity"));
        }

        @Test
        void infinityDelete() {
            assertEquals("delete(1/0)", JsMinifier.minify("delete Infinity"));
        }

        @Test
        void infinityNew() {
            assertEquals("new(1/0)", JsMinifier.minify("new Infinity"));
        }

        @Test
        void infinityThrow() {
            assertEquals("throw(1/0);", JsMinifier.minify("throw Infinity;"));
        }

        @Test
        void infinityDoubleMinifyAllContexts() {
            String input = "var a = Infinity; return Infinity; typeof Infinity; [Infinity]; -Infinity;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void infinityWithAllLiterals() {
            assertEquals("[!0,!1,void 0,(1/0)]",
                    JsMinifier.minify("[true, false, undefined, Infinity]"));
        }

        @Test
        void infinityPropertyAccessChain() {
            // obj.Infinity.toString — Infinity as property, not replaced
            assertEquals("obj.Infinity.toString", JsMinifier.minify("obj.Infinity.toString"));
        }

        @Test
        void _infinityPrefixNotReplaced() {
            assertEquals("_Infinity", JsMinifier.minify("_Infinity"));
        }

        @Test
        void $infinityPrefixNotReplaced() {
            assertEquals("$Infinity", JsMinifier.minify("$Infinity"));
        }

        @Test
        void infinityInRegex() {
            assertEquals("var x=/Infinity/;", JsMinifier.minify("var x = /Infinity/;"));
        }

        @Test
        void infinityAfterNewline() {
            // return\nInfinity — ASI applies
            assertEquals("return\n(1/0)", JsMinifier.minify("return\nInfinity"));
        }

        @Test
        void infinityInComparison() {
            assertEquals("x<(1/0)", JsMinifier.minify("x < Infinity"));
        }

        @Test
        void infinitySemicolonBeforeBrace() {
            assertEquals("function f(){return(1/0)}",
                    JsMinifier.minify("function f() { return Infinity; }"));
        }

        @Test
        void infinityAssignmentOp() {
            assertEquals("x+=(1/0)", JsMinifier.minify("x += Infinity"));
        }
    }

    // ── Semicolon Before Brace ─────────────────────────────────────────

    @Nested
    class SemicolonBeforeBrace {
        @Test
        void basic() {
            assertEquals("{a()}", JsMinifier.minify("{ a(); }"));
        }

        @Test
        void functionBody() {
            assertEquals("function f(){return 1}", JsMinifier.minify("function f() { return 1; }"));
        }

        @Test
        void nestedBlocks() {
            assertEquals("{if(x){a()}}", JsMinifier.minify("{ if (x) { a(); } }"));
        }

        @Test
        void multipleSemicolons() {
            assertEquals("{}", JsMinifier.minify("{;;;}"));
        }

        @Test
        void emptyBlockUnchanged() {
            assertEquals("{}", JsMinifier.minify("{}"));
        }

        @Test
        void forLoopSemicolonsPreserved() {
            assertEquals("for(;;){}", JsMinifier.minify("for(;;){}"));
        }

        @Test
        void doWhile() {
            assertEquals("do{a()}while(x);", JsMinifier.minify("do { a(); } while(x);"));
        }

        @Test
        void arrowFunction() {
            assertEquals("const f=()=>{return 1};", JsMinifier.minify("const f = () => { return 1; };"));
        }

        @Test
        void classBody() {
            assertEquals("class A{x=1}", JsMinifier.minify("class A { x = 1; }"));
        }

        @Test
        void switchStatement() {
            assertEquals("switch(x){case 1:break}", JsMinifier.minify("switch(x) { case 1: break; }"));
        }

        @Test
        void semicolonNotBeforeBracePreserved() {
            assertEquals("a;b", JsMinifier.minify("a; b"));
        }

        @Test
        void semicolonAtEofPreserved() {
            assertEquals("a;", JsMinifier.minify("a;"));
        }

        @Test
        void idempotency() {
            String input = "function f(){return 1}";
            assertEquals(input, JsMinifier.minify(input));
        }

        @Test
        void doubleMinify() {
            String input = "function f() { return 1; }";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }
    }

    // ── Consecutive Declaration Merging ─────────────────────────────────

    @Nested
    class ConsecutiveDeclarationMerging {

        // ── Basic merging ───────────────────────────────────────────────

        @Test
        void varMerge() {
            assertEquals("var a=1,b=2;", JsMinifier.minify("var a=1;var b=2;"));
        }

        @Test
        void letMerge() {
            assertEquals("let a=1,b=2;", JsMinifier.minify("let a=1;let b=2;"));
        }

        @Test
        void constMerge() {
            assertEquals("const a=1,b=2;", JsMinifier.minify("const a=1;const b=2;"));
        }

        @Test
        void threeConsecutive() {
            assertEquals("var a=1,b=2,c=3;", JsMinifier.minify("var a=1;var b=2;var c=3;"));
        }

        // ── Different keywords NOT merged ───────────────────────────────

        @Test
        void varThenLet() {
            assertEquals("var a=1;let b=2;", JsMinifier.minify("var a=1;let b=2;"));
        }

        @Test
        void letThenConst() {
            assertEquals("let a=1;const b=2;", JsMinifier.minify("let a=1;const b=2;"));
        }

        // ── Complex initializers ────────────────────────────────────────

        @Test
        void functionExpression() {
            assertEquals("var a=function(){},b=2;", JsMinifier.minify("var a=function(){};var b=2;"));
        }

        @Test
        void objectLiteral() {
            assertEquals("var a={x:1},b=2;", JsMinifier.minify("var a={x:1};var b=2;"));
        }

        @Test
        void arrayLiteral() {
            assertEquals("var a=[1,2],b=3;", JsMinifier.minify("var a=[1,2];var b=3;"));
        }

        @Test
        void arrowFunction() {
            assertEquals("var a=(x)=>x,b=2;", JsMinifier.minify("var a=(x)=>x;var b=2;"));
        }

        // ── Strings/regex/templates containing keywords ─────────────────

        @Test
        void stringContainingVar() {
            assertEquals("var a=\"var x\",b=1;", JsMinifier.minify("var a=\"var x\";var b=1;"));
        }

        @Test
        void regexContainingVar() {
            assertEquals("var a=/var/,b=1;", JsMinifier.minify("var a=/var/;var b=1;"));
        }

        // ── NOT merged across block boundaries ──────────────────────────

        @Test
        void differentBlocks() {
            assertEquals("{var a=1}var b=2", JsMinifier.minify("{var a=1}var b=2"));
        }

        @Test
        void ifBlock() {
            assertEquals("if(x){var a=1}var b=2", JsMinifier.minify("if(x){var a=1}var b=2"));
        }

        // ── for-loop protection ─────────────────────────────────────────

        @Test
        void forLoopVar() {
            assertEquals("for(var i=0;i<10;i++){}", JsMinifier.minify("for(var i=0;i<10;i++){}"));
        }

        @Test
        void forLoopLet() {
            assertEquals("for(let i=0;i<10;i++){}", JsMinifier.minify("for(let i=0;i<10;i++){}"));
        }

        // ── No declarations ─────────────────────────────────────────────

        @Test
        void noDeclarations() {
            assertEquals("a=1;b=2;", JsMinifier.minify("a=1;b=2;"));
        }

        @Test
        void alreadyMinifiedNoDecl() {
            String input = "x+y;z();";
            assertEquals(input, JsMinifier.minify(input));
        }

        // ── Destructuring ───────────────────────────────────────────────

        @Test
        void objectDestructuring() {
            assertEquals("var{a}=x,{b}=y;", JsMinifier.minify("var{a}=x;var{b}=y;"));
        }

        @Test
        void arrayDestructuring() {
            assertEquals("var[a]=x,[b]=y;", JsMinifier.minify("var[a]=x;var[b]=y;"));
        }

        // ── Inside function body ────────────────────────────────────────

        @Test
        void insideFunctionBody() {
            assertEquals("function f(){var a=1,b=2}",
                    JsMinifier.minify("function f(){var a=1;var b=2}"));
        }

        // ── Idempotency ────────────────────────────────────────────────

        @Test
        void idempotency() {
            String input = "var a=1;var b=2;var c=3;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void doubleMinifyStability() {
            String input = "var x = 1; var y = 2; let a = 3; let b = 4;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        // ── Edge cases ──────────────────────────────────────────────────

        @Test
        void singleDeclaration() {
            assertEquals("var a=1;", JsMinifier.minify("var a=1;"));
        }

        @Test
        void noTrailingSemicolon() {
            assertEquals("var a=1,b=2", JsMinifier.minify("var a=1;var b=2"));
        }

        @Test
        void noInitializer() {
            assertEquals("var a,b;", JsMinifier.minify("var a;var b;"));
        }

        // ── From unminified source ──────────────────────────────────────

        @Test
        void fromUnminifiedSource() {
            assertEquals("var a=1,b=2;",
                    JsMinifier.minify("var a = 1;\nvar b = 2;"));
        }

        @Test
        void fromUnminifiedThree() {
            assertEquals("var a=1,b=2,c=3;",
                    JsMinifier.minify("var a = 1;\nvar b = 2;\nvar c = 3;"));
        }

        @Test
        void mixedKeywordsFromUnminified() {
            assertEquals("var a=1;let b=2;const c=3;",
                    JsMinifier.minify("var a = 1;\nlet b = 2;\nconst c = 3;"));
        }

        // ── Many consecutive ────────────────────────────────────────────

        @Test
        void fiveConsecutive() {
            assertEquals("var a=1,b=2,c=3,d=4,e=5;",
                    JsMinifier.minify("var a=1;var b=2;var c=3;var d=4;var e=5;"));
        }

        // ── Non-declaration interrupts chain ────────────────────────────

        @Test
        void statementInterruptsChain() {
            assertEquals("var a=1;console.log(a);var b=2;",
                    JsMinifier.minify("var a=1;console.log(a);var b=2;"));
        }

        @Test
        void functionCallBetweenDeclarations() {
            assertEquals("var a=1;f();var b=2;",
                    JsMinifier.minify("var a=1;f();var b=2;"));
        }

        @Test
        void assignmentBetweenDeclarations() {
            assertEquals("var a=1;x=2;var b=3;",
                    JsMinifier.minify("var a=1;x=2;var b=3;"));
        }

        @Test
        void ifBetweenDeclarations() {
            assertEquals("var a=1;if(x){}var b=2;",
                    JsMinifier.minify("var a=1;if(x){}var b=2;"));
        }

        // ── Back-to-back merge groups ───────────────────────────────────

        @Test
        void twoMergeGroups() {
            assertEquals("var a=1,b=2;let c=3,d=4;",
                    JsMinifier.minify("var a=1;var b=2;let c=3;let d=4;"));
        }

        @Test
        void threeMergeGroups() {
            assertEquals("var a=1,b=2;let c=3,d=4;const e=5,f=6;",
                    JsMinifier.minify("var a=1;var b=2;let c=3;let d=4;const e=5;const f=6;"));
        }

        // ── Complex initializers (more) ─────────────────────────────────

        @Test
        void ternaryInInitializer() {
            assertEquals("var a=x?1:2,b=3;", JsMinifier.minify("var a=x?1:2;var b=3;"));
        }

        @Test
        void nestedCallInInitializer() {
            assertEquals("var a=f(g(h())),b=1;", JsMinifier.minify("var a=f(g(h()));var b=1;"));
        }

        @Test
        void deeplyNestedBracesInInitializer() {
            assertEquals("var a=function(){return{x:function(){}}},b=2;",
                    JsMinifier.minify("var a=function(){return{x:function(){}}};var b=2;"));
        }

        @Test
        void arrowWithBlockBody() {
            assertEquals("var a=()=>{return 1},b=2;",
                    JsMinifier.minify("var a=()=>{return 1};var b=2;"));
        }

        @Test
        void nestedObjectLiteral() {
            assertEquals("var a={x:{y:{z:1}}},b=2;",
                    JsMinifier.minify("var a={x:{y:{z:1}}};var b=2;"));
        }

        @Test
        void mixedBracesAndBrackets() {
            assertEquals("var a={x:[1,2]},b=[{y:3}];",
                    JsMinifier.minify("var a={x:[1,2]};var b=[{y:3}];"));
        }

        @Test
        void functionWithIfElseBody() {
            assertEquals("var a=function(){if(x){return 1}else{return 2}},b=3;",
                    JsMinifier.minify("var a=function(){if(x){return 1}else{return 2}};var b=3;"));
        }

        // ── Strings with semicolons and keywords ────────────────────────

        @Test
        void stringWithSemicolon() {
            assertEquals("var a=\"x;y;z\",b=1;",
                    JsMinifier.minify("var a=\"x;y;z\";var b=1;"));
        }

        @Test
        void stringWithSemicolonAndKeyword() {
            assertEquals("var a=\";var x\",b=1;",
                    JsMinifier.minify("var a=\";var x\";var b=1;"));
        }

        @Test
        void singleQuoteStringWithSemicolon() {
            assertEquals("var a=';let x',b=1;",
                    JsMinifier.minify("var a=';let x';var b=1;"));
        }

        @Test
        void escapedQuoteInString() {
            // Quote optimizer swaps 'it\'s' → "it's"
            assertEquals("var a=\"it's\",b=2;",
                    JsMinifier.minify("var a='it\\'s';var b=2;"));
        }

        @Test
        void escapedBackslashBeforeClosingQuote() {
            assertEquals("var a=\"x\\\\\",b=1;",
                    JsMinifier.minify("var a=\"x\\\\\";var b=1;"));
        }

        // ── Template literals with keywords ─────────────────────────────

        @Test
        void templateWithVarKeyword() {
            assertEquals("var a=`var x`,b=1;",
                    JsMinifier.minify("var a=`var x`;var b=1;"));
        }

        @Test
        void templateWithSemicolonAndKeyword() {
            assertEquals("var a=`;var x`,b=1;",
                    JsMinifier.minify("var a=`;var x`;var b=1;"));
        }

        @Test
        void templateWithExpression() {
            assertEquals("var a=`${x+1}`,b=2;",
                    JsMinifier.minify("var a=`${x+1}`;var b=2;"));
        }

        @Test
        void templateWithNestedTemplate() {
            assertEquals("var a=`${`inner`}`,b=2;",
                    JsMinifier.minify("var a=`${`inner`}`;var b=2;"));
        }

        // ── Regex with semicolons and keywords ──────────────────────────

        @Test
        void regexWithSemicolon() {
            assertEquals("var a=/;/,b=1;", JsMinifier.minify("var a=/;/;var b=1;"));
        }

        @Test
        void regexWithFlags() {
            assertEquals("var a=/test/gi,b=1;", JsMinifier.minify("var a=/test/gi;var b=1;"));
        }

        @Test
        void regexWithSemicolonKeyword() {
            assertEquals("var a=/;var/,b=1;", JsMinifier.minify("var a=/;var/;var b=1;"));
        }

        @Test
        void regexWithCharClass() {
            assertEquals("var a=/[;]/,b=1;", JsMinifier.minify("var a=/[;]/;var b=1;"));
        }

        // ── for-loop interactions ───────────────────────────────────────

        @Test
        void forLoopConst() {
            assertEquals("for(const x of arr){}", JsMinifier.minify("for(const x of arr){}"));
        }

        @Test
        void forLoopThenDeclarations() {
            assertEquals("for(var i=0;i<10;i++){}var j=1,k=2;",
                    JsMinifier.minify("for(var i=0;i<10;i++){}var j=1;var k=2;"));
        }

        @Test
        void forLoopWithNestedParens() {
            assertEquals("for(var i=(0);i<10;i++){}",
                    JsMinifier.minify("for(var i=(0);i<10;i++){}"));
        }

        @Test
        void forLoopWithFunctionCallInCondition() {
            assertEquals("for(var i=0;f(i);i++){}",
                    JsMinifier.minify("for(var i=0;f(i);i++){}"));
        }

        @Test
        void nestedForLoops() {
            assertEquals("for(var i=0;i<10;i++){for(var j=0;j<10;j++){}}",
                    JsMinifier.minify("for(var i=0;i<10;i++){for(var j=0;j<10;j++){}}"));
        }

        @Test
        void forInLoop() {
            assertEquals("for(var x in y){}", JsMinifier.minify("for(var x in y){}"));
        }

        @Test
        void forOfLoop() {
            assertEquals("for(var x of y){}", JsMinifier.minify("for(var x of y){}"));
        }

        // ── Nested block scoping ────────────────────────────────────────

        @Test
        void mergeInsideNestedFunction() {
            assertEquals("function f(){function g(){var a=1,b=2}}",
                    JsMinifier.minify("function f(){function g(){var a=1;var b=2}}"));
        }

        @Test
        void independentMergePerBlock() {
            assertEquals("function f(){var a=1,b=2}function g(){var c=3,d=4}",
                    JsMinifier.minify("function f(){var a=1;var b=2}function g(){var c=3;var d=4}"));
        }

        @Test
        void declAfterClosingBraceNotMergedWithInner() {
            // var inside if block should not merge with var after the block
            assertEquals("if(x){var a=1}var b=2", JsMinifier.minify("if(x){var a=1}var b=2"));
        }

        @Test
        void declsAfterClosingBraceMergeWithEachOther() {
            // Two vars AFTER the block are at the same depth — they DO merge
            assertEquals("if(x){var a=1}var b=2,c=3",
                    JsMinifier.minify("if(x){var a=1}var b=2;var c=3"));
        }

        @Test
        void mergeInsideIfBlock() {
            assertEquals("if(x){var a=1,b=2}",
                    JsMinifier.minify("if(x){var a=1;var b=2}"));
        }

        @Test
        void switchCaseDeclarations() {
            assertEquals("switch(x){case 1:var a=1,b=2;break}",
                    JsMinifier.minify("switch(x){case 1:var a=1;var b=2;break}"));
        }

        // ── Declaration-like identifiers ────────────────────────────────

        @Test
        void varInIdentifierNotTriggered() {
            // "variable" starts with "var" but isn't the keyword
            assertEquals("variable=1;var b=2;",
                    JsMinifier.minify("variable=1;var b=2;"));
        }

        @Test
        void letInIdentifierNotTriggered() {
            assertEquals("letter=1;let b=2;",
                    JsMinifier.minify("letter=1;let b=2;"));
        }

        @Test
        void constInIdentifierNotTriggered() {
            assertEquals("constructor();const b=2;",
                    JsMinifier.minify("constructor();const b=2;"));
        }

        @Test
        void varFollowedByIdentStartingWithVar() {
            // "var variable=1;var variance=2;" — both are var declarations
            assertEquals("var variable=1,variance=2;",
                    JsMinifier.minify("var variable=1;var variance=2;"));
        }

        // ── Empty declarations / semicolons ─────────────────────────────

        @Test
        void emptySemicolonsBetweenDeclarations() {
            // var a=1;;var b=2 — the empty statement interrupts
            assertEquals("var a=1;;var b=2;",
                    JsMinifier.minify("var a=1;;var b=2;"));
        }

        @Test
        void declarationWithOnlyComma() {
            // Already comma-separated declarations — should not double-merge
            assertEquals("var a=1,b=2;", JsMinifier.minify("var a=1,b=2;"));
        }

        // ── Interaction with other minifier features ────────────────────

        @Test
        void mergeWithLiteralShortening() {
            assertEquals("var a=!0,b=!1,c=void 0;",
                    JsMinifier.minify("var a = true; var b = false; var c = undefined;"));
        }

        @Test
        void mergeWithNumericShortening() {
            assertEquals("var a=1e3,b=.5;",
                    JsMinifier.minify("var a = 1000; var b = 0.5;"));
        }

        @Test
        void mergeWithStringQuoteOptimization() {
            assertEquals("var a='he said \"hi\"',b=1;",
                    JsMinifier.minify("var a = \"he said \\\"hi\\\"\"; var b = 1;"));
        }

        @Test
        void mergeWithScientificNotation() {
            assertEquals("var a=1e6,b=2e3,c=300;",
                    JsMinifier.minify("var a = 1000000; var b = 2000; var c = 300;"));
        }

        @Test
        void mergeWithInfinity() {
            assertEquals("var a=(1/0),b=1;",
                    JsMinifier.minify("var a = Infinity; var b = 1;"));
        }

        @Test
        void mergeWithSemicolonBeforeBrace() {
            // Semicolons stripped before } by main pass, then merge
            assertEquals("function f(){var a=1,b=2}",
                    JsMinifier.minify("function f() { var a = 1; var b = 2; }"));
        }

        // ── Keyword at end/start of input ───────────────────────────────

        @Test
        void declarationAtEndOfInput() {
            assertEquals("var a=1", JsMinifier.minify("var a=1"));
        }

        @Test
        void onlyDeclarations() {
            assertEquals("var a=1,b=2,c=3",
                    JsMinifier.minify("var a=1;var b=2;var c=3"));
        }

        // ── IIFE and complex expressions ────────────────────────────────

        @Test
        void iifeDoesNotBreakMerge() {
            assertEquals("var a=(function(){return 1})(),b=2;",
                    JsMinifier.minify("var a=(function(){return 1})();var b=2;"));
        }

        @Test
        void classExpressionInitializer() {
            assertEquals("var A=class{constructor(){}},b=1;",
                    JsMinifier.minify("var A=class{constructor(){}};var b=1;"));
        }

        // ── Mixed destructuring ─────────────────────────────────────────

        @Test
        void mixedDestructuringAndNormal() {
            assertEquals("var{a}=x,b=1,[c]=y;",
                    JsMinifier.minify("var{a}=x;var b=1;var[c]=y;"));
        }

        @Test
        void constDestructuring() {
            assertEquals("const{a,b}=x,{c,d}=y;",
                    JsMinifier.minify("const{a,b}=x;const{c,d}=y;"));
        }

        @Test
        void letArrayDestructuring() {
            assertEquals("let[a,b]=x,[c,d]=y;",
                    JsMinifier.minify("let[a,b]=x;let[c,d]=y;"));
        }

        // ── Comprehensive idempotency ───────────────────────────────────

        @Test
        void doubleMinifyComplex() {
            String input = "var a=function(){};var b={x:1};var c=[1,2];let d=1;let e=2;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyWithInterruption() {
            String input = "var a = 1; var b = 2; x(); var c = 3; var d = 4;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyNestedBlocks() {
            String input = "function f() { var a = 1; var b = 2; } function g() { var c = 3; var d = 4; }";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyAllFeatures() {
            String input = "var a = true; var b = 1000; var c = 0.5; var d = \"he said \\\"hi\\\"\"; var e = Infinity;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyForAndDecl() {
            String input = "for (var i = 0; i < 10; i++) {} var x = 1; var y = 2;";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }
    }

    // ── Bracket-to-Dot Property Access Conversion ─────────────────────

    @Nested
    class BracketToDotConversion {

        // ── Basic conversions ───────────────────────────────────────────

        @Test
        void doubleQuoteProp() {
            assertEquals("obj.prop", JsMinifier.minify("obj[\"prop\"]"));
        }

        @Test
        void singleQuoteProp() {
            assertEquals("obj.prop", JsMinifier.minify("obj['prop']"));
        }

        @Test
        void shortIdentifier() {
            assertEquals("a.x", JsMinifier.minify("a[\"x\"]"));
        }

        // ── Various preceding expressions ────────────────────────────────

        @Test
        void afterIdentifier() {
            assertEquals("foo.bar", JsMinifier.minify("foo[\"bar\"]"));
        }

        @Test
        void afterCloseParen() {
            assertEquals("f().bar", JsMinifier.minify("f()[\"bar\"]"));
        }

        @Test
        void afterCloseBracket() {
            assertEquals("a[0].bar", JsMinifier.minify("a[0][\"bar\"]"));
        }

        @Test
        void afterTemplateLiteral() {
            assertEquals("`t`.bar", JsMinifier.minify("`t`[\"bar\"]"));
        }

        @Test
        void afterStringLiteral() {
            assertEquals("\"s\".length", JsMinifier.minify("\"s\"[\"length\"]"));
        }

        @Test
        void afterSingleQuoteStringLiteral() {
            assertEquals("'s'.length", JsMinifier.minify("'s'[\"length\"]"));
        }

        @Test
        void afterThis() {
            assertEquals("this.prop", JsMinifier.minify("this[\"prop\"]"));
        }

        @Test
        void afterNewExpression() {
            assertEquals("new Foo().bar", JsMinifier.minify("new Foo()[\"bar\"]"));
        }

        @Test
        void afterNewWithArgs() {
            assertEquals("new Foo(1,2).bar", JsMinifier.minify("new Foo(1, 2)[\"bar\"]"));
        }

        @Test
        void afterParenWrappedFunctionExpression() {
            assertEquals("(function(){}).call", JsMinifier.minify("(function(){})[\"call\"]"));
        }

        @Test
        void afterRegexWithFlags() {
            // /re/g — last char in result is 'g' (identPart) → converts
            assertEquals("/re/g.source", JsMinifier.minify("/re/g[\"source\"]"));
        }

        // ── After number literals (bare integer safety) ──────────────────

        @Test
        void afterBareIntegerOne() {
            // 1.toString is a syntax error — must NOT convert
            assertEquals("1[\"toString\"]", JsMinifier.minify("1[\"toString\"]"));
        }

        @Test
        void afterBareIntegerFortyTwo() {
            assertEquals("42[\"toString\"]", JsMinifier.minify("42[\"toString\"]"));
        }

        @Test
        void afterBareIntegerZero() {
            assertEquals("0[\"toString\"]", JsMinifier.minify("0[\"toString\"]"));
        }

        @Test
        void afterBareIntegerHundred() {
            // 100 isn't shortened to scientific (same length), stays bare integer
            assertEquals("100[\"toString\"]", JsMinifier.minify("100[\"toString\"]"));
        }

        @Test
        void afterFloat() {
            // 1.5.toFixed is valid JS — decimal point already present
            assertEquals("1.5.toFixed", JsMinifier.minify("1.5[\"toFixed\"]"));
        }

        @Test
        void afterLeadingDotFloat() {
            // .5.toFixed is valid JS
            assertEquals(".5.toFixed", JsMinifier.minify("0.5[\"toFixed\"]"));
        }

        @Test
        void afterScientificNotation() {
            // 1e3.toString is valid JS — exponent makes it unambiguous
            assertEquals("1e3.toString", JsMinifier.minify("1000[\"toString\"]"));
        }

        @Test
        void afterScientificWithPlusSign() {
            // 1e+3.toString is valid JS
            assertEquals("1e+3.toString", JsMinifier.minify("1e+3[\"toString\"]"));
        }

        @Test
        void afterScientificWithMinusSign() {
            assertEquals("1e-3.toString", JsMinifier.minify("1e-3[\"toString\"]"));
        }

        @Test
        void afterHexLiteral() {
            // 0xff.toString is valid JS — hex prefix makes it unambiguous
            assertEquals("0xff.toString", JsMinifier.minify("0xff[\"toString\"]"));
        }

        @Test
        void afterHexEndingInDigit() {
            // 0x10 ends with digit '0' — but hex prefix makes it safe
            assertEquals("0x10.toString", JsMinifier.minify("0x10[\"toString\"]"));
        }

        @Test
        void afterOctalLiteral() {
            assertEquals("0o77.toString", JsMinifier.minify("0o77[\"toString\"]"));
        }

        @Test
        void afterBinaryLiteral() {
            assertEquals("0b10.toString", JsMinifier.minify("0b10[\"toString\"]"));
        }

        @Test
        void afterDoubleDotNumber() {
            // 1..toString() — in minified output, 1. has a trailing dot so 1..prop is fine
            // but this is already dot notation, not bracket access
            assertEquals("1..toString()", JsMinifier.minify("1..toString()"));
        }

        @Test
        void afterIdentifierEndingWithDigit() {
            // myVar2 is an identifier, not a number — should convert
            assertEquals("myVar2.prop", JsMinifier.minify("myVar2[\"prop\"]"));
        }

        @Test
        void afterNumericSeparatorBareInteger() {
            // 1_0 stays as 1_0 (1e1 not shorter), and it's a bare integer — must NOT convert
            assertEquals("1_0[\"toString\"]", JsMinifier.minify("1_0[\"toString\"]"));
        }

        @Test
        void afterParenWrappedInteger() {
            // (42)["toString"] → prev char is ')' → converts safely
            assertEquals("(42).toString", JsMinifier.minify("(42)[\"toString\"]"));
        }

        // ── NOT converted — invalid identifiers ─────────────────────────

        @Test
        void hyphenatedProperty() {
            assertEquals("obj[\"foo-bar\"]", JsMinifier.minify("obj[\"foo-bar\"]"));
        }

        @Test
        void spaceInProperty() {
            assertEquals("obj[\"foo bar\"]", JsMinifier.minify("obj[\"foo bar\"]"));
        }

        @Test
        void digitStartProperty() {
            assertEquals("obj[\"123\"]", JsMinifier.minify("obj[\"123\"]"));
        }

        @Test
        void singleDigitProperty() {
            assertEquals("obj[\"0\"]", JsMinifier.minify("obj[\"0\"]"));
        }

        @Test
        void emptyStringProperty() {
            assertEquals("obj[\"\"]", JsMinifier.minify("obj[\"\"]"));
        }

        @Test
        void digitThenLetters() {
            assertEquals("obj[\"1abc\"]", JsMinifier.minify("obj[\"1abc\"]"));
        }

        @Test
        void plusCharProperty() {
            assertEquals("obj[\"+\"]", JsMinifier.minify("obj[\"+\"]"));
        }

        @Test
        void dotCharProperty() {
            assertEquals("obj[\".\"]", JsMinifier.minify("obj[\".\"]"));
        }

        @Test
        void spaceCharProperty() {
            assertEquals("obj[\" \"]", JsMinifier.minify("obj[\" \"]"));
        }

        // ── NOT converted — not string literal ──────────────────────────

        @Test
        void variableIndex() {
            assertEquals("obj[prop]", JsMinifier.minify("obj[prop]"));
        }

        @Test
        void numericIndex() {
            assertEquals("obj[0]", JsMinifier.minify("obj[0]"));
        }

        @Test
        void expressionIndex() {
            assertEquals("obj[f()]", JsMinifier.minify("obj[f()]"));
        }

        @Test
        void binaryExpressionIndex() {
            assertEquals("obj[a+b]", JsMinifier.minify("obj[a + b]"));
        }

        @Test
        void templateLiteralIndex() {
            assertEquals("obj[`key`]", JsMinifier.minify("obj[`key`]"));
        }

        // ── NOT converted — not property access context ──────────────────

        @Test
        void arrayLiteral() {
            assertEquals("[\"a\",\"b\"]", JsMinifier.minify("[\"a\",\"b\"]"));
        }

        @Test
        void semicolonThenBracket() {
            assertEquals(";[\"a\"]", JsMinifier.minify(";[\"a\"]"));
        }

        @Test
        void assignmentThenBracket() {
            assertEquals("=[\"a\"]", JsMinifier.minify("=[\"a\"]"));
        }

        @Test
        void afterPlus() {
            assertEquals("a+[\"b\"]", JsMinifier.minify("a + [\"b\"]"));
        }

        @Test
        void afterMinus() {
            assertEquals("a-[\"b\"]", JsMinifier.minify("a - [\"b\"]"));
        }

        @Test
        void afterStar() {
            assertEquals("a*[\"b\"]", JsMinifier.minify("a * [\"b\"]"));
        }

        @Test
        void afterColon() {
            assertEquals("{key:[\"a\"]}", JsMinifier.minify("{ key: [\"a\"] }"));
        }

        @Test
        void afterQuestionMark() {
            assertEquals("x?[\"a\"]:y", JsMinifier.minify("x ? [\"a\"] : y"));
        }

        @Test
        void afterComma() {
            assertEquals("f(a,[\"b\"])", JsMinifier.minify("f(a, [\"b\"])"));
        }

        @Test
        void afterOpenParen() {
            assertEquals("([\"a\"])", JsMinifier.minify("([\"a\"])"));
        }

        @Test
        void afterOpenBracket() {
            assertEquals("[[\"a\"]]", JsMinifier.minify("[[\"a\"]]"));
        }

        @Test
        void afterReturnKeyword() {
            assertEquals("return[\"a\"]", JsMinifier.minify("return [\"a\"]"));
        }

        @Test
        void bracketAtStartOfInput() {
            assertEquals("[\"prop\"]", JsMinifier.minify("[\"prop\"]"));
        }

        // ── NOT converted — after } (ambiguous) ─────────────────────────

        @Test
        void afterCloseBrace() {
            assertEquals("{}[\"x\"]", JsMinifier.minify("{}[\"x\"]"));
        }

        @Test
        void afterFunctionBrace() {
            // (function(){})["call"] — prev char to [ is ), which IS expression-ending → converts
            assertEquals("(function(){}).call",
                    JsMinifier.minify("(function(){})[\"call\"]"));
        }

        // ── Optional chaining (should NOT convert) ───────────────────────

        @Test
        void optionalBracketAccess() {
            // obj?.["prop"] — prev char is '.', not in expression-ending list
            assertEquals("obj?.[\"prop\"]", JsMinifier.minify("obj?.[\"prop\"]"));
        }

        @Test
        void optionalBracketAfterOptionalDot() {
            assertEquals("obj?.a?.[\"b\"]", JsMinifier.minify("obj?.a?.[\"b\"]"));
        }

        @Test
        void normalBracketAfterOptionalDot() {
            // obj?.prop["name"] — prev char is 'p' (identPart) → converts
            assertEquals("obj?.prop.name", JsMinifier.minify("obj?.prop[\"name\"]"));
        }

        // ── Computed property names in object/class (should NOT convert) ──

        @Test
        void computedPropertyInObject() {
            // { ["key"]: val } — prev char is '{', not expression-ending
            assertEquals("{[\"key\"]:val}", JsMinifier.minify("{ [\"key\"]: val }"));
        }

        @Test
        void computedPropertyAfterComma() {
            assertEquals("{a:1,[\"key\"]:2}", JsMinifier.minify("{ a: 1, [\"key\"]: 2 }"));
        }

        // ── Escaped strings skipped ──────────────────────────────────────

        @Test
        void escapedSingleQuoteInProp() {
            assertEquals("obj[\"it\\'s\"]", JsMinifier.minify("obj[\"it\\'s\"]"));
        }

        @Test
        void escapedBackslashInProp() {
            assertEquals("obj[\"a\\\\b\"]", JsMinifier.minify("obj[\"a\\\\b\"]"));
        }

        @Test
        void escapedNewlineInProp() {
            assertEquals("obj[\"a\\nb\"]", JsMinifier.minify("obj[\"a\\nb\"]"));
        }

        @Test
        void escapedUnicodeInProp() {
            assertEquals("obj[\"\\u0041\"]", JsMinifier.minify("obj[\"\\u0041\"]"));
        }

        // ── Strings/regex/templates not broken ───────────────────────────

        @Test
        void bracketInsideString() {
            // String quote optimizer swaps to single quotes (2 escaped " → 0)
            assertEquals("'obj[\"prop\"]'", JsMinifier.minify("\"obj[\\\"prop\\\"]\""));
        }

        @Test
        void bracketInsideSingleQuoteString() {
            assertEquals("'obj[\"prop\"]'", JsMinifier.minify("'obj[\"prop\"]'"));
        }

        @Test
        void bracketInsideTemplate() {
            assertEquals("`obj[\"prop\"]`", JsMinifier.minify("`obj[\"prop\"]`"));
        }

        @Test
        void bracketInsideRegex() {
            assertEquals("var x=/obj\\[\"prop\"\\]/;", JsMinifier.minify("var x = /obj\\[\"prop\"\\]/;"));
        }

        @Test
        void bracketInsideRegexCharClass() {
            assertEquals("var x=/[\"']/;", JsMinifier.minify("var x = /[\"']/;"));
        }

        // ── Mixed quote types ────────────────────────────────────────────

        @Test
        void mixedQuotesChaining() {
            assertEquals("obj.a.b", JsMinifier.minify("obj['a'][\"b\"]"));
        }

        @Test
        void alternatingQuotes() {
            assertEquals("obj.a.b.c", JsMinifier.minify("obj[\"a\"]['b'][\"c\"]"));
        }

        // ── Chained conversions ──────────────────────────────────────────

        @Test
        void twoChained() {
            assertEquals("obj.a.b", JsMinifier.minify("obj[\"a\"][\"b\"]"));
        }

        @Test
        void threeChained() {
            assertEquals("obj.a.b.c", JsMinifier.minify("obj[\"a\"][\"b\"][\"c\"]"));
        }

        @Test
        void chainedWithNumericIndex() {
            assertEquals("a[0].b.c", JsMinifier.minify("a[0][\"b\"][\"c\"]"));
        }

        @Test
        void mixedDotAndBracketChain() {
            assertEquals("a.b.c.d.e", JsMinifier.minify("a.b[\"c\"].d[\"e\"]"));
        }

        @Test
        void longMixedChain() {
            assertEquals("a.b.c.d.e.f", JsMinifier.minify("a[\"b\"].c[\"d\"].e[\"f\"]"));
        }

        @Test
        void chainAfterFunctionCall() {
            assertEquals("a().b.c", JsMinifier.minify("a()[\"b\"][\"c\"]"));
        }

        // ── Multiple bracket accesses with operators ─────────────────────

        @Test
        void additionOfBracketResults() {
            assertEquals("a.x+b.y", JsMinifier.minify("a[\"x\"]+b[\"y\"]"));
        }

        @Test
        void comparisonOfBracketResults() {
            assertEquals("a.x===b.y", JsMinifier.minify("a[\"x\"]===b[\"y\"]"));
        }

        @Test
        void ternaryWithBracketResults() {
            assertEquals("a.x?b.y:c.z", JsMinifier.minify("a[\"x\"]?b[\"y\"]:c[\"z\"]"));
        }

        @Test
        void logicalAndBracketResults() {
            assertEquals("a.x&&b.y||c.z", JsMinifier.minify("a[\"x\"]&&b[\"y\"]||c[\"z\"]"));
        }

        // ── Inside template expressions ──────────────────────────────────
        // Note: skipTemplateLiteral skips the entire template including ${...}
        // expressions, so bracket accesses inside templates are NOT converted
        // by the post-pass. This is a conservative but safe behavior.

        @Test
        void bracketInTemplateExpression() {
            assertEquals("`${obj[\"prop\"]}`", JsMinifier.minify("`${obj[\"prop\"]}`"));
        }

        @Test
        void multipleBracketsInTemplate() {
            assertEquals("`${a[\"x\"]} ${b[\"y\"]}`", JsMinifier.minify("`${a[\"x\"]} ${b[\"y\"]}`"));
        }

        @Test
        void bracketWithOperatorsInTemplate() {
            assertEquals("`${a[\"x\"]+b[\"y\"]}`", JsMinifier.minify("`${a[\"x\"]+b[\"y\"]}`"));
        }

        // ── Interaction with other features ──────────────────────────────

        @Test
        void inVarDeclaration() {
            assertEquals("var a=obj.prop;", JsMinifier.minify("var a=obj[\"prop\"];"));
        }

        @Test
        void declarationMergingAndBracket() {
            assertEquals("var a=x.p,b=y.q;",
                    JsMinifier.minify("var a=x[\"p\"];var b=y[\"q\"];"));
        }

        @Test
        void afterBooleanShortening() {
            // true → !0, then !0["toString"] — prev char '0' is a bare integer
            // but !0 as a whole isn't numeric property access; prev to [ is '0' which is digit
            // 0.toString would be ambiguous → don't convert
            assertEquals("!0[\"toString\"]", JsMinifier.minify("true[\"toString\"]"));
        }

        @Test
        void afterInfinityShortening() {
            // Infinity → (1/0), then (1/0)["toString"] → prev is ')' → converts
            assertEquals("(1/0).toString", JsMinifier.minify("Infinity[\"toString\"]"));
        }

        @Test
        void bracketAfterScientificFromLargeNumber() {
            // 1000 → 1e3, then 1e3["toString"] → converts (exponent is safe)
            assertEquals("1e3.toString", JsMinifier.minify("1000[\"toString\"]"));
        }

        @Test
        void bracketAfterQuoteOptimizedString() {
            // String gets quotes swapped, then bracket converted
            assertEquals("'he said \"hi\"'.length",
                    JsMinifier.minify("\"he said \\\"hi\\\"\"[\"length\"]"));
        }

        @Test
        void threeVarsWithBracketAccess() {
            assertEquals("var a=x.p,b=y.q,c=z.r;",
                    JsMinifier.minify("var a = x[\"p\"]; var b = y[\"q\"]; var c = z[\"r\"];"));
        }

        // ── Reserved words as properties (valid in ES5+) ─────────────────

        @Test
        void reservedWordClass() {
            assertEquals("obj.class", JsMinifier.minify("obj[\"class\"]"));
        }

        @Test
        void reservedWordReturn() {
            assertEquals("obj.return", JsMinifier.minify("obj[\"return\"]"));
        }

        @Test
        void reservedWordIf() {
            assertEquals("obj.if", JsMinifier.minify("obj[\"if\"]"));
        }

        @Test
        void reservedWordVar() {
            assertEquals("obj.var", JsMinifier.minify("obj[\"var\"]"));
        }

        @Test
        void reservedWordDelete() {
            assertEquals("obj.delete", JsMinifier.minify("obj[\"delete\"]"));
        }

        @Test
        void reservedWordNew() {
            assertEquals("obj.new", JsMinifier.minify("obj[\"new\"]"));
        }

        @Test
        void reservedWordThis() {
            assertEquals("obj.this", JsMinifier.minify("obj[\"this\"]"));
        }

        @Test
        void reservedWordSuper() {
            assertEquals("obj.super", JsMinifier.minify("obj[\"super\"]"));
        }

        @Test
        void reservedWordAwait() {
            assertEquals("obj.await", JsMinifier.minify("obj[\"await\"]"));
        }

        @Test
        void reservedWordYield() {
            assertEquals("obj.yield", JsMinifier.minify("obj[\"yield\"]"));
        }

        // ── Built-in property names ──────────────────────────────────────

        @Test
        void constructorProperty() {
            assertEquals("obj.constructor", JsMinifier.minify("obj[\"constructor\"]"));
        }

        @Test
        void prototypeProperty() {
            assertEquals("obj.prototype", JsMinifier.minify("obj[\"prototype\"]"));
        }

        @Test
        void dunderProtoProperty() {
            assertEquals("obj.__proto__", JsMinifier.minify("obj[\"__proto__\"]"));
        }

        @Test
        void hasOwnPropertyProperty() {
            assertEquals("obj.hasOwnProperty", JsMinifier.minify("obj[\"hasOwnProperty\"]"));
        }

        @Test
        void toStringProperty() {
            assertEquals("obj.toString", JsMinifier.minify("obj[\"toString\"]"));
        }

        // ── Unicode identifiers ──────────────────────────────────────────

        @Test
        void unicodeProperty() {
            assertEquals("obj.café", JsMinifier.minify("obj[\"café\"]"));
        }

        @Test
        void cjkProperty() {
            assertEquals("obj.日本", JsMinifier.minify("obj[\"日本\"]"));
        }

        // ── Single character special identifiers ─────────────────────────

        @Test
        void singleUnderscore() {
            assertEquals("obj._", JsMinifier.minify("obj[\"_\"]"));
        }

        @Test
        void singleDollar() {
            assertEquals("obj.$", JsMinifier.minify("obj[\"$\"]"));
        }

        @Test
        void dollarUnderscore() {
            assertEquals("obj.$_", JsMinifier.minify("obj[\"$_\"]"));
        }

        @Test
        void identifierWithDigits() {
            assertEquals("obj.abc123", JsMinifier.minify("obj[\"abc123\"]"));
        }

        // ── Idempotency ─────────────────────────────────────────────────

        @Test
        void alreadyDotNotation() {
            assertEquals("obj.prop", JsMinifier.minify("obj.prop"));
        }

        @Test
        void doubleMinifyStability() {
            String input = "obj[\"prop\"]";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void doubleMinifyChained() {
            String input = "obj[\"a\"][\"b\"][\"c\"]";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void doubleMinifyBareInteger() {
            String input = "42[\"toString\"]";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void doubleMinifyMixedChain() {
            String input = "a.b[\"c\"].d[\"e\"]";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void doubleMinifyWithOperators() {
            String input = "a[\"x\"] + b[\"y\"] === c[\"z\"]";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void doubleMinifyInTemplate() {
            // Templates are skipped by post-pass, but still must be idempotent
            String input = "`${a[\"x\"]} ${b[\"y\"]}`";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        // ── Edge cases ──────────────────────────────────────────────────

        @Test
        void bracketAtEndOfInput() {
            assertEquals("obj.prop", JsMinifier.minify("obj[\"prop\"]"));
        }

        @Test
        void singleCharProperty() {
            assertEquals("obj.x", JsMinifier.minify("obj[\"x\"]"));
        }

        @Test
        void underscoreProperty() {
            assertEquals("obj._priv", JsMinifier.minify("obj[\"_priv\"]"));
        }

        @Test
        void dollarProperty() {
            assertEquals("obj.$x", JsMinifier.minify("obj[\"$x\"]"));
        }

        @Test
        void privateFieldNotConverted() {
            assertEquals("obj[\"#x\"]", JsMinifier.minify("obj[\"#x\"]"));
        }

        @Test
        void longPropertyName() {
            assertEquals("obj.abcdefghijklmnopqrstuvwxyz",
                    JsMinifier.minify("obj[\"abcdefghijklmnopqrstuvwxyz\"]"));
        }

        @Test
        void afterVoidExpression() {
            // void 0["prop"] — prev char '0' is a bare integer, don't convert
            // (this is void (0["prop"]), which is weird but valid)
            assertEquals("void 0[\"prop\"]", JsMinifier.minify("void 0[\"prop\"]"));
        }

        @Test
        void afterNullLiteral() {
            // null["prop"] — prev char 'l', identPart → converts
            // (runtime error but syntactically valid)
            assertEquals("null.prop", JsMinifier.minify("null[\"prop\"]"));
        }

        @Test
        void afterUnaryNotOnExpression() {
            // !obj["prop"] — the [ is preceded by ] from "obj", wait no...
            // Actually: !obj["prop"] — minified is !obj["prop"]
            // The prev char to [ is 'j' (from obj) → identPart → converts
            assertEquals("!obj.prop", JsMinifier.minify("!obj[\"prop\"]"));
        }

        @Test
        void negationOfArray() {
            // !["a"] — prev char is '!', not expression-ending → NOT converted
            assertEquals("![\"a\"]", JsMinifier.minify("![\"a\"]"));
        }

        @Test
        void bitwiseNotBeforeArray() {
            assertEquals("~[\"a\"]", JsMinifier.minify("~[\"a\"]"));
        }

        @Test
        void afterIncrementOperator() {
            // x++["toString"] — prev char is '+', not in expression-ending list
            // → NOT converted (this is a conservative choice)
            assertEquals("x++[\"toString\"]", JsMinifier.minify("x++[\"toString\"]"));
        }

        @Test
        void afterDecrementOperator() {
            assertEquals("x--[\"toString\"]", JsMinifier.minify("x--[\"toString\"]"));
        }

        @Test
        void afterRegexNoFlags() {
            // /re/["source"] — prev char is '/', not expression-ending → NOT converted
            assertEquals("/re/[\"source\"]", JsMinifier.minify("/re/[\"source\"]"));
        }

        // ── Comprehensive double-minify ──────────────────────────────────

        @Test
        void doubleMinifyAllFeatures() {
            String input = "var a = obj[\"prop\"]; var b = arr[0][\"name\"]; var c = f()[\"result\"];";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyAllInteractions() {
            String input = "var a = 1000; var b = obj[\"prop\"]; var c = true; var d = arr[0][\"name\"];";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }

        @Test
        void doubleMinifyOptionalChaining() {
            String input = "obj?.[\"prop\"]";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second);
        }

        @Test
        void doubleMinifyBareIntegersAllSizes() {
            String input = "0[\"a\"];1[\"b\"];42[\"c\"];100[\"d\"];999[\"e\"]";
            String first = JsMinifier.minify(input);
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent: first=" + first);
        }
    }

    // ── Redundant Return Removal ─────────────────────────────────────────

    @Nested
    class RedundantReturnRemoval {

        // Basic removal
        @Test
        void bareReturnWithSemicolon() {
            assertEquals("function f(){}", JsMinifier.minify("function f(){ return; }"));
        }

        @Test
        void returnUndefined() {
            assertEquals("function f(){}", JsMinifier.minify("function f(){ return undefined; }"));
        }

        @Test
        void returnVoid0() {
            assertEquals("function f(){}", JsMinifier.minify("function f(){ return void 0; }"));
        }

        // With preceding statement
        @Test
        void precedingStatementBareReturn() {
            assertEquals("function f(){doStuff()}", JsMinifier.minify("function f(){ doStuff(); return; }"));
        }

        @Test
        void precedingStatementReturnUndefined() {
            assertEquals("function f(){doStuff()}", JsMinifier.minify("function f(){ doStuff(); return undefined; }"));
        }

        // Function expressions and arrows
        @Test
        void functionExpression() {
            assertEquals("var f=function(){}", JsMinifier.minify("var f = function(){ return; }"));
        }

        @Test
        void arrowFunctionBareReturn() {
            assertEquals("var f=()=>{}", JsMinifier.minify("var f = () => { return; }"));
        }

        @Test
        void arrowFunctionWithPrecedingStatement() {
            assertEquals("var f=()=>{doStuff()}", JsMinifier.minify("var f = () => { doStuff(); return; }"));
        }

        @Test
        void iifeWithReturn() {
            assertEquals("(function(){})()", JsMinifier.minify("(function(){ return; })()"));
        }

        // Generator and async
        @Test
        void generatorFunction() {
            assertEquals("function*gen(){yield 1}", JsMinifier.minify("function* gen(){ yield 1; return; }"));
        }

        @Test
        void asyncFunction() {
            assertEquals("async function f(){await x}", JsMinifier.minify("async function f(){ await x; return; }"));
        }

        // Must NOT remove — return with value
        @Test
        void returnWithNumberValue() {
            assertEquals("function f(){return 1}", JsMinifier.minify("function f(){ return 1; }"));
        }

        @Test
        void returnWithBooleanValue() {
            assertEquals("function f(){return!0}", JsMinifier.minify("function f(){ return true; }"));
        }

        @Test
        void returnWithObjectValue() {
            assertEquals("function f(){return{x:1}}", JsMinifier.minify("function f(){ return {x:1}; }"));
        }

        @Test
        void returnWithStringValue() {
            assertEquals("function f(){return\"hello\"}", JsMinifier.minify("function f(){ return \"hello\"; }"));
        }

        // Must NOT remove — return in inner block (not end of function)
        @Test
        void returnInIfBlock() {
            assertEquals("function f(x){if(x){return}doStuff()}", JsMinifier.minify("function f(x){ if(x){ return; } doStuff(); }"));
        }

        @Test
        void returnInForBlock() {
            assertEquals("function f(x){for(;;){return}y()}", JsMinifier.minify("function f(x){ for(;;){ return; } y(); }"));
        }

        // Nested functions
        @Test
        void nestedBothRedundant() {
            assertEquals("function a(){function b(){}}", JsMinifier.minify("function a(){ function b(){ return; } return; }"));
        }

        @Test
        void nestedInnerRedundantOuterValue() {
            assertEquals("function a(){function b(){}return b()}", JsMinifier.minify("function a(){ function b(){ return; } return b(); }"));
        }

        // Edge cases
        @Test
        void myreturnIsNotKeyword() {
            assertEquals("function f(){myreturn}", JsMinifier.minify("function f(){ myreturn; }"));
        }

        @Test
        void returnInsideString() {
            assertEquals("function f(){console.log(\"return\")}", JsMinifier.minify("function f(){ console.log(\"return\"); }"));
        }

        @Test
        void emptyFunctionStays() {
            assertEquals("function f(){}", JsMinifier.minify("function f(){}"));
        }

        @Test
        void functionWithOnlyReturn() {
            assertEquals("function f(){}", JsMinifier.minify("function f(){ return; }"));
        }

        // Not converted — method shorthand (v1 limitation)
        @Test
        void methodShorthandNotConverted() {
            assertEquals("var obj={m(){return}}", JsMinifier.minify("var obj = { m(){ return; } }"));
        }

        @Test
        void classMethodNotConverted() {
            assertEquals("class A{m(){return}}", JsMinifier.minify("class A{ m(){ return; } }"));
        }

        // Idempotency
        @Test
        void alreadyOptimal() {
            assertEquals("function f(){}", JsMinifier.minify("function f(){}"));
        }

        @Test
        void doubleMinifyStable() {
            String first = JsMinifier.minify("function f(){ return; }");
            String second = JsMinifier.minify(first);
            assertEquals(first, second, "Not idempotent");
        }

        // Interaction with other features
        @Test
        void interactionWithDeclarationMerging() {
            assertEquals("var a=function(){},b=2;", JsMinifier.minify("var a = function(){ return; }; var b = 2;"));
        }

        @Test
        void interactionWithBracketToDot() {
            assertEquals("function f(){obj.x}", JsMinifier.minify("function f(){ obj[\"x\"]; return; }"));
        }

        @Test
        void interactionWithLiteralShortening() {
            assertEquals("function f(){}", JsMinifier.minify("function f(){ return undefined; }"));
        }

        // Named function expression
        @Test
        void namedFunctionExpression() {
            assertEquals("var f=function named(){}", JsMinifier.minify("var f = function named(){ return; }"));
        }

        // Arrow with parameters
        @Test
        void arrowWithParams() {
            assertEquals("var f=(a,b)=>{}", JsMinifier.minify("var f = (a, b) => { return; }"));
        }

        // ── Braceless control structures — must NOT remove return ──

        @Test
        void bracelessIfReturnAtEnd() {
            // return is the body of if, not a standalone statement
            assertEquals("function f(x){if(x)return}", JsMinifier.minify("function f(x){ if(x) return; }"));
        }

        @Test
        void bracelessElseReturnAtEnd() {
            assertEquals("function f(x){if(x)return;else return}", JsMinifier.minify("function f(x){ if(x) return; else return; }"));
        }

        @Test
        void bracelessWhileReturnAtEnd() {
            assertEquals("function f(x){while(x)return}", JsMinifier.minify("function f(x){ while(x) return; }"));
        }

        @Test
        void bracelessForReturnAtEnd() {
            assertEquals("function f(){for(;;)return}", JsMinifier.minify("function f(){ for(;;) return; }"));
        }

        @Test
        void bracelessIfReturnVoid0AtEnd() {
            assertEquals("function f(x){if(x)return void 0}", JsMinifier.minify("function f(x){ if(x) return void 0; }"));
        }

        @Test
        void bracelessIfReturnUndefinedAtEnd() {
            // undefined → void 0 by literal shortening
            assertEquals("function f(x){if(x)return void 0}", JsMinifier.minify("function f(x){ if(x) return undefined; }"));
        }

        @Test
        void nestedBracelessIfReturn() {
            // if(y)return is inside braced if(x){...} block, not function body
            assertEquals("function f(x,y){if(x){if(y)return}}", JsMinifier.minify("function f(x,y){ if(x){ if(y) return; } }"));
        }

        // ── Conditional early return with code after — must keep ──

        @Test
        void earlyReturnWithCodeAfter() {
            assertEquals("function f(x){if(x)return;doStuff()}", JsMinifier.minify("function f(x){ if(x) return; doStuff(); }"));
        }

        @Test
        void earlyReturnInBracedIfWithCodeAfter() {
            assertEquals("function f(x){if(x){return}doStuff()}", JsMinifier.minify("function f(x){ if(x){ return; } doStuff(); }"));
        }

        @Test
        void multipleEarlyReturns() {
            assertEquals("function f(x,y){if(x)return;if(y)return;doStuff()}", JsMinifier.minify("function f(x,y){ if(x) return; if(y) return; doStuff(); }"));
        }

        // ── Return after block constructs at function end — CAN remove ──

        @Test
        void returnAfterIfBlockAtEnd() {
            assertEquals("function f(x){if(x){doStuff()}}", JsMinifier.minify("function f(x){ if(x){ doStuff(); } return; }"));
        }

        @Test
        void returnAfterTryCatchAtEnd() {
            assertEquals("function f(){try{doStuff()}catch(e){}}", JsMinifier.minify("function f(){ try{ doStuff(); }catch(e){} return; }"));
        }

        @Test
        void returnAfterForLoopAtEnd() {
            assertEquals("function f(a){for(var i=0;i<a.length;i++){doStuff(a[i])}}", JsMinifier.minify("function f(a){ for(var i=0; i<a.length; i++){ doStuff(a[i]); } return; }"));
        }

        @Test
        void returnAfterWhileAtEnd() {
            assertEquals("function f(){while(cond()){doStuff()}}", JsMinifier.minify("function f(){ while(cond()){ doStuff(); } return; }"));
        }

        @Test
        void returnAfterSwitchAtEnd() {
            assertEquals("function f(x){switch(x){case 1:doA();break;default:doB()}}", JsMinifier.minify("function f(x){ switch(x){ case 1: doA(); break; default: doB(); } return; }"));
        }

        // ── Return inside inner blocks — must NOT remove ──

        @Test
        void returnInSwitchCase() {
            assertEquals("function f(x){switch(x){case 1:return;default:return}}", JsMinifier.minify("function f(x){ switch(x){ case 1: return; default: return; } }"));
        }

        @Test
        void returnInTryCatch() {
            assertEquals("function f(){try{doStuff()}catch(e){return}}", JsMinifier.minify("function f(){ try{ doStuff(); } catch(e){ return; } }"));
        }

        @Test
        void returnInFinally() {
            assertEquals("function f(){try{doStuff()}finally{return}}", JsMinifier.minify("function f(){ try{ doStuff(); } finally{ return; } }"));
        }

        @Test
        void returnInDoWhile() {
            assertEquals("function f(){do{return}while(!1)}", JsMinifier.minify("function f(){ do{ return; } while(false); }"));
        }

        @Test
        void returnInForIn() {
            assertEquals("function f(o){for(var k in o){return}}", JsMinifier.minify("function f(o){ for(var k in o){ return; } }"));
        }

        @Test
        void returnInForOf() {
            assertEquals("function f(a){for(var x of a){return}}", JsMinifier.minify("function f(a){ for(var x of a){ return; } }"));
        }

        // ── Deeply nested and mixed ──

        @Test
        void tripleNestedFunctions() {
            assertEquals("function a(){function b(){function c(){}}}", JsMinifier.minify("function a(){ function b(){ function c(){ return; } return; } return; }"));
        }

        @Test
        void arrowInsideRegularFunction() {
            assertEquals("function f(){var g=()=>{}}", JsMinifier.minify("function f(){ var g = () => { return; }; return; }"));
        }

        @Test
        void regularInsideArrow() {
            assertEquals("var f=()=>{function g(){}}", JsMinifier.minify("var f = () => { function g(){ return; } return; }"));
        }

        @Test
        void innerFunctionWithValueReturnOuterRedundant() {
            assertEquals("function a(){function b(){return 1}}", JsMinifier.minify("function a(){ function b(){ return 1; } return; }"));
        }

        // ── String/regex/template containing return-like patterns ──

        @Test
        void returnInTemplateLiteral() {
            assertEquals("function f(){console.log(`return`)}", JsMinifier.minify("function f(){ console.log(`return`); }"));
        }

        @Test
        void returnInRegex() {
            assertEquals("function f(){/return/.test(x)}", JsMinifier.minify("function f(){ /return/.test(x); }"));
        }

        @Test
        void returnInTemplateExpression() {
            assertEquals("function f(){console.log(`${return_val}`)}", JsMinifier.minify("function f(){ console.log(`${return_val}`); }"));
        }

        // ── Default parameters with tricky content ──

        @Test
        void defaultParamWithReturnString() {
            assertEquals("function f(x='return'){}", JsMinifier.minify("function f(x = 'return'){ return; }"));
        }

        @Test
        void defaultParamWithBraces() {
            assertEquals("function f(x={a:1}){}", JsMinifier.minify("function f(x = {a: 1}){ return; }"));
        }

        // ── return with values that must NOT be removed ──

        @Test
        void returnWithVoidExpression() {
            assertEquals("function f(){return void doStuff()}", JsMinifier.minify("function f(){ return void doStuff(); }"));
        }

        @Test
        void returnWithNegation() {
            assertEquals("function f(){return!1}", JsMinifier.minify("function f(){ return false; }"));
        }

        @Test
        void returnWithArray() {
            assertEquals("function f(){return[1,2]}", JsMinifier.minify("function f(){ return [1, 2]; }"));
        }

        @Test
        void returnWithFunctionCall() {
            assertEquals("function f(){return doStuff()}", JsMinifier.minify("function f(){ return doStuff(); }"));
        }

        @Test
        void returnWithTernary() {
            assertEquals("function f(x){return x?1:2}", JsMinifier.minify("function f(x){ return x ? 1 : 2; }"));
        }

        @Test
        void returnWithNull() {
            assertEquals("function f(){return null}", JsMinifier.minify("function f(){ return null; }"));
        }

        @Test
        void returnWithThis() {
            assertEquals("function f(){return this}", JsMinifier.minify("function f(){ return this; }"));
        }

        // ── Ternary and comma at function end — CAN remove bare return after them ──

        @Test
        void ternaryThenBareReturn() {
            assertEquals("function f(x){x?doA():doB()}", JsMinifier.minify("function f(x){ x ? doA() : doB(); return; }"));
        }

        @Test
        void commaExprThenBareReturn() {
            assertEquals("function f(){a(),b()}", JsMinifier.minify("function f(){ a(), b(); return; }"));
        }

        // ── Labeled statement edge case ──

        @Test
        void labeledBlockThenReturn() {
            assertEquals("function f(){label:{break label}}", JsMinifier.minify("function f(){ label: { break label; } return; }"));
        }

        // ── Getter/setter not detected as function body (v1 limitation) ──

        @Test
        void getterNotConverted() {
            assertEquals("var o={get x(){return}}", JsMinifier.minify("var o = { get x(){ return; } }"));
        }

        @Test
        void setterNotConverted() {
            assertEquals("var o={set x(v){return}}", JsMinifier.minify("var o = { set x(v){ return; } }"));
        }
    }

    // ── Performance ──────────────────────────────────────────────────────

    @Nested
    class Performance {
        @Test
        void largeInput() {
            // Generate 100KB+ input
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 5000; i++) {
                sb.append("var variable").append(i).append(" = ").append(i).append("; // comment\n");
            }
            String input = sb.toString();
            assertTrue(input.length() > 100_000);

            long start = System.nanoTime();
            String result = JsMinifier.minify(input);
            long elapsed = System.nanoTime() - start;

            // Should complete in well under 1 second
            assertTrue(elapsed < 1_000_000_000L, "Took too long: " + (elapsed / 1_000_000) + "ms");
            // Verify it actually minified
            assertTrue(result.length() < input.length());
            assertFalse(result.contains("// comment"));
        }
    }
}
