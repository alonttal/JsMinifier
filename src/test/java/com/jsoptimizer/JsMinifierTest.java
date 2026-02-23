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
            assertEquals("var x=1;var y=2;", JsMinifier.minify("var x = 1;\nvar y = 2;"));
        }

        @Test
        void windowsLineEndings() {
            assertEquals("var x=1;var y=2;", JsMinifier.minify("var x = 1;\r\nvar y = 2;"));
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
            assertEquals("var x=1;var y=2;",
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
            assertEquals("var x=\"he said \\\"hi\\\"\";",
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
            assertEquals("async function f(){await fetch(url);}",
                    JsMinifier.minify("async function f() { await fetch(url); }"));
        }

        @Test
        void classDeclaration() {
            assertEquals("class Foo extends Bar{constructor(){super();}}",
                    JsMinifier.minify("class Foo extends Bar {\n  constructor() {\n    super();\n  }\n}"));
        }

        @Test
        void destructuring() {
            assertEquals("const{a,b}=obj;const[c,d]=arr;",
                    JsMinifier.minify("const { a, b } = obj; const [c, d] = arr;"));
        }

        @Test
        void spreadRest() {
            assertEquals("const arr=[...a,...b];",
                    JsMinifier.minify("const arr = [...a, ...b];"));
        }

        @Test
        void generatorFunction() {
            assertEquals("function*gen(){yield 1;yield 2;}",
                    JsMinifier.minify("function* gen() {\n  yield 1;\n  yield 2;\n}"));
        }

        @Test
        void importExport() {
            assertEquals("import{foo}from'bar';export default baz;",
                    JsMinifier.minify("import { foo } from 'bar'; export default baz;"));
        }

        @Test
        void privateFields() {
            assertEquals("class A{#x=1;get x(){return this.#x;}}",
                    JsMinifier.minify("class A {\n  #x = 1;\n  get x() { return this.#x; }\n}"));
        }

        @Test
        void logicalAssignment() {
            assertEquals("a??=b;a||=c;a&&=d;",
                    JsMinifier.minify("a ??= b; a ||= c; a &&= d;"));
        }

        @Test
        void numericSeparators() {
            assertEquals("var x=1_000_000;",
                    JsMinifier.minify("var x = 1_000_000;"));
        }
    }

    // ── Function Minification ────────────────────────────────────────────

    @Nested
    class FunctionMinification {
        @Test
        void simpleFunction() {
            assertEquals("function foo(a,b){return a+b;}",
                    JsMinifier.minify("function foo(a, b) {\n  return a + b;\n}"));
        }

        @Test
        void iife() {
            assertEquals("(function(){var x=1;})();",
                    JsMinifier.minify("(function() {\n  var x = 1;\n})();"));
        }

        @Test
        void nestedFunctions() {
            assertEquals("function outer(){function inner(){return 1;}return inner();}",
                    JsMinifier.minify(
                            "function outer() {\n  function inner() {\n    return 1;\n  }\n  return inner();\n}"));
        }
    }

    // ── Idempotency ──────────────────────────────────────────────────────

    @Nested
    class Idempotency {
        @Test
        void alreadyMinified() {
            String minified = "var x=1;var y=2;function f(a,b){return a+b;}";
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
            // 1.0.toString() — number 1.0 then .toString()
            assertEquals("1.0.toString()", JsMinifier.minify("1.0.toString()"));
        }

        @Test
        void leadingDotNumber() {
            assertEquals("var x=.5;", JsMinifier.minify("var x = .5;"));
        }
    }

    // ── Line Ending Edge Cases ──────────────────────────────────────────

    @Nested
    class LineEndingEdgeCases {
        @Test
        void carriageReturnOnly() {
            // Old Mac-style \r only line endings
            assertEquals("var x=1;var y=2;", JsMinifier.minify("var x = 1;\rvar y = 2;"));
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
            assertEquals("switch(x){case 1:break;default:break;}",
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
