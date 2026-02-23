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
