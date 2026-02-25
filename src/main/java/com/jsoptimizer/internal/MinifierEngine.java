package com.jsoptimizer.internal;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Set;

public final class MinifierEngine {

    private static final Set<String> REGEX_PRECEDING_KEYWORDS = Set.of(
            "return", "typeof", "instanceof", "in", "delete", "void",
            "throw", "new", "case", "yield", "await", "of"
    );

    // Keywords after which [ starts an array/expression, NOT property access.
    // Excludes value keywords: this, super, null (where [ IS property access).
    private static final Set<String> NON_VALUE_KEYWORDS = Set.of(
            "var", "let", "const",
            "if", "else", "for", "while", "do", "switch", "with",
            "try", "catch", "finally",
            "class", "function",
            "import", "export", "default", "debugger",
            "return", "throw", "break", "continue",
            "typeof", "void", "delete", "new",
            "in", "instanceof", "of",
            "yield", "await",
            "case", "extends"
    );

    private static final Set<String> ASI_KEYWORDS = Set.of(
            "return", "throw", "continue", "break", "yield"
    );

    private final CharStream stream;
    private final StringBuilder out;
    private State state;

    // Template literal nesting: each entry is the brace depth when we entered
    // CODE from a template expression `${...}`
    private final Deque<Integer> templateStack = new ArrayDeque<>();
    private int braceDepth;

    // Tracks the last significant token kind for regex/division disambiguation
    private TokenKind lastTokenKind = TokenKind.NONE;
    // Buffer to capture identifier/keyword text for ASI and regex detection
    private final StringBuilder identBuf = new StringBuilder();

    // Whether we need to preserve a newline (for ASI protection)
    private boolean pendingNewline;

    // Tracks whether the current multi-line comment contains a line terminator
    private boolean commentContainedNewline;

    // Tracks where the current string literal begins in `out` (for quote optimization)
    private int stringStart;

    private enum TokenKind {
        NONE,            // start of input
        IDENTIFIER,      // identifiers, keywords, numbers
        CLOSE_PAREN,     // )
        CLOSE_BRACKET,   // ]
        CLOSE_BRACE,     // }
        INCREMENT,       // ++
        DECREMENT,       // --
        OPERATOR,        // everything else that's an operator/punctuator
        NUMBER_LITERAL   // numeric literals
    }

    public MinifierEngine(String input) {
        this.stream = new CharStream(input);
        this.out = new StringBuilder(input.length());
        this.state = State.CODE;
    }

    public String minify() {
        while (stream.hasMore()) {
            switch (state) {
                case CODE -> processCode();
                case SINGLE_LINE_COMMENT -> processSingleLineComment();
                case MULTI_LINE_COMMENT -> processMultiLineComment();
                case SINGLE_QUOTE_STRING -> processString('\'');
                case DOUBLE_QUOTE_STRING -> processString('"');
                case TEMPLATE_LITERAL -> processTemplateLiteral();
                case REGEX_LITERAL -> processRegex();
            }
        }
        return shortenArrowBodies(removeRedundantReturn(convertBracketToDot(foldStringConcatenation(mergeConsecutiveDeclarations(out.toString())))));
    }

    // ── CODE state ──────────────────────────────────────────────────────

    private void processCode() {
        char c = stream.current();

        // Handle \r\n as \n
        if (c == '\r') {
            stream.advance();
            if (stream.hasMore() && stream.current() == '\n') {
                stream.advance();
            }
            handleNewlineInCode();
            return;
        }

        if (c == '\n') {
            stream.advance();
            handleNewlineInCode();
            return;
        }

        // Whitespace (not newline)
        if (c == ' ' || c == '\t' || c == '\f') {
            stream.advance();
            skipWhitespace();
            emitSpaceIfNeeded();
            return;
        }

        // Flush any pending newline before emitting real content
        if (pendingNewline) {
            emitPendingNewline(c);
        }

        // Detect transitions: comments
        if (c == '/' && stream.peek(1) == '/') {
            stream.advance(); // skip /
            stream.advance(); // skip /
            state = State.SINGLE_LINE_COMMENT;
            return;
        }
        if (c == '/' && stream.peek(1) == '*') {
            stream.advance(); // skip /
            stream.advance(); // skip *
            state = State.MULTI_LINE_COMMENT;
            commentContainedNewline = false;
            return;
        }

        // Detect transitions: strings
        if (c == '\'') {
            stream.advance();
            stringStart = out.length();
            out.append('\'');
            state = State.SINGLE_QUOTE_STRING;
            lastTokenKind = TokenKind.IDENTIFIER; // string literal acts like an expression value
            return;
        }
        if (c == '"') {
            stream.advance();
            stringStart = out.length();
            out.append('"');
            state = State.DOUBLE_QUOTE_STRING;
            lastTokenKind = TokenKind.IDENTIFIER;
            return;
        }
        if (c == '`') {
            stream.advance();
            out.append('`');
            state = State.TEMPLATE_LITERAL;
            lastTokenKind = TokenKind.IDENTIFIER;
            return;
        }

        // Detect transitions: regex vs division
        if (c == '/') {
            if (isRegexStart()) {
                stream.advance();
                out.append('/');
                state = State.REGEX_LITERAL;
                return;
            }
            // It's division; fall through to emit as operator
        }

        // Template expression end: `}`
        if (c == '}' && !templateStack.isEmpty() && braceDepth == templateStack.peek()) {
            stream.advance();
            out.append('}');
            braceDepth--;
            templateStack.pop();
            state = State.TEMPLATE_LITERAL;
            lastTokenKind = TokenKind.IDENTIFIER; // template acts like expression
            return;
        }

        // Track braces for template nesting
        if (c == '{') {
            braceDepth++;
        } else if (c == '}') {
            braceDepth--;
        }

        // Identifiers and keywords
        if (isIdentStart(c)) {
            emitIdentifier();
            return;
        }

        // Numeric literals
        if (c >= '0' && c <= '9') {
            emitNumber();
            return;
        }

        // ++ and --
        if (c == '+' && stream.peek(1) == '+') {
            stream.advance();
            stream.advance();
            out.append("++");
            lastTokenKind = TokenKind.INCREMENT;
            return;
        }
        if (c == '-' && stream.peek(1) == '-') {
            stream.advance();
            stream.advance();
            out.append("--");
            lastTokenKind = TokenKind.DECREMENT;
            return;
        }

        // Punctuators / operators
        // Drop redundant semicolons before }
        if (c == '}') {
            while (!out.isEmpty() && out.charAt(out.length() - 1) == ';') {
                out.setLength(out.length() - 1);
            }
        }
        stream.advance();
        out.append(c);
        lastTokenKind = switch (c) {
            case ')' -> TokenKind.CLOSE_PAREN;
            case ']' -> TokenKind.CLOSE_BRACKET;
            case '}' -> TokenKind.CLOSE_BRACE;
            default -> TokenKind.OPERATOR;
        };
    }

    private void handleNewlineInCode() {
        skipWhitespaceAndNewlines();
        // Determine if this newline is ASI-significant
        if (isAsiSensitive()) {
            pendingNewline = true;
            return;
        }
        if (stream.hasMore()) {
            char next = stream.current();
            // Preserve newline if the next token is ++ or -- (prefix on next line)
            if ((next == '+' && stream.peek(1) == '+') ||
                    (next == '-' && stream.peek(1) == '-')) {
                pendingNewline = true;
                return;
            }
            // Guard space: prevent identifier merging or operator merging
            emitGuardSpace(next);
        }
    }

    private boolean isAsiSensitive() {
        // After restricted keywords: return, throw, continue, break, yield
        if (lastTokenKind == TokenKind.IDENTIFIER && ASI_KEYWORDS.contains(identBuf.toString())) {
            return true;
        }
        // After ++ or --
        return lastTokenKind == TokenKind.INCREMENT || lastTokenKind == TokenKind.DECREMENT;
    }

    private void emitPendingNewline(char nextChar) {
        pendingNewline = false;
        // Before ++ or -- (prefix on next line)
        boolean nextIsPlusPlusOrMinusMinus =
                (nextChar == '+' && stream.peek(1) == '+') ||
                (nextChar == '-' && stream.peek(1) == '-');

        // Newline needed if next token could cause ASI issues
        boolean needNewline = false;

        // After restricted keyword: always preserve if next line has an expression
        if (lastTokenKind == TokenKind.IDENTIFIER && ASI_KEYWORDS.contains(identBuf.toString())) {
            needNewline = true;
        }

        // After/before ++ --
        if (lastTokenKind == TokenKind.INCREMENT || lastTokenKind == TokenKind.DECREMENT) {
            needNewline = true;
        }
        if (nextIsPlusPlusOrMinusMinus) {
            needNewline = true;
        }

        if (needNewline) {
            out.append('\n');
        }
    }

    // ── COMMENT states ──────────────────────────────────────────────────

    private void processSingleLineComment() {
        char c = stream.current();
        if (c == '\n') {
            stream.advance();
            state = State.CODE;
            handleNewlineInCode();
            return;
        }
        if (c == '\r') {
            stream.advance();
            if (stream.hasMore() && stream.current() == '\n') {
                stream.advance();
            }
            state = State.CODE;
            handleNewlineInCode();
            return;
        }
        stream.advance(); // skip comment content
    }

    private void processMultiLineComment() {
        char c = stream.current();
        if (c == '\n' || c == '\r') {
            commentContainedNewline = true;
        }
        if (c == '*' && stream.peek(1) == '/') {
            stream.advance(); // skip *
            stream.advance(); // skip /
            state = State.CODE;
            if (commentContainedNewline) {
                commentContainedNewline = false;
                // Treat as if a newline occurred (for ASI and guard spaces)
                handleNewlineInCode();
            } else {
                // No newline in comment, but still need guard space check
                if (stream.hasMore()) {
                    emitGuardSpace(stream.current());
                }
            }
            return;
        }
        stream.advance(); // skip comment content
    }

    // ── STRING states ───────────────────────────────────────────────────

    private void processString(char quote) {
        char c = stream.advance();
        out.append(c);
        if (c == '\\') {
            // Emit the escaped character unconditionally
            if (stream.hasMore()) {
                out.append(stream.advance());
            }
            return;
        }
        if (c == quote) {
            optimizeStringQuotes(stringStart, quote);
            state = State.CODE;
            lastTokenKind = TokenKind.IDENTIFIER;
        }
    }

    private void optimizeStringQuotes(int start, char quote) {
        char otherQuote = (quote == '"') ? '\'' : '"';
        int end = out.length();
        int escapedSame = 0;
        int unescapedOther = 0;

        for (int i = start + 1; i < end - 1; i++) {
            char c = out.charAt(i);
            if (c == '\\' && i + 1 < end - 1) {
                char next = out.charAt(i + 1);
                if (next == quote) {
                    escapedSame++;
                }
                i++; // skip escaped char
            } else if (c == otherQuote) {
                unescapedOther++;
            }
        }

        if (escapedSame > unescapedOther) {
            StringBuilder rebuilt = new StringBuilder();
            rebuilt.append(otherQuote);
            for (int i = start + 1; i < end - 1; i++) {
                char c = out.charAt(i);
                if (c == '\\' && i + 1 < end - 1) {
                    char next = out.charAt(i + 1);
                    if (next == quote) {
                        // Unescape: \<quote> → <quote>
                        rebuilt.append(quote);
                    } else {
                        // Preserve other escape sequences as-is
                        rebuilt.append(c);
                        rebuilt.append(next);
                    }
                    i++;
                } else if (c == otherQuote) {
                    // Escape: <otherQuote> → \<otherQuote>
                    rebuilt.append('\\');
                    rebuilt.append(otherQuote);
                } else {
                    rebuilt.append(c);
                }
            }
            rebuilt.append(otherQuote);
            out.setLength(start);
            out.append(rebuilt);
        }
    }

    // ── TEMPLATE LITERAL state ──────────────────────────────────────────

    private void processTemplateLiteral() {
        char c = stream.advance();

        if (c == '\\') {
            out.append(c);
            if (stream.hasMore()) {
                out.append(stream.advance());
            }
            return;
        }

        if (c == '$' && stream.hasMore() && stream.current() == '{') {
            out.append(c);
            out.append(stream.advance()); // emit {
            braceDepth++;
            templateStack.push(braceDepth);
            state = State.CODE;
            lastTokenKind = TokenKind.OPERATOR; // ${ acts like an operator for regex disambiguation
            return;
        }

        out.append(c);
        if (c == '`') {
            state = State.CODE;
            lastTokenKind = TokenKind.IDENTIFIER;
        }
    }

    // ── REGEX LITERAL state ─────────────────────────────────────────────

    private void processRegex() {
        char c = stream.advance();
        out.append(c);

        if (c == '\\') {
            if (stream.hasMore()) {
                out.append(stream.advance());
            }
            return;
        }

        if (c == '[') {
            // Character class: read until unescaped ]
            processRegexCharClass();
            return;
        }

        if (c == '/') {
            // End of regex pattern — consume flags
            consumeRegexFlags();
            state = State.CODE;
            lastTokenKind = TokenKind.IDENTIFIER; // regex acts like an expression value
        }
    }

    private void processRegexCharClass() {
        while (stream.hasMore()) {
            char c = stream.advance();
            out.append(c);
            if (c == '\\' && stream.hasMore()) {
                out.append(stream.advance());
            } else if (c == ']') {
                return;
            }
        }
    }

    private void consumeRegexFlags() {
        while (stream.hasMore() && isIdentPart(stream.current())) {
            out.append(stream.advance());
        }
    }

    // ── Regex / Division disambiguation ─────────────────────────────────

    private boolean isRegexStart() {
        return switch (lastTokenKind) {
            case NONE, OPERATOR -> true;
            case IDENTIFIER -> {
                // After keywords like return, typeof, etc. → regex
                yield REGEX_PRECEDING_KEYWORDS.contains(identBuf.toString());
            }
            case CLOSE_PAREN, CLOSE_BRACKET, CLOSE_BRACE,
                 INCREMENT, DECREMENT, NUMBER_LITERAL -> false;
        };
    }

    // ── Whitespace helpers ──────────────────────────────────────────────

    private void skipWhitespace() {
        while (stream.hasMore()) {
            char c = stream.current();
            if (c == ' ' || c == '\t' || c == '\f') {
                stream.advance();
            } else {
                break;
            }
        }
    }

    private void skipWhitespaceAndNewlines() {
        while (stream.hasMore()) {
            char c = stream.current();
            if (c == ' ' || c == '\t' || c == '\f' || c == '\n' || c == '\r') {
                stream.advance();
            } else {
                break;
            }
        }
    }

    private void emitSpaceIfNeeded() {
        if (!stream.hasMore()) {
            return;
        }
        // Flush pending newline takes priority
        if (pendingNewline) {
            return;
        }
        emitGuardSpace(stream.current());
    }

    private void emitGuardSpace(char next) {
        if (out.isEmpty()) {
            return;
        }
        char prev = out.charAt(out.length() - 1);
        if (isIdentChar(prev) && isIdentChar(next)) {
            out.append(' ');
        } else if (needsSpaceBetween(prev, next)) {
            out.append(' ');
        }
    }

    private static boolean needsSpaceBetween(char prev, char next) {
        // Prevent ++ from + +
        if (prev == '+' && next == '+') return true;
        // Prevent -- from - -
        if (prev == '-' && next == '-') return true;
        // Prevent // (comment start) from / /
        if (prev == '/' && next == '/') return true;
        // Prevent /* (comment start) from / *
        if (prev == '/' && next == '*') return true;
        return false;
    }

    // ── Identifier / Number emission ────────────────────────────────────

    private void emitIdentifier() {
        identBuf.setLength(0);
        int outStart = out.length();
        while (stream.hasMore() && isIdentPart(stream.current())) {
            char c = stream.advance();
            out.append(c);
            identBuf.append(c);
        }
        // Literal shortening: true → !0, false → !1, undefined → void 0
        String replacement = switch (identBuf.toString()) {
            case "true" -> "!0";
            case "false" -> "!1";
            case "undefined" -> "void 0";
            case "Infinity" -> "(1/0)";
            default -> null;
        };
        if (replacement != null && (outStart == 0 || out.charAt(outStart - 1) != '.')) {
            out.setLength(outStart);
            // Remove guard space that is no longer needed for the shorter replacement
            if (outStart > 0 && out.charAt(outStart - 1) == ' ') {
                char replFirst = replacement.charAt(0);
                boolean spaceStillNeeded;
                if (outStart < 2) {
                    spaceStillNeeded = false;
                } else {
                    char beforeSpace = out.charAt(outStart - 2);
                    spaceStillNeeded = (isIdentChar(beforeSpace) && isIdentChar(replFirst))
                            || needsSpaceBetween(beforeSpace, replFirst);
                }
                if (!spaceStillNeeded) {
                    out.setLength(outStart - 1);
                }
            }
            out.append(replacement);
            identBuf.setLength(0);
            lastTokenKind = TokenKind.NUMBER_LITERAL;
            return;
        }
        lastTokenKind = TokenKind.IDENTIFIER;
    }

    private void emitNumber() {
        int outStart = out.length();
        char c = stream.advance();
        out.append(c);

        boolean isHex = false;
        boolean isSpecialRadix = false;
        boolean hasDot = false;
        boolean hasExponent = false;
        boolean isBigInt = false;

        // Handle 0x, 0o, 0b prefixes
        if (c == '0' && stream.hasMore()) {
            char next = stream.current();
            if (next == 'x' || next == 'X') {
                out.append(stream.advance());
                isHex = true;
                isSpecialRadix = true;
            } else if (next == 'o' || next == 'O' || next == 'b' || next == 'B') {
                out.append(stream.advance());
                isSpecialRadix = true;
            }
        }

        while (stream.hasMore()) {
            char ch = stream.current();
            if (ch >= '0' && ch <= '9') {
                out.append(stream.advance());
            } else if (isHex && ((ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))) {
                out.append(stream.advance());
            } else if (ch == '.' && !hasDot && !isHex) {
                // Only allow one dot, and not in hex numbers
                // Check next char — if it's a dot too (like 1..toString), stop
                if (stream.peek(1) == '.') {
                    break;
                }
                // Don't consume dot when followed by an identifier (property access),
                // e.g. 5.toFixed() → dot is property access, not decimal point.
                // Exception: 'e'/'E' after dot is an exponent indicator, not property.
                char afterDot = stream.peek(1);
                if (isIdentStart(afterDot) && afterDot != 'e' && afterDot != 'E') {
                    break;
                }
                hasDot = true;
                out.append(stream.advance());
            } else if ((ch == 'e' || ch == 'E') && !isHex) {
                hasExponent = true;
                out.append(stream.advance());
                // Consume optional sign after exponent
                if (stream.hasMore() && (stream.current() == '+' || stream.current() == '-')) {
                    out.append(stream.advance());
                }
            } else if (ch == '_') {
                // Numeric separator
                out.append(stream.advance());
            } else if (ch == 'n') {
                // BigInt suffix — consume and stop
                isBigInt = true;
                out.append(stream.advance());
                break;
            } else {
                break;
            }
        }
        if (hasDot && !isSpecialRadix && !isBigInt) {
            shortenDecimalNumber(outStart, hasExponent);
        }
        if (!isSpecialRadix && !isBigInt && !hasExponent) {
            shortenToScientific(outStart);
            // If scientific conversion succeeded and the stream is at "..",
            // the first dot is a trailing decimal dot (e.g. 1000..toString()).
            // Scientific notation makes it redundant — 1e3.toString() is
            // unambiguous — so skip the first dot.
            if (out.indexOf("e", outStart) >= 0
                    && stream.hasMore() && stream.current() == '.'
                    && stream.peek(1) == '.') {
                stream.advance(); // skip redundant trailing decimal dot
            }
        }
        lastTokenKind = TokenKind.NUMBER_LITERAL;
        identBuf.setLength(0);
    }

    private void shortenDecimalNumber(int outStart, boolean hasExponent) {
        String num = out.substring(outStart);

        // Find dot position
        int dotIdx = num.indexOf('.');
        if (dotIdx < 0) return;

        // Find exponent position
        int expIdx = -1;
        if (hasExponent) {
            for (int i = dotIdx + 1; i < num.length(); i++) {
                char ch = num.charAt(i);
                if (ch == 'e' || ch == 'E') {
                    expIdx = i;
                    break;
                }
            }
        }

        // Fractional part bounds
        int fracStart = dotIdx + 1;
        int fracEnd = expIdx >= 0 ? expIdx : num.length();

        // Strip trailing zeros and underscores from fractional part
        int newFracEnd = fracEnd;
        while (newFracEnd > fracStart) {
            char ch = num.charAt(newFracEnd - 1);
            if (ch == '0' || ch == '_') {
                newFracEnd--;
            } else {
                break;
            }
        }

        String intPart = num.substring(0, dotIdx);
        String fracPart = num.substring(fracStart, newFracEnd);
        String expPart = expIdx >= 0 ? num.substring(expIdx) : "";

        StringBuilder shortened = new StringBuilder();

        if (fracPart.isEmpty()) {
            // All fractional digits were zeros/underscores
            if (stream.hasMore() && stream.current() == '.') {
                // Keep dot for property access safety: 1.0.toString() → 1..toString()
                shortened.append(intPart).append('.').append(expPart);
            } else {
                // Remove dot entirely: 1.0 → 1
                shortened.append(intPart).append(expPart);
            }
        } else {
            // Leading zero removal: "0.5" → ".5"
            if (intPart.equals("0")) {
                shortened.append('.').append(fracPart).append(expPart);
            } else {
                shortened.append(intPart).append('.').append(fracPart).append(expPart);
            }
        }

        // Only replace if actually shorter
        if (shortened.length() < num.length()) {
            out.setLength(outStart);
            out.append(shortened);
        }
    }

    private void shortenToScientific(int outStart) {
        String num = out.substring(outStart);

        // If it contains a dot NOT at the final position, it has a fractional part — skip
        int dotIdx = num.indexOf('.');
        boolean hasTrailingDot = false;
        if (dotIdx >= 0) {
            if (dotIdx != num.length() - 1) {
                return; // real fractional part, not eligible
            }
            // Trailing dot (from shortenDecimalNumber property-access retention)
            hasTrailingDot = true;
            num = num.substring(0, dotIdx); // strip trailing dot for analysis
        }

        // Remove all numeric separators to get clean digit string
        String clean = num.replace("_", "");

        if (clean.isEmpty()) return;

        // Count trailing zeros
        int trailingZeros = 0;
        for (int i = clean.length() - 1; i >= 0; i--) {
            if (clean.charAt(i) == '0') {
                trailingZeros++;
            } else {
                break;
            }
        }

        // Guard: no trailing zeros, or ALL digits are zeros (e.g. "0")
        if (trailingZeros == 0 || trailingZeros >= clean.length()) return;

        // Build scientific form: <prefix>e<trailingZeroCount>
        String prefix = clean.substring(0, clean.length() - trailingZeros);
        String scientific = prefix + "e" + trailingZeros;

        // Only replace if strictly shorter than the original output
        String original = out.substring(outStart);
        if (scientific.length() < original.length()) {
            out.setLength(outStart);
            out.append(scientific);
        }
    }

    // ── Character classification ────────────────────────────────────────

    private static boolean isIdentStart(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '$' ||
                c == '#' || c > 0x7F;
    }

    private static boolean isIdentPart(char c) {
        return isIdentStart(c) || (c >= '0' && c <= '9');
    }

    private static boolean isIdentChar(char c) {
        return isIdentPart(c);
    }

    // ── Consecutive declaration merging (post-pass) ────────────────────

    static String mergeConsecutiveDeclarations(String input) {
        int len = input.length();
        if (len == 0) return input;

        StringBuilder result = new StringBuilder(len);

        // Track the current active declaration keyword ("var", "let", "const") or null
        String activeKeyword = null;
        // Brace depth where the active declaration started
        int activeBraceDepth = 0;

        int braces = 0;   // { }
        int parens = 0;    // ( )
        int brackets = 0;  // [ ]
        boolean inForParens = false; // inside for(...)

        int i = 0;
        while (i < len) {
            char c = input.charAt(i);

            // Skip string literals
            if (c == '\'' || c == '"') {
                int end = skipStringLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip template literals
            if (c == '`') {
                int end = skipTemplateLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip regex literals
            if (c == '/' && i + 1 < len && isRegexStartInPostPass(input, result)) {
                int end = skipRegexLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Track depth
            if (c == '(') {
                parens++;
                // Detect for( — check if "for" precedes this paren
                if (!inForParens && parens == 1 + countParensInStack(input, i)) {
                    // Simpler: check the word before '('
                    if (isForBefore(input, i)) {
                        inForParens = true;
                    }
                }
            } else if (c == ')') {
                if (inForParens && parens == 1) {
                    inForParens = false;
                }
                if (parens > 0) parens--;
            } else if (c == '[') {
                brackets++;
            } else if (c == ']') {
                if (brackets > 0) brackets--;
            } else if (c == '{') {
                // Closing brace boundary: reset active keyword if we're entering a new block
                braces++;
            } else if (c == '}') {
                if (braces > 0) braces--;
                // Reset active declaration only if we exit the block where it started
                if (activeKeyword != null && braces < activeBraceDepth) {
                    activeKeyword = null;
                }
            }

            // At semicolons (not inside for-parens, not inside parens/brackets),
            // check if the next token is a matching declaration keyword
            if (c == ';' && !inForParens && parens == 0 && brackets == 0
                    && activeKeyword != null && braces == activeBraceDepth) {
                // Look ahead past the semicolon for the same keyword
                int afterSemi = i + 1;
                String kw = activeKeyword;
                if (afterSemi + kw.length() < len
                        && input.startsWith(kw, afterSemi)
                        && afterSemi + kw.length() < len
                        && !isIdentPart(input.charAt(afterSemi + kw.length()))) {
                    // Replace ;keyword with ,
                    result.append(',');
                    i = afterSemi + kw.length();
                    // Skip optional space after keyword
                    if (i < len && input.charAt(i) == ' ') {
                        i++;
                    }
                    continue;
                }
                // Semicolon but next is not the same keyword — reset
                activeKeyword = null;
                result.append(c);
                i++;
                continue;
            }

            // Detect declaration keyword at current position (only when no active declaration
            // or when at top-level of current block and not inside parens/brackets)
            if (activeKeyword == null && parens == 0 && brackets == 0) {
                String kw = matchDeclKeyword(input, i, len);
                if (kw != null) {
                    // Make sure it's not preceded by an ident char (e.g., "avar")
                    if (i == 0 || !isIdentPart(input.charAt(i - 1))) {
                        activeKeyword = kw;
                        activeBraceDepth = braces;
                        result.append(kw);
                        i += kw.length();
                        // Skip optional space after keyword
                        if (i < len && input.charAt(i) == ' ') {
                            result.append(' ');
                            i++;
                        }
                        continue;
                    }
                }
            }

            result.append(c);
            i++;
        }

        return result.toString();
    }

    // ── Static string concatenation folding (post-pass) ────────────────

    static String foldStringConcatenation(String input) {
        int len = input.length();
        if (len == 0) return input;

        StringBuilder result = new StringBuilder(len);
        int i = 0;

        while (i < len) {
            char c = input.charAt(i);

            // Skip template literals
            if (c == '`') {
                int end = skipTemplateLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip regex literals
            if (c == '/' && i + 1 < len && isRegexStartInPostPass(input, result)) {
                int end = skipRegexLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Check for string literal that could start a fold chain
            if (c == '\'' || c == '"') {
                char quote = c;

                // Safety check: is it safe to fold based on what precedes?
                if (!isSafeBefore(result)) {
                    // Unsafe before — emit this string and continue
                    int end = skipStringLiteral(input, i);
                    result.append(input, i, end);
                    i = end;
                    continue;
                }

                // Read the first string
                int firstEnd = skipStringLiteral(input, i);
                // Raw content is between opening quote and closing quote
                String firstContent = input.substring(i + 1, firstEnd - 1);

                // Try to greedily extend the fold chain
                StringBuilder foldedContent = new StringBuilder(firstContent);
                int chainEnd = firstEnd;
                int foldCount = 1;

                while (chainEnd < len && input.charAt(chainEnd) == '+') {
                    int afterPlus = chainEnd + 1;
                    if (afterPlus >= len) break;
                    char nextChar = input.charAt(afterPlus);
                    if (nextChar != quote) break; // not same quote type or not a string

                    // Read candidate string
                    int candidateEnd = skipStringLiteral(input, afterPlus);
                    String candidateContent = input.substring(afterPlus + 1, candidateEnd - 1);

                    // Check if char after candidate is an unsafe higher-precedence operator
                    if (isUnsafeAfter(input, candidateEnd)) {
                        break; // stop folding, this string is captured by higher-precedence op
                    }

                    // Safe to fold this candidate
                    foldedContent.append(candidateContent);
                    chainEnd = candidateEnd;
                    foldCount++;
                }

                if (foldCount >= 2) {
                    // Emit folded string
                    result.append(quote);
                    result.append(foldedContent);
                    result.append(quote);
                } else {
                    // No folding happened, emit original string
                    result.append(input, i, firstEnd);
                }
                i = chainEnd;
                continue;
            }

            result.append(c);
            i++;
        }

        return result.toString();
    }

    private static boolean isSafeBefore(StringBuilder result) {
        if (result.isEmpty()) return true;
        char last = result.charAt(result.length() - 1);

        // Higher/same-precedence operators that capture the string
        if (last == '*' || last == '/' || last == '%' || last == '-'
                || last == '!' || last == '~') {
            return false;
        }

        // Disambiguate unary + from binary +
        // Unary + has higher precedence (16) than additive + (14), so folding
        // after unary + would change semantics: +"a"+"b" = (+"a")+"b" = "NaNb"
        // Binary + is safe (same precedence, left-assoc).
        // + is binary when preceded by an expression-ending token.
        if (last == '+') {
            // Skip back past the + and any whitespace (guard spaces)
            int j = result.length() - 2;
            while (j >= 0 && result.charAt(j) == ' ') j--;
            if (j < 0) return false; // + at start = unary
            char beforePlus = result.charAt(j);
            // Expression-ending tokens: ), ], identPart, string quotes, template
            if (beforePlus == ')' || beforePlus == ']' || beforePlus == '"'
                    || beforePlus == '\'' || beforePlus == '`'
                    || isIdentPart(beforePlus)) {
                return true; // binary +, safe
            }
            // ++ is postfix increment (expression-ending)
            if (beforePlus == '+') return true;
            // Everything else (operators, open parens, comma, etc.) = unary +
            return false;
        }

        // Check for unary keyword operators: typeof, void, delete, new
        if (isIdentPart(last)) {
            int end = result.length();
            int start = end - 1;
            while (start > 0 && isIdentPart(result.charAt(start - 1))) {
                start--;
            }
            String word = result.substring(start, end);
            return !word.equals("typeof") && !word.equals("void")
                    && !word.equals("delete") && !word.equals("new");
        }

        return true;
    }

    private static boolean isUnsafeAfter(String input, int pos) {
        if (pos >= input.length()) return false;
        char c = input.charAt(pos);
        // Higher-precedence arithmetic operators
        if (c == '*' || c == '/' || c == '%') return true;
        // Member access: dot, bracket, call, tagged template
        if (c == '.' || c == '[' || c == '(' || c == '`') return true;
        // Optional chaining ?. and ?.[ — but NOT ?? (nullish coalescing)
        // and NOT ? alone (ternary)
        if (c == '?' && pos + 1 < input.length()) {
            char next = input.charAt(pos + 1);
            if (next == '.' || next == '[') return true; // ?.  ?.[
            // ?? is nullish coalescing (low precedence) — safe
            // ? alone is ternary (low precedence) — safe
        }
        return false;
    }

    // ── Bracket-to-dot property access conversion (post-pass) ────────

    static String convertBracketToDot(String input) {
        int len = input.length();
        if (len == 0) return input;

        StringBuilder result = new StringBuilder(len);
        int i = 0;

        while (i < len) {
            char c = input.charAt(i);

            // Skip string literals
            if (c == '\'' || c == '"') {
                int end = skipStringLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip template literals
            if (c == '`') {
                int end = skipTemplateLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip regex literals
            if (c == '/' && i + 1 < len && isRegexStartInPostPass(input, result)) {
                int end = skipRegexLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Check for bracket-to-dot conversion
            if (c == '[' && !result.isEmpty()) {
                char prev = result.charAt(result.length() - 1);
                // Determine if preceding token ends an expression
                boolean canConvert = prev == ')' || prev == ']'
                        || prev == '`' || prev == '\'' || prev == '"';
                if (!canConvert && isIdentPart(prev)) {
                    // Identifier-like char: check it's not a bare integer or non-value keyword
                    canConvert = !endsWithBareInteger(result)
                            && !isPrecedingNonValueKeyword(result);
                }
                if (canConvert) {
                    int afterBracket = i + 1;
                    if (afterBracket < len) {
                        char q = input.charAt(afterBracket);
                        if (q == '\'' || q == '"') {
                            int strEnd = skipStringLiteral(input, afterBracket);
                            // Character after closing quote must be ]
                            if (strEnd < len && input.charAt(strEnd) == ']') {
                                String propName = input.substring(afterBracket + 1, strEnd - 1);
                                // Skip if contains backslash (escape sequences)
                                if (propName.indexOf('\\') < 0
                                        && isValidDotProperty(propName)) {
                                    result.append('.').append(propName);
                                    i = strEnd + 1; // skip past ]
                                    continue;
                                }
                            }
                        }
                    }
                }
            }

            result.append(c);
            i++;
        }

        return result.toString();
    }

    /**
     * Check if the result buffer ends with a bare decimal integer (no dot, no exponent,
     * no radix prefix). If so, appending "." would be ambiguous (could be decimal point).
     * Examples: 42.prop is a syntax error, but 1.5.prop, 1e3.prop, 0xff.prop are valid.
     */
    private static boolean endsWithBareInteger(StringBuilder sb) {
        int j = sb.length() - 1;
        if (j < 0) return false;

        char c = sb.charAt(j);
        if (c < '0' || c > '9') return false;

        // Scan backward through digits and numeric separators
        while (j >= 0) {
            c = sb.charAt(j);
            if ((c >= '0' && c <= '9') || c == '_') {
                j--;
            } else {
                break;
            }
        }

        // All digits from the start → bare integer (like "42")
        if (j < 0) return true;

        c = sb.charAt(j);

        // Check for exponent sign: +/- preceded by e/E (like 1e+3)
        if ((c == '+' || c == '-') && j > 0) {
            char before = sb.charAt(j - 1);
            if (before == 'e' || before == 'E') {
                return false; // Part of scientific notation
            }
        }

        // If preceded by a dot or any identifier start char
        // (covers exponent letters e/E, radix prefix x/X/o/O/b/B, identifier names)
        if (c == '.' || isIdentStart(c)) return false;

        // Otherwise it's a bare integer preceded by an operator/punctuator
        return true;
    }

    private static boolean isPrecedingNonValueKeyword(StringBuilder sb) {
        int end = sb.length();
        int start = end - 1;
        while (start > 0 && isIdentPart(sb.charAt(start - 1))) {
            start--;
        }
        String word = sb.substring(start, end);
        return NON_VALUE_KEYWORDS.contains(word);
    }

    private static boolean isValidDotProperty(String name) {
        if (name.isEmpty()) return false;
        char first = name.charAt(0);
        // # is in isIdentStart but obj.#x is only valid inside owning class body
        if (first == '#') return false;
        if (!isIdentStart(first)) return false;
        for (int i = 1; i < name.length(); i++) {
            if (!isIdentPart(name.charAt(i))) return false;
        }
        return true;
    }

    // ── Remove redundant return at end of function body (post-pass) ────

    static String removeRedundantReturn(String input) {
        int len = input.length();
        if (len == 0) return input;

        StringBuilder result = new StringBuilder(len);

        // Stack tracking which '{' opened a function body.
        // Each entry is the brace depth at the point that '{' was encountered.
        // We use a simple boolean stack: true = function body, false = other block.
        Deque<Boolean> braceStack = new ArrayDeque<>();

        // State: when we see 'function' keyword or '=>', we set a flag so the
        // next '{' is marked as a function body.
        boolean nextBraceIsFunctionBody = false;
        // For 'function' keyword, we need to skip past optional *, name, and params
        // before the next '{'. This tracks whether we're in that "skipping" state.
        boolean awaitingFunctionBrace = false;
        int funcParenDepth = 0; // tracks parens when skipping function params

        int i = 0;
        while (i < len) {
            char c = input.charAt(i);

            // Skip string literals
            if (c == '\'' || c == '"') {
                int end = skipStringLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip template literals
            if (c == '`') {
                int end = skipTemplateLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip regex literals
            if (c == '/' && i + 1 < len && isRegexStartInPostPass(input, result)) {
                int end = skipRegexLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Detect 'function' keyword
            if (c == 'f' && i + 8 <= len && input.startsWith("function", i)) {
                // Verify it's a keyword boundary (not part of a larger identifier)
                boolean prevOk = (i == 0 || !isIdentPart(input.charAt(i - 1)));
                boolean nextOk = (i + 8 >= len || !isIdentPart(input.charAt(i + 8)));
                if (prevOk && nextOk) {
                    // Emit 'function'
                    result.append("function");
                    i += 8;
                    awaitingFunctionBrace = true;
                    funcParenDepth = 0;
                    continue;
                }
            }

            // Detect '=>' for arrow functions
            if (c == '=' && i + 1 < len && input.charAt(i + 1) == '>') {
                result.append("=>");
                i += 2;
                // If next char is '{', mark it as function body
                if (i < len && input.charAt(i) == '{') {
                    nextBraceIsFunctionBody = true;
                }
                continue;
            }

            // When awaiting a function body brace, track parens for parameter list
            if (awaitingFunctionBrace) {
                if (c == '(') {
                    funcParenDepth++;
                    result.append(c);
                    i++;
                    continue;
                } else if (c == ')') {
                    funcParenDepth--;
                    result.append(c);
                    i++;
                    // After closing parens at depth 0, skip strings inside default params
                    continue;
                } else if (c == '{' && funcParenDepth == 0) {
                    // This is the function body opening brace
                    awaitingFunctionBrace = false;
                    nextBraceIsFunctionBody = true;
                    // Fall through to brace handling below
                } else {
                    // Skip over *, name, whitespace between 'function' and '('
                    // Also skip string literals inside default parameter values
                    result.append(c);
                    i++;
                    continue;
                }
            }

            // Track braces
            if (c == '{') {
                braceStack.push(nextBraceIsFunctionBody);
                nextBraceIsFunctionBody = false;
                result.append(c);
                i++;
                continue;
            }

            if (c == '}') {
                if (!braceStack.isEmpty()) {
                    boolean isFunctionBody = braceStack.pop();
                    if (isFunctionBody) {
                        tryRemoveTrailingReturn(result);
                    }
                }
                result.append(c);
                i++;
                continue;
            }

            result.append(c);
            i++;
        }

        return result.toString();
    }

    /**
     * If the result buffer ends with a redundant return pattern, remove it.
     * Patterns checked (longest first):
     *   - "return void 0" (13 chars)
     *   - "return\n"      (7 chars, bare return with ASI newline)
     *   - "return"         (6 chars, bare return)
     * Each requires the preceding char is a statement boundary ({, ;, }, \n)
     * or start of buffer — this prevents removing `return` when it's the
     * body of a braceless if/else/for/while (e.g. `if(x)return}`).
     * After removal, also strips a trailing ';'.
     */
    private static void tryRemoveTrailingReturn(StringBuilder sb) {
        int len = sb.length();

        // Try "return void 0" (13 chars)
        if (len >= 13) {
            String tail = sb.substring(len - 13);
            if (tail.equals("return void 0")) {
                int before = len - 13;
                if (isStatementBoundary(sb, before)) {
                    sb.setLength(before);
                    stripTrailingSemicolon(sb);
                    return;
                }
            }
        }

        // Try "return\n" (7 chars — bare return followed by newline for ASI)
        if (len >= 7) {
            String tail = sb.substring(len - 7);
            if (tail.equals("return\n")) {
                int before = len - 7;
                if (isStatementBoundary(sb, before)) {
                    sb.setLength(before);
                    stripTrailingSemicolon(sb);
                    return;
                }
            }
        }

        // Try "return" (6 chars)
        if (len >= 6) {
            String tail = sb.substring(len - 6);
            if (tail.equals("return")) {
                int before = len - 6;
                if (isStatementBoundary(sb, before)) {
                    sb.setLength(before);
                    stripTrailingSemicolon(sb);
                    return;
                }
            }
        }
    }

    /**
     * Check that position {@code pos} in the buffer is a statement boundary:
     * either start of buffer, or preceded by {, ;, }, or \n.
     * This ensures "return" is a standalone statement, not the body of a
     * braceless control structure like if(x)return.
     */
    private static boolean isStatementBoundary(StringBuilder sb, int pos) {
        if (pos == 0) return true;
        char c = sb.charAt(pos - 1);
        return c == '{' || c == ';' || c == '}' || c == '\n';
    }

    private static void stripTrailingSemicolon(StringBuilder sb) {
        if (!sb.isEmpty() && sb.charAt(sb.length() - 1) == ';') {
            sb.setLength(sb.length() - 1);
        }
    }

    // ── Arrow body shortening (post-pass) ─────────────────────────────

    static String shortenArrowBodies(String input) {
        int len = input.length();
        if (len == 0) return input;

        StringBuilder result = new StringBuilder(len);
        int i = 0;

        while (i < len) {
            char c = input.charAt(i);

            // Skip string literals
            if (c == '\'' || c == '"') {
                int end = skipStringLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip template literals
            if (c == '`') {
                int end = skipTemplateLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Skip regex literals
            if (c == '/' && i + 1 < len && isRegexStartInPostPass(input, result)) {
                int end = skipRegexLiteral(input, i);
                result.append(input, i, end);
                i = end;
                continue;
            }

            // Detect => followed by {return
            if (c == '=' && i + 1 < len && input.charAt(i + 1) == '>') {
                result.append("=>");
                i += 2;
                // Check for {return pattern
                if (i < len && input.charAt(i) == '{') {
                    int braceStart = i;
                    // Check if next chars are "return"
                    if (i + 7 <= len && input.startsWith("return", i + 1)) {
                        int afterReturn = i + 7; // position after "{return"
                        if (afterReturn < len) {
                            char nextCh = input.charAt(afterReturn);
                            int exprStart;
                            if (nextCh == ' ') {
                                exprStart = afterReturn + 1;
                            } else if (!isIdentPart(nextCh)) {
                                exprStart = afterReturn;
                            } else {
                                // It's something like "returnValue" — not our pattern
                                // We already appended "=>", just emit '{' and continue
                                result.append('{');
                                i++;
                                continue;
                            }
                            // Find the matching } for the opening {
                            int matchResult = findMatchingBrace(input, braceStart, exprStart);
                            if (matchResult >= 0) {
                                int closeBrace = matchResult >>> 1;
                                boolean hasTopLevelComma = (matchResult & 1) != 0;
                                // Extract the expression (without trailing semicolons)
                                String expr = input.substring(exprStart, closeBrace);
                                // Strip trailing semicolons
                                while (!expr.isEmpty() && expr.charAt(expr.length() - 1) == ';') {
                                    expr = expr.substring(0, expr.length() - 1);
                                }
                                if (expr.isEmpty()) {
                                    // =>{return} with no expr — shouldn't happen after removeRedundantReturn
                                    result.append(input, braceStart, closeBrace + 1);
                                    i = closeBrace + 1;
                                    continue;
                                }
                                // Recursively shorten nested arrows in the expression
                                expr = shortenArrowBodies(expr);
                                // Determine if wrapping in parens is needed
                                boolean needsParens = false;
                                if (expr.charAt(0) == '{') {
                                    needsParens = true; // object literal
                                } else if (hasTopLevelComma) {
                                    needsParens = true; // comma operator
                                }
                                if (needsParens) {
                                    result.append('(').append(expr).append(')');
                                } else {
                                    result.append(expr);
                                }
                                i = closeBrace + 1;
                                continue;
                            }
                        }
                    }
                }
                continue;
            }

            result.append(c);
            i++;
        }

        return result.toString();
    }

    /**
     * Find the matching closing brace for the opening brace at bracePos.
     * Scans from exprStart (after the "return " part).
     * Returns -1 if the body contains multiple statements (semicolon at top-level before the close).
     * Otherwise returns (closeBracePos << 1) | hasTopLevelComma.
     */
    private static int findMatchingBrace(String input, int bracePos, int exprStart) {
        int len = input.length();
        int curlyDepth = 1; // the opening { at bracePos
        int parenDepth = 0;
        int bracketDepth = 0;
        boolean hasTopLevelComma = false;

        int i = exprStart;
        while (i < len) {
            char c = input.charAt(i);

            // Skip string literals
            if (c == '\'' || c == '"') {
                i = skipStringLiteral(input, i);
                continue;
            }

            // Skip template literals
            if (c == '`') {
                i = skipTemplateLiteral(input, i);
                continue;
            }

            // Skip regex literals
            if (c == '/' && i + 1 < len && isRegexStartAtPosition(input, i)) {
                i = skipRegexLiteral(input, i);
                continue;
            }

            if (c == '(') { parenDepth++; }
            else if (c == ')') { if (parenDepth > 0) parenDepth--; }
            else if (c == '[') { bracketDepth++; }
            else if (c == ']') { if (bracketDepth > 0) bracketDepth--; }
            else if (c == '{') { curlyDepth++; }
            else if (c == '}') {
                curlyDepth--;
                if (curlyDepth == 0) {
                    // Found matching brace
                    return (i << 1) | (hasTopLevelComma ? 1 : 0);
                }
            }

            // Semicolon at top level of the arrow body means multiple statements
            if (c == ';' && curlyDepth == 1 && parenDepth == 0 && bracketDepth == 0) {
                // Check if this semicolon is followed by the closing brace (trailing semicolon)
                int next = i + 1;
                if (next < len && input.charAt(next) == '}') {
                    // Trailing semicolon before } — that's fine, single statement
                    i++;
                    continue;
                }
                return -1; // multiple statements
            }

            // Track top-level commas (inside the arrow body's single expression)
            if (c == ',' && curlyDepth == 1 && parenDepth == 0 && bracketDepth == 0) {
                hasTopLevelComma = true;
            }

            i++;
        }
        return -1; // unmatched
    }

    /**
     * Regex/division disambiguation for forward scanning in post-passes.
     * Looks at the character before pos to determine if / starts a regex.
     */
    private static boolean isRegexStartAtPosition(String input, int pos) {
        if (pos == 0) return true;
        char prev = input.charAt(pos - 1);
        if (prev == ')' || prev == ']' || prev == '}') return false;
        if (isIdentPart(prev)) {
            int start = pos - 1;
            while (start > 0 && isIdentPart(input.charAt(start - 1))) {
                start--;
            }
            String word = input.substring(start, pos);
            return REGEX_PRECEDING_KEYWORDS.contains(word);
        }
        return true;
    }

    private static String matchDeclKeyword(String input, int pos, int len) {
        if (pos + 3 < len && input.startsWith("var", pos) && !isIdentPart(input.charAt(pos + 3))) {
            return "var";
        }
        if (pos + 3 < len && input.startsWith("let", pos) && !isIdentPart(input.charAt(pos + 3))) {
            return "let";
        }
        if (pos + 5 < len && input.startsWith("const", pos) && !isIdentPart(input.charAt(pos + 5))) {
            return "const";
        }
        return null;
    }

    private static boolean isForBefore(String input, int parenPos) {
        // Check if the characters before '(' spell "for"
        int j = parenPos - 1;
        // skip optional space
        while (j >= 0 && input.charAt(j) == ' ') j--;
        if (j >= 2 && input.charAt(j) == 'r' && input.charAt(j - 1) == 'o' && input.charAt(j - 2) == 'f') {
            // Make sure 'f' is not part of a longer identifier
            if (j - 2 == 0 || !isIdentPart(input.charAt(j - 3))) {
                return true;
            }
        }
        return false;
    }

    private static int countParensInStack(String input, int pos) {
        // Not needed for our simplified approach — always check parens == 0 for merging
        return 0;
    }

    private static int skipStringLiteral(String input, int start) {
        char quote = input.charAt(start);
        int i = start + 1;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\\' && i + 1 < len) {
                i += 2; // skip escape
            } else if (c == quote) {
                return i + 1;
            } else {
                i++;
            }
        }
        return len;
    }

    private static int skipTemplateLiteral(String input, int start) {
        int i = start + 1;
        int len = input.length();
        int depth = 0;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\\' && i + 1 < len) {
                i += 2;
            } else if (c == '$' && i + 1 < len && input.charAt(i + 1) == '{') {
                depth++;
                i += 2;
            } else if (c == '}' && depth > 0) {
                depth--;
                i++;
            } else if (c == '`' && depth == 0) {
                return i + 1;
            } else if (c == '`' && depth > 0) {
                // Nested template literal inside expression — recurse
                int end = skipTemplateLiteral(input, i);
                i = end;
            } else {
                i++;
            }
        }
        return len;
    }

    private static int skipRegexLiteral(String input, int start) {
        int i = start + 1; // skip opening /
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\\' && i + 1 < len) {
                i += 2;
            } else if (c == '[') {
                // Character class — skip until ]
                i++;
                while (i < len) {
                    char cc = input.charAt(i);
                    if (cc == '\\' && i + 1 < len) {
                        i += 2;
                    } else if (cc == ']') {
                        i++;
                        break;
                    } else {
                        i++;
                    }
                }
            } else if (c == '/') {
                i++; // skip closing /
                // Consume flags
                while (i < len && isIdentPart(input.charAt(i))) {
                    i++;
                }
                return i;
            } else {
                i++;
            }
        }
        return len;
    }

    private static boolean isRegexStartInPostPass(String input, StringBuilder result) {
        // Simple heuristic: check the last non-space character in result
        if (result.isEmpty()) return true;
        int j = result.length() - 1;
        char prev = result.charAt(j);
        // After these, '/' is division, not regex
        if (prev == ')' || prev == ']' || prev == '}') return false;
        if (isIdentPart(prev)) {
            // Could be keyword or identifier — check if it's a regex-preceding keyword
            int start = j;
            while (start > 0 && isIdentPart(result.charAt(start - 1))) {
                start--;
            }
            String word = result.substring(start, j + 1);
            return REGEX_PRECEDING_KEYWORDS.contains(word);
        }
        // After operators, it's regex
        return true;
    }
}
