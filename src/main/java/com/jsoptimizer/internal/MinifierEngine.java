package com.jsoptimizer.internal;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Set;

public final class MinifierEngine {

    private static final Set<String> REGEX_PRECEDING_KEYWORDS = Set.of(
            "return", "typeof", "instanceof", "in", "delete", "void",
            "throw", "new", "case", "yield", "await", "of"
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
        return out.toString();
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
            out.append('\'');
            state = State.SINGLE_QUOTE_STRING;
            lastTokenKind = TokenKind.IDENTIFIER; // string literal acts like an expression value
            return;
        }
        if (c == '"') {
            stream.advance();
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
            state = State.CODE;
            lastTokenKind = TokenKind.IDENTIFIER;
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
        char c = stream.advance();
        out.append(c);

        boolean isHex = false;
        boolean hasDot = false;

        // Handle 0x, 0o, 0b prefixes
        if (c == '0' && stream.hasMore()) {
            char next = stream.current();
            if (next == 'x' || next == 'X') {
                out.append(stream.advance());
                isHex = true;
            } else if (next == 'o' || next == 'O' || next == 'b' || next == 'B') {
                out.append(stream.advance());
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
                hasDot = true;
                out.append(stream.advance());
            } else if ((ch == 'e' || ch == 'E') && !isHex) {
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
                out.append(stream.advance());
                break;
            } else {
                break;
            }
        }
        lastTokenKind = TokenKind.NUMBER_LITERAL;
        identBuf.setLength(0);
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
}
