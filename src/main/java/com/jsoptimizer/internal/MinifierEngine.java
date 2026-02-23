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
        // Also preserve newline if the next token is ++ or -- (prefix on next line)
        if (stream.hasMore()) {
            char next = stream.current();
            if ((next == '+' && stream.peek(1) == '+') ||
                    (next == '-' && stream.peek(1) == '-')) {
                pendingNewline = true;
            }
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
            skipWhitespaceAndNewlines();
            // Single-line comment ending acts like a newline for ASI
            if (isAsiSensitive()) {
                pendingNewline = true;
            }
            return;
        }
        if (c == '\r') {
            stream.advance();
            if (stream.hasMore() && stream.current() == '\n') {
                stream.advance();
            }
            state = State.CODE;
            skipWhitespaceAndNewlines();
            if (isAsiSensitive()) {
                pendingNewline = true;
            }
            return;
        }
        stream.advance(); // skip comment content
    }

    private void processMultiLineComment() {
        char c = stream.current();
        if (c == '*' && stream.peek(1) == '/') {
            stream.advance(); // skip *
            stream.advance(); // skip /
            state = State.CODE;
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
        char next = stream.current();
        // Check if the last emitted char and the next char are both identifier chars
        if (out.isEmpty()) {
            return;
        }
        char prev = out.charAt(out.length() - 1);
        if (isIdentChar(prev) && isIdentChar(next)) {
            out.append(' ');
        }
    }

    // ── Identifier / Number emission ────────────────────────────────────

    private void emitIdentifier() {
        identBuf.setLength(0);
        while (stream.hasMore() && isIdentPart(stream.current())) {
            char c = stream.advance();
            out.append(c);
            identBuf.append(c);
        }
        lastTokenKind = TokenKind.IDENTIFIER;
    }

    private void emitNumber() {
        char c = stream.advance();
        out.append(c);

        // Handle 0x, 0o, 0b prefixes
        if (c == '0' && stream.hasMore()) {
            char next = stream.current();
            if (next == 'x' || next == 'X' || next == 'o' || next == 'O' ||
                    next == 'b' || next == 'B') {
                out.append(stream.advance());
            }
        }

        while (stream.hasMore()) {
            char ch = stream.current();
            if ((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F') ||
                    ch == '.' || ch == 'e' || ch == 'E' || ch == '+' || ch == '-' ||
                    ch == '_' || ch == 'n') {
                // Numeric separators (_), BigInt suffix (n), hex digits, exponents
                // Be careful: + and - only after e/E
                if ((ch == '+' || ch == '-') && out.length() > 0) {
                    char prev = out.charAt(out.length() - 1);
                    if (prev != 'e' && prev != 'E') {
                        break;
                    }
                }
                out.append(stream.advance());
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
