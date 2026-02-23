package com.jsoptimizer.internal;

public final class CharStream {

    private final String input;
    private int pos;

    public CharStream(String input) {
        this.input = input;
        this.pos = 0;
    }

    public boolean hasMore() {
        return pos < input.length();
    }

    public char current() {
        return input.charAt(pos);
    }

    public char advance() {
        return input.charAt(pos++);
    }

    public char peek(int offset) {
        int idx = pos + offset;
        if (idx < 0 || idx >= input.length()) {
            return '\0';
        }
        return input.charAt(idx);
    }

    public int position() {
        return pos;
    }

    public int length() {
        return input.length();
    }
}
