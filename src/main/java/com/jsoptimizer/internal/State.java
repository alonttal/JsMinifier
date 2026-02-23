package com.jsoptimizer.internal;

public enum State {
    CODE,
    SINGLE_LINE_COMMENT,
    MULTI_LINE_COMMENT,
    SINGLE_QUOTE_STRING,
    DOUBLE_QUOTE_STRING,
    TEMPLATE_LITERAL,
    REGEX_LITERAL
}
