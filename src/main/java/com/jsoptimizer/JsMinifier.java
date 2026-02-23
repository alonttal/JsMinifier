package com.jsoptimizer;

import com.jsoptimizer.internal.MinifierEngine;

public final class JsMinifier {

    private JsMinifier() {
    }

    public static String minify(String input) {
        if (input == null || input.isEmpty()) {
            return "";
        }
        return new MinifierEngine(input).minify();
    }
}
