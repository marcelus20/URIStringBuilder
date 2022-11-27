package com.marcelus.uristringbuilder.utils;

public enum UriPortionConstants {
    PORT_REGEX(".*:\\d+.*"),
    POST_SCHEME_PORTION("://"),
    DEFAULT_SCHEME("https"),
    PATH_SLASH("/"),
    QUERY("?"),
    QUERY_EQUALS("="),
    QUERY_AND("&"),
    PORT_START_DELIMITER(":"),
    QUERY_COMPOSITION_FORMAT("%s%s%s");

    private final String value;

    UriPortionConstants(final String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
