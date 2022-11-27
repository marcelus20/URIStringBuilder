package com.marcelus.uristringbuilder.utils;

/**
 * Enum containing all URL related constants for aiding the Uri String Builders.
 */
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

    /**
     * All args constructor.
     * @param value the value to be assiged to the field.
     */
    UriPortionConstants(final String value) {
        this.value = value;
    }

    /**
     * Getter.
     * @return the value field.
     */
    public String getValue() {
        return value;
    }
}
