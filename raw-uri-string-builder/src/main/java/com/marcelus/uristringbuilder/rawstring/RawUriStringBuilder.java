package com.marcelus.uristringbuilder.rawstring;


public class RawUriStringBuilder {
    private final String url;

    public RawUriStringBuilder(final String url) {
        this.url = url;
    }

    public RawUriStringBuilder() {
        this.url = "";
    }

    public RawUriStringBuilder append(final Object urlPortion) {
        return new RawUriStringBuilder(url + urlPortion);
    }

    public String build() {
        return url;
    }
}
