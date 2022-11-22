package com.marcelus.uristringbuilder.uribuilders;

public interface StructuredBuildableUri extends BuildableUri {

    StructuredBuildableUri appendScheme(final String scheme);
    StructuredBuildableUri appendScheme(final Integer scheme);
    StructuredBuildableUri appendHost(final String host);
    StructuredBuildableUri appendHost(final Integer host);
    StructuredBuildableUri appendPort(final String port);
    StructuredBuildableUri appendPort(final Integer port);
    StructuredBuildableUri appendPath(final String path);
    StructuredBuildableUri appendPath(final Integer path);
    StructuredBuildableUri appendQuery(final String query);
    StructuredBuildableUri appendQuery(final Integer query);
    StructuredBuildableUri appendQuery(final String key, String value);
    StructuredBuildableUri appendQuery(final String key, Integer value);
    StructuredBuildableUri appendQuery(final Integer key, String value);
    StructuredBuildableUri appendQuery(final Integer key, Integer value);

}
