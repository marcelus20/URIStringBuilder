package com.marcelus.uristringbuilder.uribuilders;

public interface StructuredBuildableUri extends BuildableUri {

    StructuredBuildableUri appendScheme(final Object scheme);
    StructuredBuildableUri appendHost(final Object host);
    StructuredBuildableUri appendPort(final Object port);
    StructuredBuildableUri appendPath(final Object path);
    StructuredBuildableUri appendQuery(final Object query);
    StructuredBuildableUri appendQuery(final Object key, Object value);

}
