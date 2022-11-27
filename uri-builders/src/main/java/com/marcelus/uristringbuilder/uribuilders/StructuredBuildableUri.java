package com.marcelus.uristringbuilder.uribuilders;

import java.util.Map;

/**
 * The structured buildable URI interface.
 * @param <T> The type of object it should build.
 */
public interface StructuredBuildableUri<T> extends BuildableUri<T> {

    /**
     * Appends a scheme to the structure.
     * @param scheme the scheme object to compose structure.
     * @return a new object with the new scheme.
     */
    StructuredBuildableUri<T> appendScheme(final T scheme);

    /**
     * Appends a host to the structure.
     * @param host the host object to compose structure.
     * @return a new object with the new host.
     */
    StructuredBuildableUri<T> appendHost(final T host);

    /**
     * Appends a port to the structure.
     * @param port the host object to compose structure.
     * @return a new object with the new port.
     */
    StructuredBuildableUri<T> appendPort(final T port);

    /**
     * Appends a path to the structure.
     * @param path the host object to compose structure.
     * @return a new object with the new path.
     */
    StructuredBuildableUri<T> appendPath(final T path);

    /**
     * Appends a query to the structure.
     * @param query the host object to compose structure.
     * @return a new object with the new query.
     */
    StructuredBuildableUri<T> appendQuery(final T query);

    /**
     * Appends a query based on the key and value pair to the structure.
     * @param key the key portion of the query.
     * @param value the value portion of the query.
     * @return new object with the new query.
     */
    StructuredBuildableUri<T> appendQuery(final T key, T value);

    /**
     * Appends a query based on the on a map of key value pairs.
     * @param map the mapping of the key value pairs.
     * @return new object with the new query composed of the mapping.
     */
    StructuredBuildableUri<T> appendQuery(final Map<T, T> map);

}
