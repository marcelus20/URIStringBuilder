package com.marcelus.uristringbuilder.structuredstring;

import com.marcelus.uristringbuilder.uribuilders.StructuredBuildableUri;
import com.marcelus.uristringbuilder.utils.QueryTrimmers;
import com.marcelus.uristringbuilder.utils.SlashTrimmers;
import com.marcelus.uristringbuilder.utils.StringUtils;

import java.util.Map;
import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.StringUtils.convertObjectToString;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_AND;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_COMPOSITION_FORMAT;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_EQUALS;

/**
 * Class to append different parts of a URL in a structured way.
 * Each part of the URL will be assigned to its corresponding field, and all of them shall be concatenated
 * when the build method is invoked.
 */
public final class StructuredUriStringBuilder implements StructuredBuildableUri {

    private final String scheme;
    private final String host;
    private final String port;
    private final String path;
    private final String query;

    /**
     * All args constructor. No logic, just field initialisation.
     * @param scheme the scheme protocol.
     * @param host the host portion.
     * @param port the port portion.
     * @param path the path portion.
     * @param query the query string portion.
     */
    private StructuredUriStringBuilder(String scheme, String host, String port, String path, String query) {
        this.scheme = scheme;
        this.host = host;
        this.port = port;
        this.path = path;
        this.query = query;
    }

    /**
     * No args constructor. Initialises all variables with an empty string.
     */
    public StructuredUriStringBuilder() {
        this.scheme = "";
        this.host = "";
        this.port = "";
        this.path = "";
        this.query = "";
    }

    /**
     * Takes a scheme portion string and populates or appends it to the current scheme field.
     * @param scheme the scheme protocol.
     * @return a new object with the new state.
     */
    @Override
    public StructuredUriStringBuilder appendScheme(final String scheme){
        return Optional.ofNullable(scheme)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sScheme->new StructuredUriStringBuilder(mergeSchemes(this.scheme,sScheme), host, port, path, query))
                .orElse(new StructuredUriStringBuilder(this.scheme,host, port, path, query));
    }

    /**
     * Takes a scheme portion integer, converts it into a string, and populates or appends it to the current scheme field.
     * @param scheme the scheme protocol.
     * @return a new object with the new state.
     */
    @Override
    public StructuredBuildableUri appendScheme(Integer scheme) {
        return convertObjectToString(scheme)
                .map(this::appendScheme)
                .orElse(new StructuredUriStringBuilder(this.scheme,host, port, path, query));
    }

    /**
     * Takes a host portion string and populates or appends it to the current host field.
     * @param host the host potion.
     * @return a new object with the new state.
     */
    @Override
    public StructuredUriStringBuilder appendHost(final String host){
        return Optional.ofNullable(host)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sHost->new StructuredUriStringBuilder(scheme, mergeHosts(this.host, sHost), port, path, query))
                .orElse(new StructuredUriStringBuilder(scheme, this.host, port, path, query));
    }

    /**
     * Takes a host portion integer, converts it into a string, and populates or appends it to the current host field.
     * @param host the host portion.
     * @return a new object with the new state.
     */
    @Override
    public StructuredBuildableUri appendHost(Integer host) {
        return convertObjectToString(host)
                .map(this::appendHost)
                .orElse(new StructuredUriStringBuilder(scheme, this.host, port, path, query));
    }

    /**
     * Takes a port portion string and populates or appends it to the current port field.
     * @param port the port portion.
     * @return a new object with the new state.
     */
    @Override
    public StructuredUriStringBuilder appendPort(final String port){
        return Optional.ofNullable(port)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sPort->new StructuredUriStringBuilder(scheme, host, mergePorts(this.port, sPort), path, query))
                .orElse(new StructuredUriStringBuilder(scheme, host, this.port, path, query));
    }

    /**
     * Takes a port portion integer, converts it into a string, and populates or appends it to the current port field.
     * @param port the port portion.
     * @return a new object with the new state.
     */
    @Override
    public StructuredBuildableUri appendPort(Integer port) {
        return convertObjectToString(port)
                .map(this::appendPort)
                .orElse(new StructuredUriStringBuilder(scheme, host, this.port, path, query));
    }

    /**
     * Takes a path portion string and populates or appends it to the current path field.
     * @param path the path portion.
     * @return a new object with the new state.
     */
    @Override
    public StructuredUriStringBuilder appendPath(final String path){
        return Optional.ofNullable(path)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(SlashTrimmers::trimSlashes)
                .map(sPath->new StructuredUriStringBuilder(scheme, host, port, mergePaths(this.path, sPath), query))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, this.path, query));
    }

    /**
     * Takes a path portion integer, converts it into a string, and populates or appends it to the current path field.
     * @param path the path portion.
     * @return a new object with the new state.
     */
    @Override
    public StructuredBuildableUri appendPath(Integer path) {
        return convertObjectToString(path)
                .map(this::appendPath)
                .orElse(new StructuredUriStringBuilder(scheme, host, port, this.path, query));
    }


    /**
     * Takes a query key string and query value string and merge them together in a query string format (eg: ?key=value).
     * @param key The key portion of the query.
     * @param value The value portion of the query.
     * @return a new object containing the whole query string up to this point.
     */
    @Override
    public StructuredUriStringBuilder appendQuery(final String key, final String value){
        return convertObjectToString(key)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .flatMap(sKey->convertObjectToString(value)
                        .map(StringUtils::replaceBlankSpaceWithPlusIcon)
                        .map(sValue->new StructuredUriStringBuilder(scheme, host, port, path,
                                mergeQueries(this.query, QueryTrimmers
                                        .trimQueryComponents(sKey) + QUERY_EQUALS.getValue() +
                                        QueryTrimmers.trimQueryComponents(sValue)))))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    /**
     * Takes a query key string and query value integer, converts the value into a string and merge them together in a
     * query string format (eg: ?ticketNumber=1234).
     * @param key the key portion of the query.
     * @param value the integer value portion of the query that will be converted into a string.
     * @return a new object containing the whole query string up to this point.
     */
    @Override
    public StructuredBuildableUri appendQuery(String key, Integer value) {
        return convertObjectToString(value)
                .map(stringValue->appendQuery(key, value))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    /**
     * Takes a query key integer and query value string, converts the key into a string and merge them together in a
     * query string format (eg: ?12=foo).
     * @param key the integer key portion of the query that will be converted into a string. .
     * @param value the value portion of the query.
     * @return a new object containing the whole query string up to this point.
     */
    @Override
    public StructuredBuildableUri appendQuery(Integer key, String value) {
        return convertObjectToString(key)
                .map(stringValue->appendQuery(key, value))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    /**
     * Takes a query key integer and query value integer, converts them into a strings and merge them together in a
     * query string format (eg: ?12=111).
     * @param key the integer key portion of the query that will be converted into a string. .
     * @param value the integer value portion of the query that will be converted into a string.
     * @return a new object containing the whole query string up to this point.
     */
    @Override
    public StructuredBuildableUri appendQuery(Integer key, Integer value) {
        return convertObjectToString(key)
                .map(stringValue->appendQuery(key, value))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    /**
     * Takes a map and turns it into a query string by mapping the keys of the map to the keys of the query string and values
     * of the map to the values of the query string.
     * @param map the map String to String type that represents the set of key value pairs that composes the query string.
     * @return a new object with the new state.
     */
    @Override
    public StructuredBuildableUri appendQuery(Map<String, String> map) {
        return Optional.ofNullable(map)
                .map(nonNullMap->nonNullMap.entrySet().stream()
                        .map(entry->String.format(QUERY_COMPOSITION_FORMAT.getValue(),entry.getKey() == null? "": entry.getKey(),
                                entry.getKey() == null ? "" : QUERY_EQUALS.getValue(),
                                entry.getKey() == null || entry.getValue() == null? "": entry.getValue()))
                        .reduce("",(acc,entry)->String.format(QUERY_COMPOSITION_FORMAT.getValue(),
                                acc, entry.isEmpty()?"":QUERY_AND.getValue(),
                                entry))
                ).map(this::appendQuery)
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    /**
     * Takes the queryPortion and appends it to the query field. The portion can consist of a standalone key, key or key
     * with value. If that's the initial phase of the query string, it will add the question tag before the key.
     * @param queryPortion the portion to be appended to the current query field.
     * @return a new object with the new state.
     */
    @Override
    public StructuredUriStringBuilder appendQuery(final String queryPortion) {
        return convertObjectToString(queryPortion)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sQuery->new StructuredUriStringBuilder(scheme, host, port, path, mergeQueries(this.query, sQuery)))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    /**
     * Takes an integer queryPortion, converts into a string and append it to the current query field.
     * @param queryPortion the value to be converted into a string and appended to the current field.
     * @return a new object with the new stage.
     */
    @Override
    public StructuredBuildableUri appendQuery(Integer queryPortion) {
        return Optional.ofNullable(queryPortion)
                .map(this::appendQuery)
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    /**
     * Composes the scheme by joining the current scheme with the new scheme portion.
     * @param currentScheme the current scheme class field.
     * @param newSchemePortion the new portion to be appended to the current scheme class field.
     * @return a resulting string with the two values joined.
     */
    private String mergeSchemes(String currentScheme, String newSchemePortion) {
        return String.format("%s%s",currentScheme, newSchemePortion);
    }

    /**
     * Composes the host by joining the current host with the new host portion.
     * @param currentHost the current host value supplied by the class field.
     * @param newHostPortion the new value to be appended to the current host.
     * @return a resulting string with the two values joined.
     */
    private String mergeHosts(String currentHost, String newHostPortion) {
        return String.format("%s%s",currentHost, newHostPortion);
    }

    /**
     * Composes the port by joining the current port with the new port portion.
     * @param currentPort the current port value supplied by the class field.
     * @param newPortPortion the new port value to be appended to the current port.
     * @return a resulting string with the two values joined.
     */
    private String mergePorts(final String currentPort, final String newPortPortion) {
        if(currentPort.isEmpty()) return String.format(":%s", newPortPortion);
        return String.format("%s%s", currentPort, newPortPortion);
    }

    /**
     * Composes the path by joining the current path with the new path portion.
     * @param currentPath the current path value supplied by the class field.
     * @param newPathPortion the new path value to be appended to the current port.
     * @return a resulting string with the two values joined.
     */
    private String mergePaths(String currentPath, String newPathPortion) {
        if(currentPath.isEmpty()) return String.format("/%s", newPathPortion);
        return String.format("%s/%s", currentPath, newPathPortion);
    }

    /**
     * Composes the query by joining the current query with the new query portion.
     * @param currentQuery the current query supplied by the class field.
     * @param newQueryPortion the new query portion to be appended to the current query.
     * @return a resulting string with the two values joined.
     */
    private String mergeQueries(final String currentQuery, final String newQueryPortion) {
        return Optional.ofNullable(currentQuery)
                .flatMap(nonNullCurrentQuery->Optional.ofNullable(newQueryPortion))
                .map(QueryTrimmers::trimQueryAnd)
                .map(QueryTrimmers::trimQueryEquals)
                .map(QueryTrimmers::trimQuery)
                .map(trimmedNewQuery->{
                    if(currentQuery.isEmpty()) return String.format("%s%s",
                            trimmedNewQuery.isEmpty()? "" : QUERY.getValue(),
                            trimmedNewQuery);
                    return String.format(QUERY_COMPOSITION_FORMAT.getValue(),
                            currentQuery,
                            trimmedNewQuery.isEmpty()? "" : QUERY_AND.getValue(),
                            trimmedNewQuery);
                })
                .orElse("");
    }

    /**
     * Joins all fields together in a URI formatted string.
     * @return an url formatted string composed by the fields.
     */
    @Override
    public String build(){
        if(scheme.isEmpty()) return String.format("%s%s%s%s", host, port, path, query);
        return String.format("%s://%s%s%s%s", scheme, host, port, host.isEmpty() ? SlashTrimmers.trimSlashAtStart(path)
                .orElse("") : path, query);
    }

    @Override
    public String toString(){
        return build();
    }
}
