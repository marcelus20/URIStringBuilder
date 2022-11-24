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
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_EQUALS;

public final class StructuredUriStringBuilder implements StructuredBuildableUri {

    private final String scheme;
    private final String host;
    private final String port;
    private final String path;
    private final String query;

    private StructuredUriStringBuilder(String scheme, String host, String port, String path, String query) {
        this.scheme = scheme;
        this.host = host;
        this.port = port;
        this.path = path;
        this.query = query;
    }

    public StructuredUriStringBuilder() {
        this.scheme = "";
        this.host = "";
        this.port = "";
        this.path = "";
        this.query = "";
    }

    @Override
    public StructuredUriStringBuilder appendScheme(final String scheme){
        return Optional.ofNullable(scheme)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sScheme->new StructuredUriStringBuilder(mergeSchemes(this.scheme,sScheme), host, port, path, query))
                .orElse(new StructuredUriStringBuilder(this.scheme,host, port, path, query));
    }

    @Override
    public StructuredBuildableUri appendScheme(Integer scheme) {
        return convertObjectToString(scheme)
                .map(this::appendScheme)
                .orElse(new StructuredUriStringBuilder(this.scheme,host, port, path, query));
    }

    @Override
    public StructuredUriStringBuilder appendHost(final String host){
        return Optional.ofNullable(host)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sHost->new StructuredUriStringBuilder(scheme, mergeHosts(this.host, sHost), port, path, query))
                .orElse(new StructuredUriStringBuilder(scheme, this.host, port, path, query));
    }

    @Override
    public StructuredBuildableUri appendHost(Integer host) {
        return convertObjectToString(host)
                .map(this::appendHost)
                .orElse(new StructuredUriStringBuilder(scheme, this.host, port, path, query));
    }

    @Override
    public StructuredUriStringBuilder appendPort(final String port){
        return Optional.ofNullable(port)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sPort->new StructuredUriStringBuilder(scheme, host, mergePorts(this.port, sPort), path, query))
                .orElse(new StructuredUriStringBuilder(scheme, host, this.port, path, query));
    }

    @Override
    public StructuredBuildableUri appendPort(Integer port) {
        return convertObjectToString(port)
                .map(this::appendPort)
                .orElse(new StructuredUriStringBuilder(scheme, host, this.port, path, query));
    }

    @Override
    public StructuredUriStringBuilder appendPath(final String path){
        return Optional.ofNullable(path)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(SlashTrimmers::trimSlashes)
                .map(sPath->new StructuredUriStringBuilder(scheme, host, port, mergePaths(this.path, sPath), query))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, this.path, query));
    }

    @Override
    public StructuredBuildableUri appendPath(Integer path) {
        return convertObjectToString(path)
                .map(this::appendPath)
                .orElse(new StructuredUriStringBuilder(scheme, host, port, this.path, query));
    }


    @Override
    public StructuredUriStringBuilder appendQuery(final String key, final String value){
        return convertObjectToString(key)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .flatMap(sKey->convertObjectToString(value)
                        .map(StringUtils::replaceBlankSpaceWithPlusIcon)
                        .map(sValue->new StructuredUriStringBuilder(scheme, host, port, path,
                                mergeQueries(this.query, QueryTrimmers
                                        .trimQueryComponents(sKey) + "=" + QueryTrimmers.trimQueryComponents(sValue)))))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    @Override
    public StructuredBuildableUri appendQuery(String key, Integer value) {
        return convertObjectToString(value)
                .map(stringValue->appendQuery(key, value))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    @Override
    public StructuredBuildableUri appendQuery(Integer key, String value) {
        return convertObjectToString(key)
                .map(stringValue->appendQuery(key, value))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    @Override
    public StructuredBuildableUri appendQuery(Integer key, Integer value) {
        return convertObjectToString(key)
                .map(stringValue->appendQuery(key, value))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    @Override
    public StructuredBuildableUri appendQuery(Map<String, String> map) {
        return Optional.ofNullable(map)
                .map(nonNullMap->nonNullMap.entrySet().stream()
                        .map(entry->String.format("%s%s%s",entry.getKey() == null? "": entry.getKey(),
                                entry.getKey() == null ? "" : QUERY_EQUALS.getValue(),
                                entry.getKey() == null || entry.getValue() == null? "": entry.getValue()))
                        .reduce("",(acc,entry)->String.format("%s%s%s",
                                acc, entry.isEmpty()?"":QUERY_AND.getValue(),
                                entry))
                ).map(this::appendQuery)
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    @Override
    public StructuredUriStringBuilder appendQuery(final String query) {
        return convertObjectToString(query)
                .map(StringUtils::replaceBlankSpaceWithEmptyStrings)
                .map(sQuery->new StructuredUriStringBuilder(scheme, host, port, path, mergeQueries(this.query, sQuery)))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    @Override
    public StructuredBuildableUri appendQuery(Integer query) {
        return Optional.ofNullable(query)
                .map(this::appendQuery)
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    private String mergeSchemes(String currentScheme, String newSchemePortion) {
        return String.format("%s%s",currentScheme, newSchemePortion);
    }

    private String mergeHosts(String currentHost, String newHost) {
        return String.format("%s%s",currentHost, newHost);
    }

    private String mergePorts(final String currentPort, final String newPortionPort) {
        if(currentPort.isEmpty()) return String.format(":%s", newPortionPort);
        return String.format("%s%s", currentPort, newPortionPort);
    }

    private String mergePaths(String currentPath, String appendedPath) {
        if(currentPath.isEmpty()) return String.format("/%s", appendedPath);
        return String.format("%s/%s", currentPath, appendedPath);
    }

    private String mergeQueries(final String currentQuery, final String newQuery) {
        return Optional.ofNullable(currentQuery)
                .flatMap(nonNullCurrentQuery->Optional.ofNullable(newQuery))
                .map(QueryTrimmers::trimQueryAnd)
                .map(QueryTrimmers::trimQueryEquals)
                .map(QueryTrimmers::trimQuery)
                .map(trimmedNewQuery->{
                    if(currentQuery.isEmpty()) return String.format("%s%s",
                            trimmedNewQuery.isEmpty()? "" : QUERY.getValue(),
                            trimmedNewQuery);
                    return String.format("%s%s%s",
                            currentQuery,
                            trimmedNewQuery.isEmpty()? "" : QUERY_AND.getValue(),
                            trimmedNewQuery);
                })
                .orElse("");
    }

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
