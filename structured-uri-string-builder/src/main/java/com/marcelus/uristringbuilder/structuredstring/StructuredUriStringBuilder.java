package com.marcelus.uristringbuilder.structuredstring;

import java.util.Optional;

public final class StructuredUriStringBuilder {

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

    public StructuredUriStringBuilder appendScheme(final Object scheme){
        return convertObjectToString(scheme)
                .map(sScheme->new StructuredUriStringBuilder(sScheme, host, port, path, query))
                .orElse(new StructuredUriStringBuilder(this.scheme,host, port, path, query));
    }

    public StructuredUriStringBuilder appendHost(final Object host){
        return convertObjectToString(host)
                .map(sHost->new StructuredUriStringBuilder(scheme, sHost, port, path, query))
                .orElse(new StructuredUriStringBuilder(scheme, this.host, port, path, query));
    }

    public StructuredUriStringBuilder appendPort(final Object port){
        return convertObjectToString(port)
                .map(sPort->new StructuredUriStringBuilder(scheme, host, processPort(this.port, sPort), path, query))
                .orElse(new StructuredUriStringBuilder(scheme, host, this.port, path, query));
    }

    public StructuredUriStringBuilder appendPath(final Object path){
        return convertObjectToString(path)
                .map(this::trimSlashes)
                .map(sPath->new StructuredUriStringBuilder(scheme, host, port, processPath(this.path, sPath), query))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, this.path, query));
    }

    private String trimSlashes(String path) {
        return Optional.ofNullable(path)
                .map(String::trim)
                .flatMap(this::trimSlashAtStart)
                .flatMap(this::trimSlashAtEnd)
                .orElse("");
    }

    private Optional<String> trimSlashAtEnd(String path){
        return Optional.ofNullable(path)
                .map(String::trim)
                .map(p->{
                    if(p.startsWith("/")){
                        return p.replaceAll("/*$", "");
                    }
                    return p;
                });
    }

    private Optional<String> trimSlashAtStart(String path) {
        return Optional.ofNullable(path)
                .map(String::trim)
                .map(nonNullPath->{
                    if(nonNullPath.startsWith("/")){
                        return nonNullPath.replaceAll("^/*", "");
                    }
                    return nonNullPath;
                });
    }

    public StructuredUriStringBuilder appendQuery(final Object key, final Object value){
        return convertObjectToString(key)
                .flatMap(sKey->convertObjectToString(value)
                        .map(sValue->new StructuredUriStringBuilder(scheme, host, port, path,
                                processQuery(this.query, sKey + "=" +sValue))))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    public StructuredUriStringBuilder appendQuery(final Object query) {
        return convertObjectToString(query)
                .map(sQuery->new StructuredUriStringBuilder(scheme, host, port, path, processQuery(this.query, sQuery)))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    private String processPort(final String currentPort, final String newPortionPort) {
        if(port.isEmpty()) return String.format(":%s", newPortionPort);
        return String.format("%s%s", currentPort, newPortionPort);
    }

    private String processPath(String currentPath, String appendedPath) {
        if(currentPath.isEmpty()) return String.format("/%s", appendedPath);
        return String.format("%s/%s", currentPath, appendedPath);
    }

    private String processQuery(final String currentQuery, final String newQuery) {
        if(currentQuery.isEmpty()) return String.format("?%s", newQuery);
        return String.format("%s&%s", currentQuery ,newQuery);
    }

    private Optional<String> convertObjectToString(Object object){
       return Optional.ofNullable(object)
               .map(String::valueOf)
               .map(String::trim);
    }

    public String build(){
        if(scheme.isEmpty()) return String.format("%s%s%s%s", host, port, path, query);
        return String.format("%s://%s%s%s%s", scheme, host, port, host.isEmpty() ? trimSlashAtStart(path)
                .orElse("") : path, query);
    }

    @Override
    public String toString(){
        return build();
    }
}
