package com.marcelus.uristringbuilder.structuredstring;

import com.marcelus.uristringbuilder.utils.QueryTrimmers;
import com.marcelus.uristringbuilder.utils.SlashTrimmers;

import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.StringUtils.convertObjectToString;

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
                .map(SlashTrimmers::trimSlashes)
                .map(sPath->new StructuredUriStringBuilder(scheme, host, port, processPath(this.path, sPath), query))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, this.path, query));
    }



    public StructuredUriStringBuilder appendQuery(final Object key, final Object value){
        return convertObjectToString(key)
                .flatMap(sKey->convertObjectToString(value)
                        .map(sValue->new StructuredUriStringBuilder(scheme, host, port, path,
                                processQuery(this.query, QueryTrimmers
                                        .trimQueryComponents(sKey) + "=" + QueryTrimmers.trimQueryComponents(sValue)))))
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
        return Optional.ofNullable(currentQuery)
                .flatMap(nonNullCurrentQuery->Optional.ofNullable(newQuery))
                .map(QueryTrimmers::trimQueryAnd)
                .map(QueryTrimmers::trimQueryEquals)
                .map(QueryTrimmers::trimQuery)
                .map(trimmedNewQuery->{
                    if(currentQuery.isEmpty()) return String.format("?%s", trimmedNewQuery);
                    return String.format("%s&%s", currentQuery ,newQuery);
                })
                .orElse("");
    }

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
