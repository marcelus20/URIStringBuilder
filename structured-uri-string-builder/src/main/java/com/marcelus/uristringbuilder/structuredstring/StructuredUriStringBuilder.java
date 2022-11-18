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

    public final StructuredUriStringBuilder appendScheme(final Object scheme){
        return convertObjectToString(scheme)
                .map(sScheme->new StructuredUriStringBuilder(sScheme, host, port, path, query))
                .orElse(new StructuredUriStringBuilder(this.scheme,host, port, path, query));
    }

    public final StructuredUriStringBuilder appendHost(final Object host){
        return convertObjectToString(host)
                .map(sHost->new StructuredUriStringBuilder(scheme, sHost, port, path, query))
                .orElse(new StructuredUriStringBuilder(scheme, this.host, port, path, query));
    }

    public final StructuredUriStringBuilder appendPort(final Object port){
        return convertObjectToString(port)
                .map(sPort->new StructuredUriStringBuilder(scheme, host, sPort, path, query))
                .orElse(new StructuredUriStringBuilder(scheme, host, this.port, path, query));
    }

    public final StructuredUriStringBuilder appendPath(final Object path){
        return convertObjectToString(path)
                .map(sPath->new StructuredUriStringBuilder(scheme, host, port, sPath, query))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, this.path, query));
    }

    public final StructuredUriStringBuilder appendQuery(final Object key, final Object value){
        return convertObjectToString(key)
                .flatMap(sKey->convertObjectToString(value)
                        .map(sValue->new StructuredUriStringBuilder(scheme, host, port, path, sKey + "=" +sValue)))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    public StructuredUriStringBuilder appendQuery(final Object query) {
        return convertObjectToString(query)
                .map(sQuery->new StructuredUriStringBuilder(scheme, host, port, path, sQuery))
                .orElse(new StructuredUriStringBuilder(scheme, host, port, path, this.query));
    }

    private final Optional<String> convertObjectToString(Object object){
       return Optional.ofNullable(object)
               .map(String::valueOf)
               .map(String::trim);
    }

    public final String build(){
        return null;
    }

    @Override
    public String toString(){
        return build();
    }
}
