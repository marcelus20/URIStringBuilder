package com.marcelus.uristringbuilder.structuredstring;

import java.util.Optional;

public class StructuredUriStringBuilder {

    public final StructuredUriStringBuilder appendScheme(final Object scheme){
        return new StructuredUriStringBuilder();
    }

    public final StructuredUriStringBuilder appendHost(final Object host){
        return new StructuredUriStringBuilder();
    }

    public final StructuredUriStringBuilder appendPort(final Object port){
        return new StructuredUriStringBuilder();
    }

    public final StructuredUriStringBuilder appendPath(final Object path){
        return new StructuredUriStringBuilder();
    }

    public final StructuredUriStringBuilder appendQuery(final Object key, final Object value){
        return new StructuredUriStringBuilder();
    }

    private final Optional<String> convertObjectToString(Object object){
       return Optional.ofNullable(object)
               .map(String::valueOf);
    }

    public final String build(){
        return null;
    }

    @Override
    public String toString(){
        return build();
    }
}
