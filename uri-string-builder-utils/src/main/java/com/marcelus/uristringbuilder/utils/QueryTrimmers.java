package com.marcelus.uristringbuilder.utils;

import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_AND;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_EQUALS;

public class QueryTrimmers {

    private QueryTrimmers(){

    }

    public static String trimQueryAnd(String path) {
        if(path.equals(QUERY_AND.getValue())) return path;
        return StringUtils.trim(path, QUERY_AND.getValue()).orElse("");
    }

    public static String trimQueryAndAtEnd(String path){
        if(path.equals(QUERY_AND.getValue())) return path;
        return StringUtils.trimCharAtEnd(path, QUERY_AND.getValue()).orElse("");
    }

    public static String trimQueryAndAtStart(String path) {
        if(path.equals(QUERY_AND.getValue())) return path;
        return StringUtils.trimCharAtStart(path, QUERY_AND.getValue()).orElse("");
    }

    public static String trimQueryEquals(String path) {
        if(path.equals(QUERY_EQUALS.getValue())) return path;
        return StringUtils.trim(path, QUERY_EQUALS.getValue()).orElse("");
    }

    public static String trimQueryEqualsAtEnd(String path){
        if(path.equals(QUERY_EQUALS.getValue())) return path;
        return StringUtils.trimCharAtEnd(path, QUERY_EQUALS.getValue()).orElse("");
    }

    public static String trimQueryEqualsAtStart(String path) {
        if(path.equals(QUERY_EQUALS.getValue())) return path;
        return StringUtils.trimCharAtStart(path, QUERY_EQUALS.getValue()).orElse("");
    }

    public static String trimQuery(String path) {
        if(path.equals(QUERY.getValue())) return path;
        return StringUtils.trim(path, QUERY.getValue()).orElse("");
    }

    public static String trimQueryAtEnd(String path){
        if(path.equals(QUERY.getValue())) return path;
        return StringUtils.trimCharAtEnd(path, QUERY.getValue()).orElse("");
    }

    public static String trimQueryAtStart(String path) {
        if(path.equals(QUERY.getValue())) return path;
        return StringUtils.trimCharAtStart(path, QUERY.getValue()).orElse("");
    }

    public static String trimQueryComponents(String path) {
        return Optional.ofNullable(trimQuery(path))
                .map(QueryTrimmers::trimQuery)
                .map(QueryTrimmers::trimQueryAnd)
                .map(QueryTrimmers::trimQueryEquals)
                .orElse("");

    }

    public static String trimQueryComponentsAndMerge(final String urlPortion, String trimmedUrlPortion) {
        if(urlPortion.endsWith(QUERY_AND.getValue())){
            return Optional.of(trimQueryAndAtEnd(urlPortion))
                    .flatMap(removedUrlPortionQueryAnd->Optional.of(trimQueryAndAtStart(trimmedUrlPortion))
                            .map(removedTrimmedUrlPortionQueryAnd->
                                    removedUrlPortionQueryAnd + QUERY_AND.getValue()
                                            + removedTrimmedUrlPortionQueryAnd)
                    ).orElse("");
        } else if(urlPortion.endsWith(UriPortionConstants.QUERY_EQUALS.getValue())){
            return Optional.of(trimQueryEqualsAtEnd(urlPortion))
                    .flatMap(removedUrlPortionQueryEquals-> Optional.of(trimQueryEqualsAtStart(trimmedUrlPortion))
                            .map(removedTrimmedUrlPortionQueryEquals->
                                    removedUrlPortionQueryEquals + UriPortionConstants.QUERY_EQUALS.getValue()
                                            + removedTrimmedUrlPortionQueryEquals)
                    ).orElse("");
        }else if(urlPortion.endsWith(QUERY.getValue())){
            return Optional.of(trimQueryAtEnd(urlPortion))
                    .flatMap(removedUrlPortionQueryEquals->Optional.of(trimQueryAtStart(trimmedUrlPortion))
                            .map(removedTrimmedUrlPortionQueryEquals->
                                    removedUrlPortionQueryEquals + QUERY.getValue()
                                            + removedTrimmedUrlPortionQueryEquals)
                    ).orElse("");
        }else{
            return urlPortion + trimmedUrlPortion;
        }
    }

}
