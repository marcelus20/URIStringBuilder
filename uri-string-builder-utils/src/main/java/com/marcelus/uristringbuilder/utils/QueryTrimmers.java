package com.marcelus.uristringbuilder.utils;

import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_AND;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_EQUALS;

/**
 * Utility class for trimming query components.
 * The query components are:
 * 1 - The question tag (?)
 * 2 - The equal (=)
 * 3 - And the And (&)
 * The trim will consist of removing these components from either the start of the string, or end, or both.
 */
public class QueryTrimmers {

    /**
     * Private constructor to avoid instantiating the class.
     */
    private QueryTrimmers(){

    }

    /**
     * Removes the "&" character from start and end of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "&" at the start and end.
     */
    public static String trimQueryAnd(final String queryPortion) {
        if(queryPortion.equals(QUERY_AND.getValue())) return queryPortion;
        return StringUtils.trim(queryPortion, QUERY_AND.getValue()).orElse("");
    }

    /**
     * Removes the "&" character from end of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "&" at the end.
     */
    public static String trimQueryAndAtEnd(final String queryPortion){
        if(queryPortion.equals(QUERY_AND.getValue())) return queryPortion;
        return StringUtils.trimCharAtEnd(queryPortion, QUERY_AND.getValue()).orElse("");
    }

    /**
     * Removes the "&" character from start of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "&" at the start.
     */
    public static String trimQueryAndAtStart(final String queryPortion) {
        if(queryPortion.equals(QUERY_AND.getValue())) return queryPortion;
        return StringUtils.trimCharAtStart(queryPortion, QUERY_AND.getValue()).orElse("");
    }

    /**
     * Removes the "=" character from start and end of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "=" at the start and end.
     */
    public static String trimQueryEquals(final String queryPortion) {
        if(queryPortion.equals(QUERY_EQUALS.getValue())) return queryPortion;
        return StringUtils.trim(queryPortion, QUERY_EQUALS.getValue()).orElse("");
    }

    /**
     * Removes the "=" character from end of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "=" at the end.
     */
    public static String trimQueryEqualsAtEnd(final String queryPortion){
        if(queryPortion.equals(QUERY_EQUALS.getValue())) return queryPortion;
        return StringUtils.trimCharAtEnd(queryPortion, QUERY_EQUALS.getValue()).orElse("");
    }

    /**
     * Removes the "=" character from start of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "=" at the start.
     */
    public static String trimQueryEqualsAtStart(final String queryPortion) {
        if(queryPortion.equals(QUERY_EQUALS.getValue())) return queryPortion;
        return StringUtils.trimCharAtStart(queryPortion, QUERY_EQUALS.getValue()).orElse("");
    }

    /**
     * Removes the "?" character from start and end of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "?" at the start.
     */
    public static String trimQuery(final String queryPortion) {
        if(queryPortion.equals(QUERY.getValue())) return queryPortion;
        return StringUtils.trim(queryPortion, QUERY.getValue()).orElse("");
    }

    /**
     * Removes the "?" character from end of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "?" at the end.
     */
    public static String trimQueryAtEnd(final String queryPortion){
        if(queryPortion.equals(QUERY.getValue())) return queryPortion;
        return StringUtils.trimCharAtEnd(queryPortion, QUERY.getValue()).orElse("");
    }

    /**
     * Removes the "?" character from the end of the string.
     * @param queryPortion the string to be trimmed.
     * @return a new string without the "?" at the start.
     */
    public static String trimQueryAtStart(final String queryPortion) {
        if(queryPortion.equals(QUERY.getValue())) return queryPortion;
        return StringUtils.trimCharAtStart(queryPortion, QUERY.getValue()).orElse("");
    }

    /**
     * Removes all the query components (?, = and &) from the string.
     * @param newQueryPortion the string to be trimmed.
     * @return a new string without any components at both side.
     */
    public static String trimQueryComponents(String newQueryPortion) {
        return Optional.ofNullable(trimQuery(newQueryPortion))
                .map(QueryTrimmers::trimQuery)
                .map(QueryTrimmers::trimQueryAnd)
                .map(QueryTrimmers::trimQueryEquals)
                .orElse("");

    }

    /**
     * Trims all the query components from strings (?, & and =) and join both of them together.
     * @param currentQuery the current query.
     * @param newQueryPortion the new query to be appended.
     * @return a new string containing the resulting query of two joined queries.
     */
    public static String trimQueryComponentsAndMerge(final String currentQuery, String newQueryPortion) {
        if(currentQuery.endsWith(QUERY_AND.getValue())){
            return Optional.of(trimQueryAndAtEnd(currentQuery))
                    .flatMap(removedUrlPortionQueryAnd->Optional.of(trimQueryAndAtStart(newQueryPortion))
                            .map(removedTrimmedUrlPortionQueryAnd->
                                    removedUrlPortionQueryAnd + QUERY_AND.getValue()
                                            + removedTrimmedUrlPortionQueryAnd)
                    ).orElse("");
        } else if(currentQuery.endsWith(UriPortionConstants.QUERY_EQUALS.getValue())){
            return Optional.of(trimQueryEqualsAtEnd(currentQuery))
                    .flatMap(removedUrlPortionQueryEquals-> Optional.of(trimQueryEqualsAtStart(newQueryPortion))
                            .map(removedTrimmedUrlPortionQueryEquals->
                                    removedUrlPortionQueryEquals + UriPortionConstants.QUERY_EQUALS.getValue()
                                            + removedTrimmedUrlPortionQueryEquals)
                    ).orElse("");
        }else if(currentQuery.endsWith(QUERY.getValue())){
            return Optional.of(trimQueryAtEnd(currentQuery))
                    .flatMap(removedUrlPortionQueryEquals->Optional.of(trimQueryAtStart(newQueryPortion))
                            .map(removedTrimmedUrlPortionQueryEquals->
                                    removedUrlPortionQueryEquals + QUERY.getValue()
                                            + removedTrimmedUrlPortionQueryEquals)
                    ).orElse("");
        }else{
            return currentQuery + newQueryPortion;
        }
    }

}
