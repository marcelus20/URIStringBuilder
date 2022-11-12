package com.marcelus.uristringbuilder.rawstring;

import com.marcelus.uristringbuilder.available.uri.schemes.URISchemes;


import java.util.Optional;

public final class RawUriStringBuilder {
    private static final String POST_SCHEME_PORTION = "://";
    private static final String DEFAULT_SCHEME = "https";
    private static final String PATH_SLASH = "/";
    private static final String QUERY = "?";
    private final String url;
    private final Boolean pathStarted;
    private final Boolean queryStringStarted;

    public RawUriStringBuilder(final String url) {
        if((!url.contains(POST_SCHEME_PORTION) || url.startsWith(POST_SCHEME_PORTION)) && Boolean
                .FALSE.equals(URISchemes.isScheme(url.trim()))){
            this.url = DEFAULT_SCHEME + POST_SCHEME_PORTION + url.trim();
        }else{
            this.url = url.trim();
        }
        pathStarted = false;
        queryStringStarted = url.contains(QUERY);
    }

    public RawUriStringBuilder() {
        url = "";
        pathStarted = false;
        queryStringStarted = false;
    }

    private RawUriStringBuilder(final String url, final Boolean pathStarted, final Boolean queryStringStarted) {
        this.url = url.trim();
        this.pathStarted = pathStarted;
        this.queryStringStarted = queryStringStarted;
    }

    public RawUriStringBuilder append(final Object urlPortion) {
        return Optional.ofNullable(urlPortion)
                .map(String::valueOf)
                .map(String::trim)
                .map(trimmedUrlPortion->handleSchemeAndEmptyUrlScenario(url, trimmedUrlPortion, pathStarted,
                        queryStringStarted))
                .orElse(new RawUriStringBuilder(""));
    }

    private RawUriStringBuilder handleSchemeAndEmptyUrlScenario
            (String url, String trimmedUrlPortion, Boolean pathStarted, Boolean queryStringStarted) {
        if(Boolean.TRUE.equals(checkIfSchemeAndUrlEmpty(trimmedUrlPortion, url))){
            return new RawUriStringBuilder(trimmedUrlPortion + POST_SCHEME_PORTION, pathStarted,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleNotSchemeWithEmptyURLScenario(url, trimmedUrlPortion, pathStarted, queryStringStarted);
    }

    private RawUriStringBuilder handleNotSchemeWithEmptyURLScenario
            (String url, String trimmedUrlPortion, Boolean pathStarted, Boolean queryStringStarted) {
        // Defaults to https scheme if scheme isn't provided.
        if(Boolean.TRUE.equals(checkIfNotSchemeAndURLIsEmpty(url, trimmedUrlPortion))){
            return new RawUriStringBuilder(produceHttpsDefaultScheme() + trimmedUrlPortion, pathStarted,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleSchemeUrlAndPostSchemePortionScenario(url, trimmedUrlPortion, pathStarted, queryStringStarted);
    }

    private RawUriStringBuilder handleSchemeUrlAndPostSchemePortionScenario
            (String url, String trimmedUrlPortion, Boolean pathStarted, Boolean queryStringStarted) {
        if(Boolean.TRUE.equals(checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion(url, trimmedUrlPortion))){
            return new RawUriStringBuilder( url+ POST_SCHEME_PORTION +trimmedUrlPortion, pathStarted,
                determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion(url, trimmedUrlPortion,
                pathStarted, queryStringStarted);
    }

    private RawUriStringBuilder handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion
            (String url, String trimmedUrlPortion, Boolean pathStarted, Boolean queryStringStarted){
        if(Boolean.TRUE.equals(url.contains(POST_SCHEME_PORTION) && trimmedUrlPortion.contains(POST_SCHEME_PORTION))){
            return new RawUriStringBuilder(url + trimmedUrlPortion.replace(POST_SCHEME_PORTION, ""),
                    pathStarted, determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleStartedPathScenario(url, trimmedUrlPortion, pathStarted, queryStringStarted);
    }

    private RawUriStringBuilder handleStartedPathScenario
            (String url, String trimmedUrlPortion, Boolean pathStarted, Boolean queryStringStarted) {
        // If path started, remove trailing slashes from start and end of trimmedUrlPortion ONLY if
        // query string portion hasn't started.
        if(Boolean.TRUE.equals(pathStarted)){
            return new RawUriStringBuilder((trimSlashes(url) +
                (Boolean.TRUE.equals(queryStringStarted)?"":PATH_SLASH)+ trimSlashes(trimmedUrlPortion))
                .replace(PATH_SLASH + QUERY, QUERY),
                    pathStarted, determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handlePathNeedsToBeStartedScenario(url, trimmedUrlPortion, pathStarted, queryStringStarted);
    }

    private RawUriStringBuilder handlePathNeedsToBeStartedScenario
            (String url, String trimmedUrlPortion, Boolean pathStarted, Boolean queryStringStarted) {
        // Verifying if path needs to be started at this point.
        if(checkIfPathNeedsToBeStarted(url, trimmedUrlPortion)){
            return new RawUriStringBuilder(url+trimmedUrlPortion, true,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }

        // End of flow
        return new RawUriStringBuilder((url + trimmedUrlPortion), pathStarted, queryStringStarted ||
                trimmedUrlPortion.contains(QUERY) || url.contains(QUERY));
    }

    private boolean checkIfPathNeedsToBeStarted(String url, String trimmedUrlPortion) {
        return (url+trimmedUrlPortion).replace(POST_SCHEME_PORTION,"").contains(PATH_SLASH);
    }

    private Boolean checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion(String url, String trimmedUrlPortion) {
        return URISchemes.isScheme(url) && !trimmedUrlPortion.contains(POST_SCHEME_PORTION);
    }

    private String produceHttpsDefaultScheme() {
        return DEFAULT_SCHEME + POST_SCHEME_PORTION;
    }

    private Boolean checkIfNotSchemeAndURLIsEmpty(String url, String trimmedUrlPortion) {
        return !URISchemes.isScheme(trimmedUrlPortion) && url.isEmpty();
    }

    private Boolean determineIfQueryHasStarted(final Boolean queryStringStarted, final String trimmedUrlPortion) {
        return queryStringStarted|| trimmedUrlPortion.contains(QUERY);
    }

    private Boolean checkIfSchemeAndUrlEmpty(final String trimmedUrlPortion, final String url) {
        return URISchemes.isScheme(trimmedUrlPortion) && url.isEmpty();
    }

    private String trimSlashes(final String url) {
        return removeSlashFromEnd(url)
                .flatMap(this::removeSlashFromStart)
                .orElse("");
    }

    private Optional<String> removeSlashFromStart(String url) {
        return Optional.ofNullable(url)
                .map(String::trim)
                .map(trimmedUrl->{
                    if (trimmedUrl.startsWith(PATH_SLASH)) {
                        return trimmedUrl.substring(1);
                    }
                    return trimmedUrl;
                });
    }

    private Optional<String> removeSlashFromEnd(final String url){
        return Optional.ofNullable(url)
                .map(String::trim)
                .map(trimmedUrl->{
                    if (trimmedUrl.endsWith(PATH_SLASH)) {
                        return trimmedUrl.substring(0, trimmedUrl.length() - 1);
                    }
                    return trimmedUrl;
                });
    }

    public String build() {
        return url;
    }
}
