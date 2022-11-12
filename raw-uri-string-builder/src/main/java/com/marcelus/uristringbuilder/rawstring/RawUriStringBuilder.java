package com.marcelus.uristringbuilder.rawstring;

import com.marcelus.uristringbuilder.available.uri.schemes.URISchemes;


import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class RawUriStringBuilder {
    private static final String PORT_REGEX=".*:\\d+.*";
    private static final String POST_SCHEME_PORTION = "://";
    private static final String DEFAULT_SCHEME = "https";
    private static final String PATH_SLASH = "/";
    private static final String QUERY = "?";
    private final String url;
    private final Boolean portDetected;
    private final Boolean pathStarted;
    private final Boolean queryStringStarted;

    public RawUriStringBuilder(final String url) {
        if((!url.contains(POST_SCHEME_PORTION) || url.startsWith(POST_SCHEME_PORTION)) && Boolean
                .FALSE.equals(URISchemes.isScheme(url.trim()))){
            this.url = DEFAULT_SCHEME + POST_SCHEME_PORTION + url.trim();
        }else{
            this.url = url.trim();
        }
        portDetected = url.matches(PORT_REGEX);
        pathStarted = portDetected || url.replace(POST_SCHEME_PORTION,"").contains("/");
        queryStringStarted = url.contains(QUERY);
    }

    public RawUriStringBuilder() {
        url = "";
        pathStarted = false;
        portDetected = false;
        queryStringStarted = false;
    }

    private RawUriStringBuilder(final String url, final Boolean pathStarted, final Boolean portDetected,
                                final Boolean queryStringStarted) {
        this.url = url.trim();
        this.pathStarted = pathStarted;
        this.portDetected = portDetected;
        this.queryStringStarted = queryStringStarted;
    }

    public RawUriStringBuilder append(final Object urlPortion) {
        return Optional.ofNullable(urlPortion)
                .map(String::valueOf)
                .map(String::trim)
                .map(trimmedUrlPortion->handleSchemeAndEmptyUrlScenario(url, trimmedUrlPortion, pathStarted,
                        portDetected, queryStringStarted))
                .orElse(new RawUriStringBuilder(""));
    }

    private RawUriStringBuilder handleSchemeAndEmptyUrlScenario
            (final String url, final String trimmedUrlPortion, final Boolean pathStarted, final Boolean portDetected,
             final Boolean queryStringStarted) {
        if(Boolean.TRUE.equals(checkIfSchemeAndUrlEmpty(trimmedUrlPortion, url))){
            return new RawUriStringBuilder(trimmedUrlPortion + POST_SCHEME_PORTION, pathStarted, portDetected,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleNotSchemeWithEmptyURLScenario(url, trimmedUrlPortion, pathStarted, portDetected,
                queryStringStarted);
    }

    private RawUriStringBuilder handleNotSchemeWithEmptyURLScenario
            (final String url, final String trimmedUrlPortion, final Boolean pathStarted, final Boolean portDetected,
             final Boolean queryStringStarted) {
        // Defaults to https scheme if scheme isn't provided.
        if(Boolean.TRUE.equals(checkIfNotSchemeAndURLIsEmpty(url, trimmedUrlPortion))){
            return new RawUriStringBuilder(produceHttpsDefaultScheme() + trimmedUrlPortion, pathStarted,
                    portDetected, determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleSchemeUrlAndPostSchemePortionScenario(url, trimmedUrlPortion, pathStarted, portDetected, queryStringStarted);
    }

    private RawUriStringBuilder handleSchemeUrlAndPostSchemePortionScenario
            (final String url, final String trimmedUrlPortion, final Boolean pathStarted, final Boolean portDetected,
             final Boolean queryStringStarted) {
        if(Boolean.TRUE.equals(checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion(url, trimmedUrlPortion))){
            return new RawUriStringBuilder( url+ POST_SCHEME_PORTION +trimmedUrlPortion, pathStarted, portDetected,
                determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion(url, trimmedUrlPortion,
                pathStarted, portDetected,queryStringStarted);
    }

    private RawUriStringBuilder handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion
            (final String url, final String trimmedUrlPortion, final Boolean pathStarted, final Boolean portDetected,
             final Boolean queryStringStarted){
        if(Boolean.TRUE.equals(url.contains(POST_SCHEME_PORTION) && trimmedUrlPortion.contains(POST_SCHEME_PORTION))){
            return new RawUriStringBuilder(url + trimmedUrlPortion.replace(POST_SCHEME_PORTION, ""),
                    pathStarted, portDetected,determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleStartedPathScenario(url, trimmedUrlPortion, pathStarted, portDetected,queryStringStarted);
    }

    private RawUriStringBuilder handleStartedPathScenario
            (final String url, final String trimmedUrlPortion, final Boolean pathStarted, final Boolean portDetected,
             final Boolean queryStringStarted) {
        // If path started, remove trailing slashes from start and end of trimmedUrlPortion ONLY if
        // query string portion hasn't started.
        if(Boolean.TRUE.equals(pathStarted)){
            return new RawUriStringBuilder(((trimSlashes(url) +
                (Boolean.TRUE.equals(queryStringStarted)?"":PATH_SLASH)+ trimSlashes(trimmedUrlPortion))
                .replace(PATH_SLASH + QUERY, QUERY)
                    .replaceAll("\\?{2,}", "?")
                    .replaceAll("={2,}", "=")
                    .replaceAll("&{2,}", "&")),
                    true, portDetected, determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handlePathNeedsToBeStartedScenario(url, trimmedUrlPortion, pathStarted, portDetected, queryStringStarted);
    }

    private RawUriStringBuilder handlePathNeedsToBeStartedScenario
            (final String url, final String trimmedUrlPortion, final Boolean pathStarted, final Boolean portDetected,
             final Boolean queryStringStarted) {
        // Verifying if path needs to be started at this point.
        if(checkIfPathNeedsToBeStarted(url, trimmedUrlPortion)){
            return new RawUriStringBuilder(url+trimmedUrlPortion, true, portDetected,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }

        return handlePortDetectionCheck(url, trimmedUrlPortion, pathStarted, portDetected, queryStringStarted);

    }

    private RawUriStringBuilder handlePortDetectionCheck
            (final String url, final String trimmedUrlPortion, final Boolean pathStarted, final Boolean portDetected,
             final Boolean queryStringStarted){
        if(url.matches(PORT_REGEX) || trimmedUrlPortion.matches(PORT_REGEX) || url.endsWith(":") && trimmedUrlPortion
                .matches("\\d+.?") || (url+trimmedUrlPortion).matches(PORT_REGEX)){
            final Pattern p = Pattern.compile(":\\d+");
            final Matcher m = p.matcher(url+trimmedUrlPortion);
            String result = "";
            if(m.find()){
                result = m.group();
            }
            return new RawUriStringBuilder((url+trimmedUrlPortion).replace(result, result + PATH_SLASH),
                    true, true, determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }

        // End of flow
        return new RawUriStringBuilder((url + trimmedUrlPortion), pathStarted, portDetected,
                queryStringStarted ||trimmedUrlPortion.contains(QUERY) || url.contains(QUERY));
    }


    private boolean checkIfPathNeedsToBeStarted(final String url, final String trimmedUrlPortion) {
        return (url+trimmedUrlPortion).replace(POST_SCHEME_PORTION,"").contains(PATH_SLASH);
    }

    private Boolean checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion
            (final String url, final String trimmedUrlPortion) {
        return URISchemes.isScheme(url) && !trimmedUrlPortion.contains(POST_SCHEME_PORTION);
    }

    private String produceHttpsDefaultScheme() {
        return DEFAULT_SCHEME + POST_SCHEME_PORTION;
    }

    private Boolean checkIfNotSchemeAndURLIsEmpty(final String url, final String trimmedUrlPortion) {
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
