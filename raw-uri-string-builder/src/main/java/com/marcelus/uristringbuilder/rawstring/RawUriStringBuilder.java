package com.marcelus.uristringbuilder.rawstring;

import com.marcelus.uristringbuilder.available.uri.schemes.URISchemes;
import com.marcelus.uristringbuilder.uribuilders.RawBuildableUri;
import com.marcelus.uristringbuilder.utils.StringUtils;

import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.QueryTrimmers.trimQueryComponentsAndMerge;
import static com.marcelus.uristringbuilder.utils.SlashTrimmers.trimSlashes;
import static com.marcelus.uristringbuilder.utils.StringUtils.convertObjectToString;
import static com.marcelus.uristringbuilder.utils.StringUtils.retrievePartAfterPatternMatcher;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.DEFAULT_SCHEME;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PATH_SLASH;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PORT_REGEX;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PORT_START_DELIMITER;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.POST_SCHEME_PORTION;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_EQUALS;

public final class RawUriStringBuilder implements RawBuildableUri {


    /*
    Fields
     */
    private final String url;
    private final Boolean portDetected;
    private final Boolean pathStarted;
    private final Boolean queryStringStarted;

    /*
    End of fields
     */



    /*
    Constructors
     */
    public RawUriStringBuilder(final String url) {
        if(checkPostSchemeCondition(url)){
            this.url = DEFAULT_SCHEME.getValue() + POST_SCHEME_PORTION.getValue() + url.trim();
        }else{
            this.url = url.trim();
        }
        portDetected = url.matches(PORT_REGEX.getValue());
        pathStarted = portDetected || url.replace(POST_SCHEME_PORTION.getValue(),"")
                .contains(PATH_SLASH.getValue());
        queryStringStarted = url.contains(QUERY.getValue());
    }

    public RawUriStringBuilder() {
        url = "";
        pathStarted = false;
        portDetected = false;
        queryStringStarted = false;
    }

    private RawUriStringBuilder(
            final String url,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {
        this.url = url.trim();
        this.pathStarted = pathStarted;
        this.portDetected = portDetected;
        this.queryStringStarted = queryStringStarted;
    }


    /*
    End of constructors
     */


    @Override
    public RawBuildableUri append(final String urlPortion) {
        return Optional.ofNullable(urlPortion)
                .map(this::handleSpaces)
                .map(trimmedUrlPortion->handleSchemeAndEmptyUrlScenario(url, trimmedUrlPortion, pathStarted,
                        portDetected, queryStringStarted))
                .orElse(new RawUriStringBuilder(""));
    }

    private String handleNonQuerySpaces(String trimmedUrlPortion) {
        return StringUtils.replaceBlankSpaceWithEmptyStrings(trimmedUrlPortion);
    }

    @Override
    public RawBuildableUri append(final Integer urlPortion) {
        return convertObjectToString(urlPortion)
                .map(this::append)
                .orElse(new RawUriStringBuilder(""));
    }

    @Override
    public String build() {
        return url;
    }




    /*
        Handlers
     */

    private String handleSpaces(final String trimmedUrlPortion) {
        if(Boolean.FALSE.equals(queryStringStarted)){
            return handleNonQuerySpaces(trimmedUrlPortion);
        }else{
            return handleQuerySpaces(trimmedUrlPortion);
        }
    }

    private String handleQuerySpaces(String trimmedUrlPortion) {
        if(url.endsWith(QUERY.getValue())){
            return StringUtils.replaceBlankSpaceWithEmptyStrings(trimmedUrlPortion);
        }else if(url.endsWith(QUERY_EQUALS.getValue())){
            return StringUtils.replaceBlankSpaceWithPlusIcon(trimmedUrlPortion);
        }else {
            return StringUtils.replaceBlankSpaceWithEmptyStrings(trimmedUrlPortion);
        }
    }
    private RawUriStringBuilder handleSchemeAndEmptyUrlScenario(
            final String url,
            final String trimmedUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        if(Boolean.TRUE.equals(checkIfSchemeAndUrlEmpty(trimmedUrlPortion, url))){
            return new RawUriStringBuilder(trimmedUrlPortion + POST_SCHEME_PORTION.getValue(), pathStarted, portDetected,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleNotSchemeWithEmptyURLScenario(url, trimmedUrlPortion, pathStarted, portDetected,
                queryStringStarted);
    }

    private RawUriStringBuilder handleNotSchemeWithEmptyURLScenario(
            final String url,
            final String trimmedUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        // Defaults to https scheme if scheme isn't provided.
        if(Boolean.TRUE.equals(checkIfNotSchemeAndURLIsEmpty(url, trimmedUrlPortion))){
            return new RawUriStringBuilder(produceHttpsDefaultScheme() + trimmedUrlPortion, pathStarted,
                    portDetected, determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleSchemeUrlAndPostSchemePortionScenario(url, trimmedUrlPortion, pathStarted, portDetected, queryStringStarted);
    }

    private RawUriStringBuilder handleSchemeUrlAndPostSchemePortionScenario(
            final String url,
            final String trimmedUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        if(Boolean.TRUE.equals(checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion(url, trimmedUrlPortion))){
            return new RawUriStringBuilder( url+ POST_SCHEME_PORTION.getValue() +trimmedUrlPortion, pathStarted, portDetected,
                determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }
        return handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion(url, trimmedUrlPortion,
                pathStarted, portDetected,queryStringStarted);
    }

    private RawUriStringBuilder handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion(
            final String url,
            final String trimmedUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted){


        if(Boolean.TRUE.equals(url.contains(POST_SCHEME_PORTION.getValue()) && trimmedUrlPortion
                .contains(POST_SCHEME_PORTION.getValue()))){
            return new RawUriStringBuilder(
                    url + trimmedUrlPortion.replace(POST_SCHEME_PORTION.getValue(), ""),
                    pathStarted,
                    portDetected,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion)
            );
        }
        return handleStartedPathScenario(url, trimmedUrlPortion, pathStarted, portDetected,queryStringStarted);
    }

    private RawUriStringBuilder handleStartedPathScenario(
            final String url,
            final String trimmedUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        // If path started, remove trailing slashes from start and end of trimmedUrlPortion ONLY if
        // query string portion hasn't started.
        if(Boolean.TRUE.equals(pathStarted) || Boolean.TRUE.equals(queryStringStarted)){
            return new RawUriStringBuilder(
                    mergeUrlAndTrimmedPortion(url, trimmedUrlPortion, queryStringStarted),
                    true,
                    portDetected,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion)
            );
        }
        return handlePathNeedsToBeStartedScenario(url, trimmedUrlPortion, pathStarted, portDetected, queryStringStarted);
    }

    private RawUriStringBuilder handlePathNeedsToBeStartedScenario(
            final String url,
            final String trimmedUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        // Verifying if path needs to be started at this point.
        if(checkIfPathNeedsToBeStarted(url, trimmedUrlPortion)){
            return new RawUriStringBuilder(url+trimmedUrlPortion, true, portDetected,
                    determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion));
        }

        return handlePortDetectionCheck(url, trimmedUrlPortion, pathStarted, portDetected, queryStringStarted);

    }

    private RawUriStringBuilder handlePortDetectionCheck(
            final String url,
            final String trimmedUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted){


        if(checkPortDetection(url, trimmedUrlPortion)){

            final String result = retrievePartAfterPatternMatcher(url + trimmedUrlPortion, ":\\d+");
            final Boolean queryStringHasStarted = determineIfQueryHasStarted(queryStringStarted, trimmedUrlPortion);
            final String suffix = Boolean.TRUE.equals(queryStringHasStarted)? "" : PATH_SLASH.getValue();

            return new RawUriStringBuilder((url+trimmedUrlPortion).replace(result, result + suffix),
                    true, true, queryStringHasStarted);
        }

        // End of flow
        return new RawUriStringBuilder((url + trimmedUrlPortion), pathStarted, portDetected,
                queryStringStarted ||trimmedUrlPortion.contains(QUERY.getValue()) || url
                        .contains(QUERY.getValue()));
    }





    /*
    End of handlers
     */




    /*
    MISC
     */
    private String mergeUrlAndTrimmedPortion(
            final String url, final String trimmedUrlPortion, Boolean queryStringStarted
    ) {
        if(Boolean.TRUE.equals(queryStringStarted)){
            // Trim the query strings components: ?, = and &
            return trimQueryComponentsAndMerge(url, trimmedUrlPortion);
        }else{
            // trim the slashes (path has started)
            return ((trimSlashes(url) +
                    (Boolean.TRUE.equals(this.queryStringStarted)?"":PATH_SLASH.getValue())+ trimSlashes(trimmedUrlPortion))
                    .replace(PATH_SLASH.getValue() + QUERY.getValue(), QUERY.getValue()));
        }
    }

    private String produceHttpsDefaultScheme() {
        return DEFAULT_SCHEME.getValue() + POST_SCHEME_PORTION.getValue();
    }

    /*
    END OF MISC
     */




    /*
        CONDITION HELPERS
     */
    private boolean checkPostSchemeCondition(String url) {
        return (!url.contains(POST_SCHEME_PORTION.getValue()) || url.startsWith(POST_SCHEME_PORTION.getValue())) && Boolean
                .FALSE.equals(URISchemes.isScheme(url.trim()));
    }

    private boolean checkPortDetection(final String url, final String trimmedUrlPortion) {
        return url.matches(PORT_REGEX.getValue()) || trimmedUrlPortion.matches(PORT_REGEX.getValue()) || url
                .endsWith(PORT_START_DELIMITER.getValue())
                && trimmedUrlPortion.matches("\\d+.?") || (url+trimmedUrlPortion).matches(PORT_REGEX.getValue());
    }
    private Boolean determineIfQueryHasStarted(final Boolean queryStringStarted, final String trimmedUrlPortion) {
        return queryStringStarted|| trimmedUrlPortion.contains(QUERY.getValue());
    }

    private boolean checkIfPathNeedsToBeStarted(final String url, final String trimmedUrlPortion) {
        return (url+trimmedUrlPortion).replace(POST_SCHEME_PORTION.getValue(),"")
                .contains(PATH_SLASH.getValue());
    }
    private Boolean checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion(
            final String url, final String trimmedUrlPortion
    ) {
        return URISchemes.isScheme(url) && !trimmedUrlPortion.contains(POST_SCHEME_PORTION.getValue());
    }

    private Boolean checkIfNotSchemeAndURLIsEmpty(final String url, final String trimmedUrlPortion) {
        return !URISchemes.isScheme(trimmedUrlPortion) && url.isEmpty();
    }

    private Boolean checkIfSchemeAndUrlEmpty(final String trimmedUrlPortion, final String url) {
        return URISchemes.isScheme(trimmedUrlPortion) && url.isEmpty();
    }

    /*
       END OF CONDITION HELPERS
     */


    @Override
    public String toString() {
        return build();
    }
}
