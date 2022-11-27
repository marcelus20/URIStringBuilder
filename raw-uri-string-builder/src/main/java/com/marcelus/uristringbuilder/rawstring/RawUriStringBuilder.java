package com.marcelus.uristringbuilder.rawstring;

import com.marcelus.uristringbuilder.available.uri.schemes.URISchemes;
import com.marcelus.uristringbuilder.uribuilders.RawBuildableUri;
import com.marcelus.uristringbuilder.utils.StringUtils;

import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.QueryTrimmers.trimQueryComponentsAndMerge;
import static com.marcelus.uristringbuilder.utils.SlashTrimmers.trimSlashes;
import static com.marcelus.uristringbuilder.utils.StringUtils.retrievePartAfterPatternMatcher;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.DEFAULT_SCHEME;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PATH_SLASH;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PORT_REGEX;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PORT_START_DELIMITER;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.POST_SCHEME_PORTION;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY;
import static com.marcelus.uristringbuilder.utils.UriPortionConstants.QUERY_EQUALS;

/**
 * Class that contains the append method for assisting building the URI strings.
 */
public final class RawUriStringBuilder implements RawBuildableUri<String> {


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

    /**
     * One arg constructor - It takes only a string url, does some analysis on this string and derives
     * the values of the other attributes based on the url string contents.
     * If scheme is not detected, then DEFAULT_SCHEME (https) will take place at the start of the url.
     * Also, if url contains slashes, ports, query components, then the other attributes will be initialised as true.
     * @param url The url to be assigned to the url field.
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

    /**
     * No args constructor.
     */
    public RawUriStringBuilder() {
        url = "";
        pathStarted = false;
        portDetected = false;
        queryStringStarted = false;
    }

    /**
     * All args constructor. Just a regular constructor with no logic on it, but only field assignment.
     * @param url The url to be assigned to the url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     */
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


    /**
     * Takes a portion of a URL, process it and append it to the current url field by returning a new RawUriStringBuilder
     * object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return the resulting object with a new state
     */
    @Override
    public RawBuildableUri<String> append(final String newUrlPortion) {
        return Optional.ofNullable(newUrlPortion)
                .map(this::handleSpaces)
                .map(trimmedUrlPortion->handleSchemeAndEmptyUrlScenario(url, trimmedUrlPortion, pathStarted,
                        portDetected, queryStringStarted))
                .orElse(new RawUriStringBuilder(""));
    }


    /**
     * A sort of getter for the URL field. Usually to be invoked when the URL is ready and has no pending appending.
     * @return The url field.
     */
    @Override
    public String build() {
        return url;
    }





    /*
        Handlers
     */
    /**
     * Helper method for handling a scenario where the string doesn't contain query components, and thus,
     * the field queryStarted is set to false.
     * The behaviour in this scenario is to replace the blank space to empty string, turning "foo bar" into "foobar"
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a string with without blank spaces
     */
    private String handleNonQuerySpaces(String newUrlPortion) {
        return StringUtils.replaceBlankSpaceWithEmptyStrings(newUrlPortion);
    }

    /**
     * Helper method for handling either a portion of the URL that doesn't contain query components and
     * portion that contains query components, thus field queryStarted set to either true or false.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a string without the blank spaces.
     */
    private String handleSpaces(final String newUrlPortion) {
        if(Boolean.FALSE.equals(queryStringStarted)){
            return handleNonQuerySpaces(newUrlPortion);
        }else{
            return handleQuerySpaces(newUrlPortion);
        }
    }

    /**
     * Helper that hadles a scenario that the portion to be processed contains queryComponents.
     * In this case, the correct behaviour is to replace blank spaces with empty strings if the portion of the query is
     * part of the key, eg: "?key 1" turns into "?key1", and to replace empty spaces with + character when the portion
     * seems to be a value, eg: "foo bar" turns into "foo+bar"
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a string without the blank spaces.
     */
    private String handleQuerySpaces(String newUrlPortion) {
        if(url.endsWith(QUERY.getValue())){
            return StringUtils.replaceBlankSpaceWithEmptyStrings(newUrlPortion);
        }else if(url.endsWith(QUERY_EQUALS.getValue())){
            return StringUtils.replaceBlankSpaceWithPlusIcon(newUrlPortion);
        }else {
            return StringUtils.replaceBlankSpaceWithEmptyStrings(newUrlPortion);
        }
    }

    /**
     * Helper that handles a scenario that the scheme is present and URL is empty.
     * The if this condition matches, then the step will append the POST_SCHEME_PORTION (://) to it.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return the resulting object with a new state
     */
    private RawUriStringBuilder handleSchemeAndEmptyUrlScenario(
            final String url,
            final String newUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        if(Boolean.TRUE.equals(checkIfSchemeAndUrlEmpty(url, newUrlPortion))){
            return new RawUriStringBuilder(newUrlPortion + POST_SCHEME_PORTION.getValue(), pathStarted, portDetected,
                    determineIfQueryHasStarted(queryStringStarted, newUrlPortion));
        }
        return handleNotSchemeWithEmptyURLScenario(url, newUrlPortion, pathStarted, portDetected,
                queryStringStarted);
    }

    /**
     * Helper that handles whether the portion doesn't contain a scheme and the URL field is empty.
     * If the condition matches, then it should add the default scheme (https) before the current portion.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return the resulting object with a new state
     */
    private RawUriStringBuilder handleNotSchemeWithEmptyURLScenario(
            final String url,
            final String newUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        // Defaults to https scheme if scheme isn't provided.
        if(Boolean.TRUE.equals(checkIfNotSchemeAndURLIsEmpty(url, newUrlPortion))){
            return new RawUriStringBuilder(produceHttpsDefaultScheme() + newUrlPortion, pathStarted,
                    portDetected, determineIfQueryHasStarted(queryStringStarted, newUrlPortion));
        }
        return handleSchemeUrlAndPostSchemePortionScenario(url, newUrlPortion, pathStarted, portDetected, queryStringStarted);
    }


    /**
     * Helper for handling the condition that both, the URL field already contains some content, and the portion to add
     * Doesn't contain the POST_SCHEE_PORTION (://). In this case, just place it between the URL and Portion, making
     * "ftpwww" become "ftp://www"
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return the resulting object with a new state
     */
    private RawUriStringBuilder handleSchemeUrlAndPostSchemePortionScenario(
            final String url,
            final String newUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        if(Boolean.TRUE.equals(checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion(url, newUrlPortion))){
            return new RawUriStringBuilder( url+ POST_SCHEME_PORTION.getValue() +newUrlPortion, pathStarted, portDetected,
                determineIfQueryHasStarted(queryStringStarted, newUrlPortion));
        }
        return handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion(url, newUrlPortion,
                pathStarted, portDetected,queryStringStarted);
    }

    /**
     * Helper that handles the scenario where POST_SCHEME_PORTION (://) is present in both, URL field, and in the portion
     * to append.
     * In this case, it should not duplicate the POST_SCHEME_PORTION, but replace the POST_SCHEME_PORTION of the
     * portion to append with an empty string.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return the resulting object with a new state
     */
    private RawUriStringBuilder handleUriSchemeWithPostSchemePortionAndTrimmedUrlContainingPostSchemePortion(
            final String url,
            final String newUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted){


        if(Boolean.TRUE.equals(url.contains(POST_SCHEME_PORTION.getValue()) && newUrlPortion
                .contains(POST_SCHEME_PORTION.getValue()))){
            return new RawUriStringBuilder(
                    url + newUrlPortion.replace(POST_SCHEME_PORTION.getValue(), ""),
                    pathStarted,
                    portDetected,
                    determineIfQueryHasStarted(queryStringStarted, newUrlPortion)
            );
        }
        return handleStartedPathScenario(url, newUrlPortion, pathStarted, portDetected,queryStringStarted);
    }


    /**
     * Helper that changes the appending strategy when the path portion of the URL is detected.
     * In this case, the trailing slashes are to be removed. from the end of the URL field and both sides of the current
     * URL portion.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return the resulting object with a new state
     */
    private RawUriStringBuilder handleStartedPathScenario(
            final String url,
            final String newUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        // If path started, remove trailing slashes from start and end of newUrlPortion ONLY if
        // query string portion hasn't started.
        if(Boolean.TRUE.equals(pathStarted) || Boolean.TRUE.equals(queryStringStarted)){
            return new RawUriStringBuilder(
                    mergeUrlAndTrimmedPortion(url, newUrlPortion, queryStringStarted),
                    true,
                    portDetected,
                    determineIfQueryHasStarted(queryStringStarted, newUrlPortion)
            );
        }
        return handlePathNeedsToBeStartedScenario(url, newUrlPortion, pathStarted, portDetected, queryStringStarted);
    }


    /**
     * This helper method will only serve to change the pathStartedFlag to true, in case it detects a slash that is not
     * part of the POST_SCHEME_PORTION (://) is present in the url or url portion. Since it's immutable, the "change"
     * is through returning a new object with the flag set to true.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return the resulting object with a new state
     */
    private RawUriStringBuilder handlePathNeedsToBeStartedScenario(
            final String url,
            final String newUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted) {


        // Verifying if path needs to be started at this point.
        if(checkIfPathNeedsToBeStarted(url, newUrlPortion)){
            return new RawUriStringBuilder(url+newUrlPortion, true, portDetected,
                    determineIfQueryHasStarted(queryStringStarted, newUrlPortion));
        }

        return handlePortDetectionCheck(url, newUrlPortion, pathStarted, portDetected, queryStringStarted);

    }

    /**
     * Handles a scenario where the current portion of the URI to append or the URI fields represents the port portion,
     * so the area between the host and path or query.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param pathStarted The flag that represents whether the appending is at the path stage.
     * @param portDetected The flag that represents whether the port has been detected or not.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return the resulting object with a new state
     */
    private RawUriStringBuilder handlePortDetectionCheck(
            final String url,
            final String newUrlPortion,
            final Boolean pathStarted,
            final Boolean portDetected,
            final Boolean queryStringStarted){


        if(checkPortDetection(url, newUrlPortion)){

            final String result = retrievePartAfterPatternMatcher(url + newUrlPortion, ":\\d+");
            final Boolean queryStringHasStarted = determineIfQueryHasStarted(queryStringStarted, newUrlPortion);
            final String suffix = Boolean.TRUE.equals(queryStringHasStarted)? "" : PATH_SLASH.getValue();

            return new RawUriStringBuilder((url+newUrlPortion).replace(result, result + suffix),
                    true, true, queryStringHasStarted);
        }

        // End of flow
        return new RawUriStringBuilder((url + newUrlPortion), pathStarted, portDetected,
                queryStringStarted ||newUrlPortion.contains(QUERY.getValue()) || url
                        .contains(QUERY.getValue()));
    }





    /*
    End of handlers
     */




    /*
    MISC
     */

    /**
     * It merges the current URL field and the new URL portion by making sure to remove trimming slashes or query
     * components so that they don't duplicate.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @return a string resulted from the appending of url and newUrlportion.
     */
    private String mergeUrlAndTrimmedPortion(
            final String url, final String newUrlPortion, Boolean queryStringStarted
    ) {
        if(Boolean.TRUE.equals(queryStringStarted)){
            // Trim the query strings components: ?, = and &
            return trimQueryComponentsAndMerge(url, newUrlPortion);
        }else{
            // trim the slashes (path has started)
            return ((trimSlashes(url) +
                    (Boolean.TRUE.equals(this.queryStringStarted)?"":PATH_SLASH.getValue())+ trimSlashes(newUrlPortion))
                    .replace(PATH_SLASH.getValue() + QUERY.getValue(), QUERY.getValue()));
        }
    }

    /**
     * Method that only returns the "https" string.
     * @return a string with "https" in it.
     */
    private String produceHttpsDefaultScheme() {
        return DEFAULT_SCHEME.getValue() + POST_SCHEME_PORTION.getValue();
    }

    /*
    END OF MISC
     */




    /*
        CONDITION HELPERS
     */

    /**
     * Check if the URL contains the substring "://" on it.
     * @param url The url of the current object.
     * @return a boolean.
     */
    private boolean checkPostSchemeCondition(String url) {
        return (!url.contains(POST_SCHEME_PORTION.getValue()) || url.startsWith(POST_SCHEME_PORTION.getValue())) && Boolean
                .FALSE.equals(URISchemes.isScheme(url.trim()));
    }

    /**
     * Checks if the port is url or url portion to add contains the regex of a port (:DIGIT)
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a boolean.
     */
    private boolean checkPortDetection(final String url, final String newUrlPortion) {
        return url.matches(PORT_REGEX.getValue()) || newUrlPortion.matches(PORT_REGEX.getValue()) || url
                .endsWith(PORT_START_DELIMITER.getValue())
                && newUrlPortion.matches("\\d+.?") || (url+newUrlPortion).matches(PORT_REGEX.getValue());
    }

    /**
     * Checks if the URL or the current portion of URL to append contains the the QUERY (?) component.
     * @param queryStringStarted The flag that represents whether the query string started or not.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a boolean.
     */
    private Boolean determineIfQueryHasStarted(final Boolean queryStringStarted, final String newUrlPortion) {
        return queryStringStarted|| newUrlPortion.contains(QUERY.getValue());
    }

    /**
     * It determines if it's the correct time to set the pathStarted flag to true, by detecting whether
     * the PATH_SLASH (/) is present in either url or current url portion to add.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a boolean.
     */
    private boolean checkIfPathNeedsToBeStarted(final String url, final String newUrlPortion) {
        return (url+newUrlPortion).replace(POST_SCHEME_PORTION.getValue(),"")
                .contains(PATH_SLASH.getValue());
    }

    /**
     * It checks whether the scenario falls into the URL being a scheme and the portion to add doesn't contain the (://)
     * @param url  The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a boolean.
     */
    private Boolean checkIfUrlIsSchemeAndTrimmedUrlDoesntContainPostSchemePortion(
            final String url, final String newUrlPortion
    ) {
        return URISchemes.isScheme(url) && !newUrlPortion.contains(POST_SCHEME_PORTION.getValue());
    }

    /**
     * Checking if it falls into the scenario of the current portion to add not being a scheme and the URL is empty.
     * @param url The url of the current object.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @return a boolean.
     */
    private Boolean checkIfNotSchemeAndURLIsEmpty(final String url, final String newUrlPortion) {
        return !URISchemes.isScheme(newUrlPortion) && url.isEmpty();
    }

    /**
     * It checks if the current portion to add is matches any scheme and the URL is empty.
     * @param newUrlPortion The portion of the URL to be appended to the current url field.
     * @param url The url of the current object.
     * @return a boolean.
     */
    private Boolean checkIfSchemeAndUrlEmpty(final String url, final String newUrlPortion) {
        return URISchemes.isScheme(newUrlPortion) && url.isEmpty();
    }

    /*
       END OF CONDITION HELPERS
     */


    @Override
    public String toString() {
        return build();
    }
}
