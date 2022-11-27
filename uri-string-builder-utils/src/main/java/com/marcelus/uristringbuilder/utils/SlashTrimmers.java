package com.marcelus.uristringbuilder.utils;

import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PATH_SLASH;

/**
 * Utility class for trimming slashes from string.
 */
public class SlashTrimmers {

    /**
     * Private constructor to avoid instantiating.
     */
    private SlashTrimmers(){

    }

    /**
     * Removes slashes at the end and at the start of the string.
     * @param path string to be trimmed.
     * @return a new string with slashes removed from end and start.
     */
    public static String trimSlashes(final String path) {
        return Optional.ofNullable(path)
                .map(String::trim)
                .flatMap(SlashTrimmers::trimSlashAtStart)
                .flatMap(SlashTrimmers::trimSlashAtEnd)
                .orElse("");
    }

    /**
     * Removes slashes at the end of the string.
     * @param path string to be trimmed.
     * @return a new string with slashes removed from end.
     */
    public static Optional<String> trimSlashAtEnd(final String path){
        return Optional.ofNullable(path)
                .map(String::trim)
                .map(p->{
                    if(p.endsWith(PATH_SLASH.getValue())){
                        return p.replaceAll(String.format("%s*$",PATH_SLASH.getValue()), "");
                    }
                    return p;
                });
    }

    /**
     * Removes slashes at the start of the string.
     * @param path string to be trimmed.
     * @return a new string with slashes removed from start.
     */
    public static Optional<String> trimSlashAtStart(final String path) {
        return Optional.ofNullable(path)
                .map(String::trim)
                .map(nonNullPath->{
                    if(nonNullPath.startsWith(PATH_SLASH.getValue())){
                        return nonNullPath.replaceAll(String.format("^%s*", PATH_SLASH.getValue()), "");
                    }
                    return nonNullPath;
                });
    }
}
