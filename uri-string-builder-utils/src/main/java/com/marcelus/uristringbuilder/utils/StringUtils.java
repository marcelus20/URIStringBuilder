package com.marcelus.uristringbuilder.utils;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * String utility class.
 */
public class StringUtils {

    /**
     * Private constructor for avoiding instantiating.
     */
    private StringUtils(){

    }

    /**
     * Turns a string into an optional and trims it if it is present.
     * @param s the string input.
     * @return the optional of the trimmed string.
     */
    public static Optional<String> trim(final String s){
        return Optional.ofNullable(s)
                .map(String::trim);
    }

    /**
     * Trims a string based on a character rather than the default blank space.
     * @param string the string to be trimmed.
     * @param character the character that should be removed from the start and end of the string.
     * @return a new string without the character at the end and start.
     */
    public static Optional<String> trim(final String string, final String character){
        return trim(string)
                .flatMap(s->StringUtils.trimCharAtStart(s, character))
                .flatMap(s->StringUtils.trimCharAtEnd(s, character));
    }

    /**
     * Trims the string only at the end based on a character.
     * @param string the string to be trimmed.
     * @param character the character that should be removed from the start and end of the string.
     * @return a new string without the character at the end.
     */
    public static Optional<String> trimCharAtEnd(final String string, final String character) {
        return Optional.ofNullable(string)
                .flatMap(nonNullString->Optional.ofNullable(character)
                        .map(nonNullCharacter->{
                            if(nonNullString.endsWith(nonNullCharacter)){
                                return nonNullString.replaceAll(String.format("%s*$",
                                        escape(nonNullCharacter)), "");
                            }
                            return nonNullString;
                        }));
    }

    /**
     * Trims the string only at the start based on a character.
     * @param string the string to be trimmed.
     * @param character the character that should be removed from the start and end of the string.
     * @return a new string without the character at the start.
     */
    public static Optional<String> trimCharAtStart(final String string, final String character) {
        return Optional.ofNullable(string)
                .flatMap(nonNullString->Optional.ofNullable(character)
                        .map(nonNullCharacter->{
                            if(nonNullString.startsWith(nonNullCharacter)){
                                return nonNullString.replaceAll(String.format("^%s*",
                                        escape(nonNullCharacter)), "");
                            }
                            return nonNullString;
                        }));
    }

    /**
     * Helper method for escaping a character if character is contained in the reservedChars list.
     * @param character the character to be escaped.
     * @return a string character with double backslash at the start if character was escaped.
     */
    private static String escape(final String character){
        // List of reserved chars
        List<String> reservedChars= Arrays.asList("\\",".","<",">","*","+","-","=","!","?","^","$","|");
        return reservedChars.contains(character) ? String.format("\\%s", character) : character;
    }

    /**
     * Takes a generic object and turns it into a string using the String.valueOf.
     * @param object the object to be converted into a string.
     * @return the string representation of the object.
     */
    public static Optional<String> convertObjectToString(final Object object){
        return Optional.ofNullable(object)
                .map(String::valueOf)
                .flatMap(StringUtils::trim);
    }

    /**
     * It retrieves the substring that matches the pattern matching.
     * Eg: if the string is "abc" and pattern matching is "ab", this function will return "ab".
     * @param string the string where substring will be extracted.
     * @param patternString the pattern matching used to extract the substring.
     * @return the substring of the original string.
     */
    public static String retrievePartAfterPatternMatcher(final String string, final String patternString) {
        final Pattern p = Pattern.compile(patternString);
        final Matcher m = p.matcher(string);
        if(m.find()){
            return m.group();
        }else {
            return "";
        }
    }

    /**
     * Takes a string and replaces all instances of blank spaces with empty strings ("").
     * @param string the string input.
     * @return the string without the blank spaces.
     */
    public static String replaceBlankSpaceWithEmptyStrings(final String string) {
        return trim(string).map(s->s.replace(" ", "")).orElse("");
    }

    /**
     * Takes a string and replaces all instances of blank spaces with plus character (+).
     * @param string the string input.
     * @return the string without the blank spaces.
     */
    public static String replaceBlankSpaceWithPlusIcon(final String string) {
        return trim(string).map(s->s.replace(" ", "+")).orElse("");
    }
}
