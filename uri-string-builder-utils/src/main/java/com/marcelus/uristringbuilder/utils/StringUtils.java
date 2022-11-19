package com.marcelus.uristringbuilder.utils;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class StringUtils {

    private StringUtils(){

    }

    public static Optional<String> trim(final String s){
        return Optional.ofNullable(s)
                .map(String::trim);
    }

    public static Optional<String> trim(final String string, final String character){
        return trim(string)
                .flatMap(s->StringUtils.trimCharAtStart(s, character))
                .flatMap(s->StringUtils.trimCharAtEnd(s, character));
    }

    public static Optional<String> trimCharAtEnd(String string, String character) {
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

    public static Optional<String> trimCharAtStart(String string, String character) {
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

    private static String escape(final String character){
        // List of reserved chars
        List<String> reservedChars= Arrays.asList("\\",".","<",">","*","+","-","=","!","?","^","$","|");
        return reservedChars.contains(character) ? String.format("\\%s", character) : character;
    }
}
