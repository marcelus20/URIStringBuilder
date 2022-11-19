package com.marcelus.uristringbuilder.utils;

import java.util.Optional;

import static com.marcelus.uristringbuilder.utils.UriPortionConstants.PATH_SLASH;

public class SlashTrimmers {

    private SlashTrimmers(){

    }

    public static String trimSlashes(String path) {
        return Optional.ofNullable(path)
                .map(String::trim)
                .flatMap(SlashTrimmers::trimSlashAtStart)
                .flatMap(SlashTrimmers::trimSlashAtEnd)
                .orElse("");
    }

    public static Optional<String> trimSlashAtEnd(String path){
        return Optional.ofNullable(path)
                .map(String::trim)
                .map(p->{
                    if(p.endsWith(PATH_SLASH.getValue())){
                        return p.replaceAll(String.format("%s*$",PATH_SLASH.getValue()), "");
                    }
                    return p;
                });
    }

    public static Optional<String> trimSlashAtStart(String path) {
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
