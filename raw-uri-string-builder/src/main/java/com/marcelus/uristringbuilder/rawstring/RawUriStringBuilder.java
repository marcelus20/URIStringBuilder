package com.marcelus.uristringbuilder.rawstring;

import com.marcelus.uristringbuilder.available.uri.schemes.URISchemes;


import java.util.Optional;

public final class RawUriStringBuilder {
    private final String url;
    private final Boolean pathStarted;
    private final Boolean queryStringStarted;

    public RawUriStringBuilder(final String url) {
        if((!url.contains("://") || url.startsWith("://")) && Boolean.FALSE.equals(URISchemes.isScheme(url.trim()))){
            this.url = "https://" + url.trim();
        }else{
            this.url = url.trim();
        }
        pathStarted = false;
        queryStringStarted = url.contains("?");
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
                .map(trimmedUrlPortion->{

                    if(Boolean.TRUE.equals(URISchemes.isScheme(trimmedUrlPortion)) && url.isEmpty()){
                        return new RawUriStringBuilder(trimmedUrlPortion + "://", pathStarted,
                                queryStringStarted|| trimmedUrlPortion.contains("?"));
                    }

                    // Defaults to https scheme if scheme isn't provided.
                    if(Boolean.FALSE.equals(URISchemes.isScheme(trimmedUrlPortion)) && url.isEmpty()){
                        return new RawUriStringBuilder("https://" + trimmedUrlPortion, pathStarted,
                                queryStringStarted || trimmedUrlPortion.contains("?"));
                    }

                    final Boolean queryStringHasStarted = queryStringStarted || trimmedUrlPortion.contains("?") ||
                            url.contains("?");
                    if(Boolean.TRUE.equals(URISchemes.isScheme(url)) && !trimmedUrlPortion.contains("://")){
                        return new RawUriStringBuilder( url+ "://" +trimmedUrlPortion, pathStarted,
                                queryStringHasStarted);
                    }

                    if(url.contains("://") && trimmedUrlPortion.contains("://")){
                        return new RawUriStringBuilder(url + trimmedUrlPortion.replace("://", ""),
                                pathStarted, queryStringHasStarted);
                    }


                    // If path started, remove trailing slashes from start and end of trimmedUrlPortion ONLY if
                    // query string portion hasn't started.
                    if(Boolean.TRUE.equals(pathStarted)){
                        return new RawUriStringBuilder((trimSlashes(url) +
                                (Boolean.TRUE.equals(queryStringStarted)?"":"/")+ trimSlashes(trimmedUrlPortion))
                                .replace("/?", "?"),
                                pathStarted, queryStringHasStarted);
                    }

                    // Verifying if path needs to be started at this point.
                    if((url+trimmedUrlPortion).replace("://","").contains("/")){
                        return new RawUriStringBuilder(url+trimmedUrlPortion, true, queryStringHasStarted);
                    }
                    return new RawUriStringBuilder((url + trimmedUrlPortion), pathStarted, queryStringHasStarted);
                }).orElse(new RawUriStringBuilder(""));
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
                    if (trimmedUrl.startsWith("/")) {
                        return trimmedUrl.substring(1);
                    }
                    return trimmedUrl;
                });
    }

    private Optional<String> removeSlashFromEnd(final String url){
        return Optional.ofNullable(url)
                .map(String::trim)
                .map(trimmedUrl->{
                    if (trimmedUrl.endsWith("/")) {
                        return trimmedUrl.substring(0, trimmedUrl.length() - 1);
                    }
                    return trimmedUrl;
                });
    }

    public String build() {
        return url;
    }
}
