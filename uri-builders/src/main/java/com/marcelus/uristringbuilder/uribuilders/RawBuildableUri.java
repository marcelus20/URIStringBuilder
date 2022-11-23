package com.marcelus.uristringbuilder.uribuilders;

public interface RawBuildableUri extends BuildableUri {


    RawBuildableUri append(final String urlPortion);
    RawBuildableUri append(final Integer urlPortion);
}
