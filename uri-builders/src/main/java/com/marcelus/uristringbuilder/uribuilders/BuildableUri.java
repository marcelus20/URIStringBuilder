package com.marcelus.uristringbuilder.uribuilders;

/**
 * The buildable URI interface.
 * @param <T> The type of the build return.
 */
public interface BuildableUri<T> {

    /**
     * Method for building the BuildableUri object.
     * @return the built object.
     */
    T build();

}
