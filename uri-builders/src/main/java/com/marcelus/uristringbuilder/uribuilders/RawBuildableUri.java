package com.marcelus.uristringbuilder.uribuilders;

/**
 * The RawBuildableUri interface.
 * Since RawBuildableUri doesn't use any structure, it will only contain the append method for composing the build.
 * @param <T> the generic type that the object will wrap.
 */
public interface RawBuildableUri<T> extends BuildableUri<T> {


    /**
     * The appending method to compose the BuildableUri object.
     * @param t the t input.
     * @return an object instance of RawBuildable URI.
     */
    RawBuildableUri<T> append(final T t);
}
