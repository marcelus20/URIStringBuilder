package com.marcelus.uristringbuilder.rawstring;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;


class RawUriStringBuilderTest
{
    /*
     *Happiest scenario: All string portions correctly positioned one after another.
     */
    @Test
    void testingHappyScenario(){
        String url = new RawUriStringBuilder("https")
                .append("://")
                .append("www.foo.bar.com")
                .append(":")
                .append(8080)
                .append("/")
                .append("path/portion")
                .append("/")
                .append("another/path/portion")
                .build();

        assertEquals("https://www.foo.bar.com:8080/path/portion/another/path/portion", url);
    }
}
