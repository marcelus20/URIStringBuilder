package com.marcelus.uristringbuilder.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SlashTrimmersTest {

    @Test
    void testingTrimSlashAtStart(){
        final String result = SlashTrimmers.trimSlashAtStart("/foo/").orElse("");

        assertEquals("foo/", result);
    }

    @Test
    void testingTrimSlashAtStartWithMultipleSlashes(){
        final String result = SlashTrimmers.trimSlashAtStart("/////foo/").orElse("");

        assertEquals("foo/", result);
    }

    @Test
    void testingTrimSlashAtEnd(){
        final String result = SlashTrimmers.trimSlashAtEnd("/foo/").orElse("");

        assertEquals("/foo", result);
    }

    @Test
    void testingTrimSlashAtEndWithMultipleSlashes(){
        final String result = SlashTrimmers.trimSlashAtEnd("//foo///////").orElse("");

        assertEquals("//foo", result);
    }

    @Test
    void testingTrimSlashAtBothSides(){
        final String result = SlashTrimmers.trimSlashes("/foo/");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimSlashAtBothSidesWithMutipleSlashes(){
        final String result = SlashTrimmers.trimSlashes("////foo////");

        assertEquals("foo", result);
    }

}