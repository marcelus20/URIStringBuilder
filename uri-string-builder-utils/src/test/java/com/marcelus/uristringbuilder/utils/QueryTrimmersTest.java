package com.marcelus.uristringbuilder.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class QueryTrimmersTest {

    @Test
    void testingTrimQueryAtStart(){
        final String result = QueryTrimmers.trimQueryAtStart("?foo");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAtStartWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryAtStart("?????foo");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAtEnd(){
        final String result = QueryTrimmers.trimQueryAtEnd("foo???????");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAtEndWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryAtEnd("foo?????");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAtBothSides(){
        final String result = QueryTrimmers.trimQuery("?foo?");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAtBothSidesWithMultipleQueries(){
        final String result = QueryTrimmers.trimQuery("?????foo?????");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAndAtStart(){
        final String result = QueryTrimmers.trimQueryAndAtStart("&foo");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAndAtStartWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryAndAtStart("&&&&foo");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAndAtEnd(){
        final String result = QueryTrimmers.trimQueryAndAtEnd("foo&");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAndAtEndWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryAndAtEnd("foo&&&&&&&");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAndAtBothSides(){
        final String result = QueryTrimmers.trimQueryAnd("&foo&");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryAndAtBothSidesWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryAnd("&&&&&&foo&&&&&&&");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryEqualsAtStart(){
        final String result = QueryTrimmers.trimQueryEqualsAtStart("=foo");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryEqualsAtStartWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryEqualsAtStart("=====foo");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryEqualsAtEnd(){
        final String result = QueryTrimmers.trimQueryEqualsAtEnd("foo=");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryEqualsAtEndWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryEqualsAtEnd("foo======");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryEqualsAtBothSides(){
        final String result = QueryTrimmers.trimQueryEquals("=foo=");

        assertEquals("foo", result);
    }

    @Test
    void testingTrimQueryEqualsAtBothSidesWithMultipleQueries(){
        final String result = QueryTrimmers.trimQueryEquals("======foo=======");

        assertEquals("foo", result);
    }
}