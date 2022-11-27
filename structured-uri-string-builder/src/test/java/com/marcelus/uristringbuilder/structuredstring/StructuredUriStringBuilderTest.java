package com.marcelus.uristringbuilder.structuredstring;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class StructuredUriStringBuilderTest {

    /*
     * Testing the happiest scenario where everything is provided in the correct order.
     */
    @Test
    void testingHappiestScenario(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("www.foo.bar.bazz")
                .appendPath("fizz")
                .appendPath("buzz")
                .appendQuery("ticket", "12345")
                .build();
        assertEquals("http://www.foo.bar.bazz/fizz/buzz?ticket=12345", result);
    }


    @Test
    void testingOmittingSchemeScenario(){
        final String result = new StructuredUriStringBuilder()
//                .appendScheme("http")
                .appendHost("www.foo.bar.bazz")
                .appendPath("fizz")
                .appendPath("buzz")
                .appendQuery("ticket", "12345")
                .build();
        assertEquals("www.foo.bar.bazz/fizz/buzz?ticket=12345", result);
    }


    @Test
    void testingOmittingHostScenario(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
//                .appendHost("www.foo.bar.bazz")
                .appendPath("fizz")
                .appendPath("buzz")
                .appendQuery("ticket", "12345")
                .build();
        assertEquals("http://fizz/buzz?ticket=12345", result);
    }


    @Test
    void testingOmittingPathScenario(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("www.foo.bar.bazz")
//                .appendPath("fizz")
//                .appendPath("buzz")
                .appendQuery("ticket", "12345")
                .build();
        assertEquals("http://www.foo.bar.bazz?ticket=12345", result);
    }


    @Test
    void testingOmittingQueryScenario(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("www.foo.bar.bazz")
                .appendPath("fizz")
                .appendPath("buzz")
//                .appendQuery("ticket", "12345")
                .build();
        assertEquals("http://www.foo.bar.bazz/fizz/buzz", result);
    }

    @Test
    void testingMultipleQueryKeyValuePairsScenario(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("www.foo.bar.bazz")
                .appendQuery("ticket", "12345")
                .appendQuery("month", "11")
                .appendQuery("name", "misha")
                .appendQuery("type", "buyer")
                .build();
        assertEquals("http://www.foo.bar.bazz?ticket=12345&month=11&name=misha&type=buyer", result);
    }

    @Test
    void testingMultipleQueryKeyValuePairsWithEmptyValueScenario(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("www.foo.bar.bazz")
                .appendQuery("ticket")
                .appendQuery("month")
                .appendQuery("name", "misha")
                .appendQuery("type")
                .build();
        assertEquals("http://www.foo.bar.bazz?ticket&month&name=misha&type", result);
    }

    @Test
    void testingInvertingTheOrderOfCalls(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendQuery("type")
                .appendQuery("ticket")
                .appendQuery("month")
                .appendHost("www.foo.bar.bazz")
                .appendQuery("name", "misha")
                .build();
        assertEquals("http://www.foo.bar.bazz?type&ticket&month&name=misha", result);
    }

    @Test
    void testingReappending(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendQuery("type")
                .appendQuery("id")
                .appendQuery("ticket")
                .appendQuery("month")
                .appendHost("www.foo.bar.bazz")
                .appendQuery("name", "misha")
                .build();
        assertEquals("http://www.foo.bar.bazz?type&id&ticket&month&name=misha", result);
    }

    @Test
    void testingAppendingPort(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendQuery("type")
                .appendQuery("id")
                .appendQuery("ticket")
                .appendQuery("month")
                .appendHost("www.foo.bar.bazz")
                .appendQuery("name", "misha")
                .appendPort("32895")
                .build();
        assertEquals("http://www.foo.bar.bazz:32895?type&id&ticket&month&name=misha", result);
    }

    @Test
    void testingAppendingPortInSeparatedAppends(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendPort("20")
                .appendQuery("type")
                .appendQuery("id")
                .appendQuery("ticket")
                .appendQuery("month")
                .appendHost("www.foo.bar.bazz")
                .appendQuery("name", "misha")
                .appendPort("328")
                .build();
        assertEquals("http://www.foo.bar.bazz:20328?type&id&ticket&month&name=misha", result);
    }

    @Test
    void testingAppendingQueriesWithTrailingQuestionTag(){
        final String result = new StructuredUriStringBuilder()
                .appendHost("www.foo.bar")
                .appendQuery("?foo=bar")
                .build();
        assertEquals("www.foo.bar?foo=bar", result);
    }

    @Test
    void testingAppendingQueriesWithTrailingQuestionTagUsing2ArgumentAppendQuery(){
        final String result = new StructuredUriStringBuilder()
                .appendHost("www.foo.bar")
                .appendQuery("?foo", "bar")
                .build();
        assertEquals("www.foo.bar?foo=bar", result);
    }

    @Test
    void testingAppendingQueriesWithTrailingQuestionTagAndTrailingEqualsSignUsing2ArgumentAppendQuery(){
        final String result = new StructuredUriStringBuilder()
                .appendHost("www.foo.bar")
                .appendQuery("?foo", "=bar")
                .build();
        assertEquals("www.foo.bar?foo=bar", result);
    }


    @Test
    void testingAppendingQueriesWithTrailingAndOperatorInQuery(){
        final String result = new StructuredUriStringBuilder()
                .appendHost("www.foo.bar")
                .appendQuery("foo", "bar&")
                .appendQuery("fizz", "buzz")
                .build();
        assertEquals("www.foo.bar?foo=bar&fizz=buzz", result);
    }

    @Test
    void testingAppendingQueriesWithWrongAndOperatorInFirstQuery(){
        final String result = new StructuredUriStringBuilder()
                .appendHost("www.foo.bar")
                .appendQuery("&foo", "bar&") // <-- The &foo should be removed and replaced with ?foo
                .appendQuery("fizz", "buzz")
                .build();
        assertEquals("www.foo.bar?foo=bar&fizz=buzz", result);
    }

    @Test
    void testingHostInDifferentAppends(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("localhost")
                .appendHost("8081")
                .appendPath("v1/api/users")
                .appendQuery("activated")
                .build();
        assertEquals("http://localhost8081/v1/api/users?activated", result);
    }

    @Test
    void testingEmptySpacesInScheme(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("ht tp")
                .appendHost("localhost")
                .appendPort("8081")
                .appendPath("v1/api/users")
                .appendQuery("activated")
                .build();
        assertEquals("http://localhost:8081/v1/api/users?activated", result);
    }

    @Test
    void testingEmptySpacesInHost(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("192.1 68.7 7.3")
                .appendPort("8081")
                .appendPath("v1/api/users")
                .appendQuery("activated")
                .build();
        assertEquals("http://192.168.77.3:8081/v1/api/users?activated", result);
    }

    @Test
    void testingEmptySpacesInPort(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("localhost")
                .appendPort("808 1")
                .appendPath("v1/api/users")
                .appendQuery("activated")
                .build();
        assertEquals("http://localhost:8081/v1/api/users?activated", result);
    }

    @Test
    void testingEmptySpacesInPath(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("localhost")
                .appendPort("8081")
                .appendPath("v1/ap i/users")
                .appendQuery("activated")
                .build();
        assertEquals("http://localhost:8081/v1/api/users?activated", result);
    }

    @Test
    void testingEmptySpacesInQueryKeyString(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("ht tp")
                .appendHost("localhost")
                .appendPort("8081")
                .appendPath("v1/api/users")
                .appendQuery("activ ated")
                .build();
        assertEquals("http://localhost:8081/v1/api/users?activated", result);
    }

    @Test
    void testingEmptySpacesInQueryValueString(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("ht tp")
                .appendHost("localhost")
                .appendPort("8081")
                .appendPath("v1/api/articles")
                .appendQuery("contains", "Once upon a time, there was a beast")
                .build();
        assertEquals("http://localhost:8081/v1/api/articles?contains=Once+upon+a+time,+there+was+a+beast", result);
    }

    @Test
    void testingAppendShchemeInDifferentAppends(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("ht")
                .appendScheme("tp")
                .appendHost("localhost")
                .appendPort("8081")
                .build();

        assertEquals("http://localhost:8081", result);
    }

    @Test
    void testingAppendPortsInDifferentAppends(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendHost("localhost")
                .appendPort("80")
                .appendPort("81")
                .build();

        assertEquals("http://localhost:8081", result);
    }

    @Test
    void testingAppendQueryUsingMap(){
        Map<String, String> map = new HashMap<>();
        map.put("fizz", "buzz");
        map.put("bar", "foo");
        final String result = new StructuredUriStringBuilder()
                .appendHost("http")
                .appendScheme("foo")
                .appendQuery(map)
                .build();


        assertEquals("foo://http?bar=foo&fizz=buzz", result);
    }

    @Test
    void testingAppendQueryUsingMapsInDifferentAppends(){
        Map<String, String> map = new HashMap<>();
        map.put("fizz", "buzz");
        Map<String, String> map2 = new HashMap<>();
        map2.put("bar", "foo");
        final String result = new StructuredUriStringBuilder()
                .appendHost("http")
                .appendScheme("foo")
                .appendQuery(map)
                .appendQuery(map2)
                .build();


        assertEquals("foo://http?fizz=buzz&bar=foo", result);
    }

    @Test
    void testingAppendQueryUsingMapsInDifferentAppendsAndNullValues(){
        Map<String, String> map = new HashMap<>();
        map.put("fizz", "buzz");
        Map<String, String> map2 = new HashMap<>();
        map2.put("bar", null);
        final String result = new StructuredUriStringBuilder()
                .appendHost("http")
                .appendScheme("foo")
                .appendQuery(map)
                .appendQuery(map2)
                .build();


        assertEquals("foo://http?fizz=buzz&bar", result);
    }

    @Test
    void testingAppendQueryUsingMapsInDifferentAppendsAndNullValuesInBothMaps(){
        Map<String, String> map = new HashMap<>();
        map.put("fizz", null);
        Map<String, String> map2 = new HashMap<>();
        map2.put("bar", null);
        final String result = new StructuredUriStringBuilder()
                .appendHost("http")
                .appendScheme("foo")
                .appendQuery(map)
                .appendQuery(map2)
                .build();


        assertEquals("foo://http?fizz&bar", result);
    }

    @Test
    void testingAppendQueryUsingMapsInDifferentAppendsAndNullKeys(){
        Map<String, String> map = new HashMap<>();
        map.put("fizz", "buzz");
        Map<String, String> map2 = new HashMap<>();
        map2.put(null, "bar");
        final String result = new StructuredUriStringBuilder()
                .appendHost("http")
                .appendScheme("foo")
                .appendQuery(map)
                .appendQuery(map2)
                .build();


        assertEquals("foo://http?fizz=buzz", result);
    }

    @Test
    void testingAppendQueryUsingMapsWithKeysAndValuesNull(){
        Map<String, String> map = new HashMap<>();
        map.put(null, null);
        Map<String, String> map2 = new HashMap<>();
        map2.put(null, null);
        final String result = new StructuredUriStringBuilder()
                .appendHost("http")
                .appendScheme("foo")
                .appendQuery(map)
                .appendQuery(map2)
                .build();


        assertEquals("foo://http", result);
    }

}