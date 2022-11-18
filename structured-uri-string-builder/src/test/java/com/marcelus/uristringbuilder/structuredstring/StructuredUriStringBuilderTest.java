package com.marcelus.uristringbuilder.structuredstring;

import org.junit.jupiter.api.Test;

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
        assertEquals("http://www.foo.bar.bazz", result);
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
        assertEquals("http://www.foo.bar.bazz?ticket=12345&month11&name=misha&type=buyer", result);
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
        assertEquals("http://www.foo.bar.bazz?ticket?month?&name=misha&type", result);
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
        assertEquals("http://www.foo.bar.bazz?type&ticket?month?&name=misha", result);
    }

    @Test
    void testingReappending(){
        final String result = new StructuredUriStringBuilder()
                .appendScheme("http")
                .appendQuery("type")
                .appendQuery("id")
                .appendQuery("ticket")
                .appendQuery("month")
                .appendQuery("of")
                .appendQuery("the")
                .appendQuery("year")
                .appendHost("www.foo.bar.bazz")
                .appendQuery("name", "misha")
                .build();
        assertEquals("http://www.foo.bar.bazz?typeid&ticket?monthoftheyear?&name=misha", result);
    }
}