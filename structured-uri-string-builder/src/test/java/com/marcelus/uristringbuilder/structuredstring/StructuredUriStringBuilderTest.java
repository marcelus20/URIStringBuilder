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
                .appendPort(32895)
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
                .appendPort(328)
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
}