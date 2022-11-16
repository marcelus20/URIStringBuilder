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

}