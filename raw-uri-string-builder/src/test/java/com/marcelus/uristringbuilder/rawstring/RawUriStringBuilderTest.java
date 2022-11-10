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

    /*
     *Scenario: :// not available, the RawUriStringBuilder should still infer and add after the protocol.
     */
    @Test
    void testingWithMissingColonFollowedByTwoSlashesScenario(){
        String url = new RawUriStringBuilder("https")
//                .append("://") <-- Missing this part.
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

    /*
     *Scenario: :// and : (after host) not available, the RawUriStringBuilder should still infer and add after the protocol.
     */
    @Test
    void testingWithMissingColonFollowedByTwoSlashesScenarioAndMissingPortColon(){
        String url = new RawUriStringBuilder("https")
//                .append("://") <-- Missing this part.
                .append("www.foo.bar.com")
//                .append(":") <- Missing colon after host:
                .append(8080) // <- This should be considered part of the host
                .append("/")
                .append("path/portion")
                .append("/")
                .append("another/path/portion")
                .build();

        assertEquals("https://www.foo.bar.com8080/path/portion/another/path/portion", url);
    }

    /*
     *Scenario: / missing after the host or port number, the RawStringBuilder should still add it to begin the path portion
     */
    @Test
    void testingWithMissingSlashAfterPortNumber(){
        String url = new RawUriStringBuilder("https")
//                .append("://") <-- Missing this part.
                .append("www.foo.bar.com")
//                .append(":") <- Missing colon after host:
                .append(8080) // <- This should be considered part of the host
//                .append("/") This missing slash should be added.
                .append("path/portion")
                .append("/")
                .append("another/path/portion")
                .build();

        assertEquals("https://www.foo.bar.com8080/path/portion/another/path/portion", url);
    }

    /*
     *Scenario: Trailing Slashes at path portions should not duplicate when next portion is appended.
     */
    @Test
    void testingScenariosWithTrailingSlashesOnPath(){
        String url = new RawUriStringBuilder("https")
                .append("://")
                .append("www.foo.bar.com")
                .append(":")
                .append(8080)
                .append("/")
                .append("/path/portion/") // <- Trailing Slashes on Path
                .append("/") // <- Trailing Slashes on Path
                .append("/another/path/portion/") // <- Trailing Slashes on Path
                .build();

        assertEquals("https://www.foo.bar.com:8080/path/portion/another/path/portion", url);
    }
}
