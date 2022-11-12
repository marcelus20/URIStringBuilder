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
                .append(8080) // <- This should be considered part of the host since colon was missing in previous append
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

        assertEquals("https://www.foo.bar.com8080path/portion/another/path/portion", url);
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

    /*
     *Scenario: If Schema isn't passed, RawUriStringBuilder should infer to an https schema.
     */
    @Test
    void defaultsToHttpsIfSchemaIsOmitted(){
        String url = new RawUriStringBuilder("www.foo.bar").build();

        assertEquals("https://www.foo.bar", url);
    }

    /*
     *Scenario: Do not wrap string with slashes when portion is part of a query string.
     */
    @Test
    void doNotWrapWithSlashesStringsThatRepresentQueryStringParts(){
        String url = new RawUriStringBuilder("www.foo.bar")
                .append("/api") // <- This will be wrapped with slashes /api/
                .append("v1") // <- Slash is supposed to be added only at the start /v1
                .append("?")
                .append("key")
                .append("=")
                .append("value").build();

        assertEquals("https://www.foo.bar/api/v1?key=value", url);
    }

    @Test
    void colonFollowedByDoubleSlashesShouldBeAppendedAfterTheSchemeIfOmitted(){
        String url = new RawUriStringBuilder()
                .append("ssh")
                .append("192.168.0.58")
                .build();
        assertEquals("ssh://192.168.0.58", url);
    }

    @Test
    void colonFollowedByDoubleSlashesShouldNotDuplicateWhenProvided(){
        String url = new RawUriStringBuilder()
                .append("ssh")
                .append("://") // <-providing ://
                .append("192.168.0.58")
                .build();
        assertEquals("ssh://192.168.0.58", url);
    }

    @Test
    void pathShouldBeCorrectlyAppended(){
        String url = new RawUriStringBuilder()
                .append("ssh")
                .append("192.168.0.58")
                .append("/path")
                .build();
        assertEquals("ssh://192.168.0.58/path", url);
    }

    @Test
    void pathShouldBeCorrectlyAppendedButSchemePassedInTheConstructorParam(){
        String url = new RawUriStringBuilder("ssh")
                .append("192.168.0.58")
                .append("/path")
                .build();
        assertEquals("ssh://192.168.0.58/path", url);
    }

    @Test
    void pathShouldBeCorrectlyAppendedWhenPreviousStringEndsWithSlash(){
        String url = new RawUriStringBuilder()
                .append("ssh")
                .append("192.168.0.58/")
                .append("path")
                .build();
        assertEquals("ssh://192.168.0.58/path", url);
    }

    @Test
    void pathShouldNotDuplicateSlashesWhenPreviousAppendEndsWithSlashAndNextStartsWithSlash(){
        String url = new RawUriStringBuilder()
                .append("ssh")
                .append("192.168.0.58/")
                .append("/path")
                .build();
        assertEquals("ssh://192.168.0.58/path", url);
    }

    @Test
    void lastPathWithTrailingSlashScenario(){
        String url = new RawUriStringBuilder()
                .append("ssh")
                .append("192.168.0.58/")
                .append("/path/") //<- trailing slash
                .build();
        assertEquals("ssh://192.168.0.58/path", url);
    }

    @Test
    void ifSchemePortionIsFoundInTheMiddleOfTheChainItWontBeConsideredScheme(){
        String url = new RawUriStringBuilder()
                .append("ssh")
                .append("192.168.0.58/")
                .append("/z39.50s")
                .build();
        assertEquals("ssh://192.168.0.58/z39.50s", url);
    }

    @Test
    void handlingOfSlashlessPathWhenPortIsDetected(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58")
                .append(":8080") // <-port provided
                .append("api") // <- So slash will be automatically provided.
                .build();
        assertEquals("ftp://192.168.0.58:8080/api", url);
    }


    @Test
    void handlingColonSeparatedFromPortNumber(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:") // <-- Colon separated from port number
                .append("8080") // <-port provided
                .append("api") //
                .build();
        assertEquals("ftp://192.168.0.58:8080/api", url);
    }

    @Test
    void handlingPortSplitIntoTwoAppendsWillTreatSecondAppendAsPathAndNotPort(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:80") // <-- one part of port here
                .append("80") // other port part here, should then consider path, not part of the port.
                .append("api")
                .build();
        assertEquals("ftp://192.168.0.58:80/80/api", url);
    }

    @Test
    void portDetectionShouldAddSlashAfterNumberAndBeforePathLetterIfBothAreInTheSameAppend(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:80pathhere") // <-- slash should be added between 80/pathhere
                .append("80") // other port part here, should then consider path, not part of the port.
                .append("api")
                .build();
        assertEquals("ftp://192.168.0.58:80/pathhere/80/api", url);
    }

    @Test
    void testingPortDetectionWithQueryString(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:80pathhere") // <-- slash should be added between 80/pathhere
                .append("80") // other port part here, should then consider path, not part of the port.
                .append("api?")
                .build();
        assertEquals("ftp://192.168.0.58:80/pathhere/80/api?", url);
    }

    @Test
    void testingPortDetectionWithQueryStringAnd2PairsAfter(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:80pathhere") // <-- slash should be added between 80/pathhere
                .append("80") // other port part here, should then consider path, not part of the port.
                .append("api")
                .append("?foo=")
                .append("bar")
                .append("&fizz=")
                .append("buzz")
                .build();
        assertEquals("ftp://192.168.0.58:80/pathhere/80/api?foo=bar&fizz=buzz", url);
    }

    @Test
    void testingPortDetectionWithQueryStringAnd2PairsAfterWithDuplicatedQueryStrings(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:80pathhere")
                .append("80")
                .append("api?")
                .append("?foo=")
                .append("=bar&")
                .append("&fizz=")
                .append("=buzz")
                .build();
        assertEquals("ftp://192.168.0.58:80/pathhere/80/api?foo=bar&fizz=buzz", url);
    }

    @Test
    void duplicatedEqualsAsAQueryValueMustRemainDuplicated(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:80pathhere")
                .append("80")
                .append("api?")
                .append("?text=The+comparison+Operator+is+==") //<-- Duplicate == at the end should remain
                .build();
        assertEquals("ftp://192.168.0.58:80/pathhere/80/api?text=The+comparison+Operator+is+==", url);
    }

    @Test
    void duplicatedInterrogationTagAsAQueryValueMustRemainDuplicated(){
        String url = new RawUriStringBuilder()
                .append("ftp")
                .append("192.168.0.58:80pathhere")
                .append("80")
                .append("api?")
                .append("text=as+value+it+should+be+possible+to+duplicate+this??") //<-- Duplicate == at the end should remain
                .build();
        assertEquals("ftp://192.168.0.58:80/pathhere/80/api?text=as+value+it+should+be+possible+to+duplicate+this??", url);
    }
}
