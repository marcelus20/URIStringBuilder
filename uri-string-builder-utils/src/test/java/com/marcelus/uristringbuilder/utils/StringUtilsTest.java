package com.marcelus.uristringbuilder.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class StringUtilsTest {

    @Test
    void testingReplaceBlankSpaceWithEmptyStrings(){
        // Given
        final String string = "Believe or not.";

        final String result = StringUtils.replaceBlankSpaceWithEmptyStrings(string);

        assertEquals("Believeornot.", result);
    }

    @Test
    void testingReplaceBlankSpaceWithEmptyStringsUsingStringThatDoesntContainEmptySpace(){
        // Given
        final String string = "Believeornot.";

        final String result = StringUtils.replaceBlankSpaceWithEmptyStrings(string);

        assertEquals("Believeornot.", result);
    }

    @Test
    void testingReplaceBlankSpaceWithEmptyStringsUsingANullArgument(){

        final String result = StringUtils.replaceBlankSpaceWithEmptyStrings(null);

        assertEquals("", result);
    }

    @Test
    void testingReplaceBlankSpaceWithPlusIson(){
        // Given
        final String string = "Believe or not.";

        final String result = StringUtils.replaceBlankSpaceWithPlusIcon(string);

        assertEquals("Believe+or+not.", result);
    }

    @Test
    void testingReplaceBlankSpaceWithPlusIconUsingStringThatDoesntContainEmptySpace(){
        // Given
        final String string = "Believeornot.";

        final String result = StringUtils.replaceBlankSpaceWithPlusIcon(string);

        assertEquals("Believeornot.", result);
    }

    @Test
    void testingReplaceBlankSpaceWithPlusIconUsingANullArgument(){

        final String result = StringUtils.replaceBlankSpaceWithPlusIcon(null);

        assertEquals("", result);
    }

}