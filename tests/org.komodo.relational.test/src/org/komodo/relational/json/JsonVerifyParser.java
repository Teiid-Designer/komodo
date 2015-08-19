/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.json;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.komodo.spi.constants.StringConstants;

@SuppressWarnings({"nls", "javadoc"})
public class JsonVerifyParser implements JsonConstants, StringConstants {

    private static final Pattern NAME_PROP_PATTERN = Pattern.compile("\\\"[a-zA-Z0-9_\\-:]+\\\"");

    private static final Pattern ID_PATTERN = Pattern.compile("\\\"[a-zA-Z0-9_:\\-\\.]+\\\"\\,");

    private static final Pattern NAME_PATTERN = ID_PATTERN;

    private static final Pattern PATH_PATTERN = Pattern.compile("\\\"[a-zA-Z0-9_:/\\[\\]\\-\\.]+\\\"(\\,)?");

    private static final Pattern TYPE_PATTERN = Pattern.compile("\\\"[a-zA-Z0-9_]+\\\"(\\,)?");

    private StringTokenizer tokens;

    private String currToken = EMPTY_STRING;

    private StringBuffer pastTokens = new StringBuffer();

    private void nextToken() {
        pastTokens.append(currToken);
        currToken = tokens.hasMoreTokens() ? tokens.nextToken() : EMPTY_STRING;
    }

    private void value(Pattern pattern) {
        Matcher matcher = pattern.matcher(currToken);
        assertTrue("Failed to match " + currToken, matcher.matches());
        nextToken();
    }

    private void parentValue() {
        if (NULL.equals(currToken)) {
            nextToken();
            return;
        }

        value(ID_PATTERN);
    }

    private void hasChildrenValue() {
        /*
         * Children come below has-children so only if true would a comma
         * follow this property.
         */
        if ("\"true\",".equals(currToken) || "\"false\"".equals(currToken)) {
            nextToken();
            return;
        }

        fail(HAS_CHILDREN + " property can only be \"true\" or \"false\" but was " + currToken);
    }

    private void linksValue() {
        assertEquals(OPEN_SQUARE_BRACKET, currToken);
        nextToken();

        while (tokens.hasMoreTokens()) {
            if ((CLOSE_SQUARE_BRACKET + COMMA).equals(currToken)) {
                nextToken();
                return;
            } else
                jsonObject();
        }
    }

    private String propertyName() {
        String nameToken = currToken;
        value(NAME_PROP_PATTERN);
        return nameToken.replaceAll("\\\"", EMPTY_STRING);
    }

    private void jsonProperty() {

        String name = propertyName();

        assertEquals(COLON, currToken);
        nextToken();

        if (ID.equals(name))
            value(ID_PATTERN);
        else if (NAME.equals(name))
            value(NAME_PATTERN);
        else if (PARENT.equals(name))
            parentValue();
        else if (DATA_PATH.equals(name))
            value(PATH_PATTERN);
        else if (TYPE.equals(name))
            value(TYPE_PATTERN);
        else if (HAS_CHILDREN.equals(name))
            hasChildrenValue();
        else if (LINKS.equals(name))
            linksValue();
        else if (REL.equals(name))
            value(ID_PATTERN);
        else if (HREF.equals(name))
            value(PATH_PATTERN);
        else {
            // child object
            jsonObject();
        }
    }

    private void jsonObject() {

        assertEquals(OPEN_BRACE, currToken);
        nextToken();

        while (tokens.hasMoreTokens()) {
            if (CLOSE_BRACE.equals(currToken)) {
                nextToken();
                return;
            }
            else if ((CLOSE_BRACE + COMMA).equals(currToken)) {
                nextToken();
                return;
            }
            else
                jsonProperty();
        }
    }

    /**
     * @param clause
     */
    public void verify(String clause) {
        try {
            tokens = new StringTokenizer(clause);
            nextToken();

            jsonObject();
        } catch (AssertionError err) {
            AssertionError ae = new AssertionError("Progress made: " + pastTokens.toString(), err);
            throw ae;
        }
    }

}
