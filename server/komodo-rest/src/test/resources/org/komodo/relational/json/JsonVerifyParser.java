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
import org.komodo.spi.json.JsonConstants;

@SuppressWarnings({"nls", "javadoc"})
public class JsonVerifyParser implements JsonConstants {

    private static final Pattern NAME_PROP_PATTERN = Pattern.compile("\\\"[\\S_\\-:]+\\\"");

    private static final Pattern ID_PATTERN = Pattern.compile("\\\"[\\S_:\\-\\.]+\\\"\\,");

    // More generous that ID pattern as it allows unquoted integers, forward slash and escaped speech marks
    private static final Pattern PROPERTY_PATTERN = Pattern.compile("(\\\")?[\\S_:/\\[\\]\\-\\.\\*\\\"\\\\ ']+(\\\")?(\\,)?");

    private static final Pattern PATH_PATTERN = Pattern.compile("\\\"[a-zA-Z0-9_:/\\[\\]\\-\\.]+\\\"(\\,)?");

    private static final Pattern TYPE_PATTERN = Pattern.compile("\\\"[a-zA-Z0-9_]+\\\"(\\,)?");

    // Pattern where a word ends with \". With the speech mark escaped the word is not an ending phrase of a quoted clause
    private static final Pattern NON_END_SM_WORD_PATTERN = Pattern.compile("[\\S]+\\\\\"(\\,)?");

    // Pattern where a word ends with ". Describes an ending phrase of a quoted clause
    private static final Pattern END_SM_WORD_PATTERN = Pattern.compile("([\\S]+)?\\\"(\\,)?");

    private StringTokenizer tokens;

    private String currToken = EMPTY_STRING;

    private StringBuffer pastTokens = new StringBuffer();

    public static void main(String[] args) {
        JsonVerifyParser p = new JsonVerifyParser();
        String text = "\"CREATE VIRTUAL PROCEDURE getTweets(IN query varchar) RETURNS TABLE (created_on varchar(25), from_user varchar(25), to_user varchar(25), profile_image_url varchar(25), source varchar(25), text varchar(140))" + NEW_LINE +
        "AS" + NEW_LINE +
        "SELECT tweet.* FROM (EXEC twitter.invokeHTTP(action => 'GET', endpoint => QUERYSTRING('', query AS q))) AS w, XMLTABLE('results' PASSING JSONTOXML('myxml', w.result) COLUMNS created_on string PATH 'created_at',  from_user string PATH 'from_user',  to_user string PATH 'to_user',  profile_image_url string PATH 'profile_image_url',  source string PATH 'source',  text string PATH 'text') AS tweet;" + NEW_LINE +
        NEW_LINE +
        "CREATE VIEW Tweet" + NEW_LINE +
        "AS" + NEW_LINE +
        "SELECT * FROM twitterview.getTweets;" + NEW_LINE +
        " \",";
        StringTokenizer textTokens = new StringTokenizer(text);
        p.tokens = textTokens;
        p.nextToken();
        p.value(PROPERTY_PATTERN);
    }

    private void nextToken() {
        pastTokens.append(currToken);
        currToken = tokens.hasMoreTokens() ? tokens.nextToken() : EMPTY_STRING;
    }

    private void concatQuotedValue() {
        if (! currToken.startsWith(SPEECH_MARK))
            return;

        StringBuffer valueTokens = new StringBuffer();
        do {
            if (valueTokens.length() > 0)
                valueTokens.append(SPACE);

            valueTokens.append(currToken);

            Matcher matcher = END_SM_WORD_PATTERN.matcher(currToken);
            if (matcher.matches()) {
                matcher = NON_END_SM_WORD_PATTERN.matcher(currToken);
                if (! matcher.matches())
                    break;
            }

            nextToken();
        } while (tokens.hasMoreTokens());

        currToken = valueTokens.toString();
    }

    private void value(Pattern pattern) {
        concatQuotedValue();

        Matcher matcher = pattern.matcher(currToken);
        assertTrue("Failed to match " + currToken, matcher.matches());
        nextToken();

        //
        // Deals with possibility that property value is multi-worded
        //
//        do {
//            matcher = BARE_WORD_PATTERN.matcher(currToken);
//            if (matcher.matches()) {
//                nextToken();
//                continue;
//            }
//
//            matcher = END_WORD_PATTERN.matcher(currToken);
//            if (matcher.matches()) {
//                nextToken();
//                break;
//            }
//
//            // Neither a bare word or end word so get out of this loop
//            break;
//
//        } while (tokens.hasMoreTokens());
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

    private void propertiesValue() {
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
        else if (PROPERTIES.equals(name))
            propertiesValue();
        else if (currToken.equals(OPEN_BRACE)) {
            // child object
            jsonObject();
        } else
            value(PROPERTY_PATTERN);
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
