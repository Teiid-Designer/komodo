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
package org.komodo.rest.relational.json.datasource;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.rest.relational.datasource.DataSourceSchema;
import org.komodo.rest.relational.json.AbstractSerializerTest;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;

public class DSSSerializerTest extends AbstractSerializerTest {

    private static String q(String value) {
        return SPEECH_MARK + value + SPEECH_MARK;
    }

    private static String colon() {
        return COLON + SPACE;
    }

    private static String tab(int freq) {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < freq; ++i)
            buf.append(SPACE + SPACE);

        return buf.toString();
    }

    private static final String DATA_SOURCE_SCHEMA = EMPTY_STRING +
        OPEN_BRACE + NEW_LINE +
            tab(1) + q("keng__id") + colon() +  q("datasource") + COMMA + NEW_LINE +
            tab(1) + q("keng__kType") + colon() +  q("Datasource") + COMMA + NEW_LINE +
            tab(1) + q("keng__description") + colon() +  q("Describes the configuration for a datasource") + COMMA + NEW_LINE +
            tab(1) + q("keng__properties") + colon() +  OPEN_BRACE + NEW_LINE +
                tab(2) + q("tko__jndiName") + colon() +  OPEN_BRACE + NEW_LINE +
                    tab(3) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                    tab(3) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                    tab(3) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                tab(2) + CLOSE_BRACE + COMMA + NEW_LINE +
                tab(2) + q("tko__driverName") + colon() +  OPEN_BRACE + NEW_LINE +
                    tab(3) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                    tab(3) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                    tab(3) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                tab(2) + CLOSE_BRACE + COMMA + NEW_LINE +
                tab(2) + q("property") + colon() +  OPEN_BRACE + NEW_LINE +
                    tab(3) + q("keng__properties") + colon() +  OPEN_BRACE + NEW_LINE +
                        tab(4) + q("name") + colon() +  OPEN_BRACE + NEW_LINE +
                            tab(5) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                            tab(5) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                            tab(5) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                        tab(4) + CLOSE_BRACE + COMMA + NEW_LINE +
                        tab(4) + q("value") + colon() +  OPEN_BRACE + NEW_LINE +
                            tab(5) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                            tab(5) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                            tab(5) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                        tab(4) + CLOSE_BRACE + NEW_LINE +
                    tab(3) + CLOSE_BRACE + COMMA + NEW_LINE +
                    tab(3) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                    tab(3) + q("keng__required") + colon() +  false + COMMA + NEW_LINE +
                    tab(3) + q("keng__repeatable") + colon() +  true + COMMA + NEW_LINE +
                    tab(3) + q("keng__limit") + colon() + "-1" + NEW_LINE +
                tab(2) + CLOSE_BRACE + NEW_LINE +
            tab(1) + CLOSE_BRACE + NEW_LINE +
        CLOSE_BRACE;

    @Test
    public void testDataSourceSchema() {
        DataSourceSchema schema = new DataSourceSchema();
        String output = KomodoJsonMarshaller.marshall(schema, true);
        assertEquals(DATA_SOURCE_SCHEMA, output);
    }
}
