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
package org.komodo.rest.relational.json;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.relational.response.RestQueryColumn;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.spi.constants.StringConstants;

public class QueryResultSerializerTest implements StringConstants {

    private static String[][] columnsData = {
        { "Id", "ID", "long" },
        { "Name", "Name", "varchar" },
        { "Code", "Code", "varchar" }
    };

    private static Object[][] rowsData = {
        { 1, "Florida", "FL" },
        { 2, "Washington", "WA" },
        { 3, "Missouri", "MI" },
        { 4, "District of Columbia", "DC" },
        { 5, "Montana", "MO" }
    };

    private static int COLUMN_NAME = 0;

    private static int COLUMN_LABEL = 1;

    private static int COLUMN_TYPE = 2;

    private RestQueryResult queryResult;

    private String JSON;

    private String tab(String text, int num) {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < num; ++i)
            buf.append(SPACE).append(SPACE);

        buf.append(text);
        return buf.toString();
    }

    private String tab(String text) {
        return tab(text, 1);
    }

    @Before
    public void init() throws Exception {
        StringBuffer jsonBuf = new StringBuffer(OPEN_BRACE).append(NEW_LINE);
        jsonBuf.append(tab("\"columns\"")).append(COLON)
                        .append(SPACE).append(OPEN_SQUARE_BRACKET).append(NEW_LINE);

        List<RestQueryColumn> columns = new ArrayList<RestQueryColumn>();

        for (int i = 0; i < columnsData.length; ++i) {
            String[] columnData = columnsData[i];

            jsonBuf.append(tab(OPEN_BRACE, 2)).append(NEW_LINE)
                                .append(tab("\"name\"", 3))
                                    .append(COLON)
                                    .append(SPACE)
                                    .append(SPEECH_MARK)
                                    .append(columnsData[i][COLUMN_NAME])
                                    .append(SPEECH_MARK)
                                    .append(COMMA)
                                    .append(NEW_LINE)
                                .append(tab("\"label\"", 3))
                                    .append(COLON)
                                    .append(SPACE)
                                    .append(SPEECH_MARK)
                                    .append(columnsData[i][COLUMN_LABEL])
                                    .append(SPEECH_MARK)
                                    .append(COMMA)
                                    .append(NEW_LINE)
                                .append(tab("\"type\"", 3))
                                    .append(COLON)
                                    .append(SPACE)
                                    .append(SPEECH_MARK)
                                    .append(columnsData[i][COLUMN_TYPE])
                                    .append(SPEECH_MARK)
                                    .append(NEW_LINE)
                            .append(tab(CLOSE_BRACE, 2));

            if ((i + 1) < columnData.length)
                jsonBuf.append(COMMA);

            jsonBuf.append(NEW_LINE);

            RestQueryColumn column = new RestQueryColumn();
            column.setName(columnData[COLUMN_NAME]);
            column.setLabel(columnData[COLUMN_LABEL]);
            column.setType(columnData[COLUMN_TYPE]);

            columns.add(column);
        }

        jsonBuf.append(tab(CLOSE_SQUARE_BRACKET)).append(COMMA).append(NEW_LINE);

        jsonBuf.append(tab("\"rows\""))
                        .append(COLON)
                        .append(SPACE)
                        .append(OPEN_SQUARE_BRACKET)
                        .append(NEW_LINE);

        List<RestQueryRow> rows = new ArrayList<RestQueryRow>();
        for (int i = 0; i < rowsData.length; ++i) {
            Object[] rowData = rowsData[i];

            jsonBuf
                        .append(tab(OPEN_BRACE, 2))
                        .append(NEW_LINE)
                            .append(tab("\"row\"", 3))
                            .append(COLON)
                            .append(SPACE)
                            .append(OPEN_SQUARE_BRACKET)
                            .append(NEW_LINE)
                            .append(tab(SPEECH_MARK, 4)).append(rowData[COLUMN_NAME]).append(SPEECH_MARK)
                            .append(COMMA)
                            .append(NEW_LINE)
                            .append(tab(SPEECH_MARK, 4)).append(rowData[COLUMN_LABEL]).append(SPEECH_MARK)
                            .append(COMMA)
                            .append(NEW_LINE)
                            .append(tab(SPEECH_MARK, 4)).append(rowData[COLUMN_TYPE]).append(SPEECH_MARK)
                            .append(NEW_LINE)
                        .append(tab(CLOSE_SQUARE_BRACKET, 3))
                        .append(NEW_LINE)
                    .append(tab(CLOSE_BRACE, 2));
                    
            if ((i + 1) < rowsData.length)
                jsonBuf.append(COMMA);

            jsonBuf.append(NEW_LINE);

            RestQueryRow row = new RestQueryRow();
            row.setValues(rowData);

            rows.add(row);
        }

        jsonBuf.append(tab(CLOSE_SQUARE_BRACKET))
                        .append(NEW_LINE)
                        .append(CLOSE_BRACE);

        JSON = jsonBuf.toString();

        queryResult = new RestQueryResult();
        queryResult.setColumns(columns.toArray(new RestQueryColumn[0]));
        queryResult.setRows(rows.toArray(new RestQueryRow[0]));
    }

    @Test
    public void shouldExportResult() throws Exception {
        String json = KomodoJsonMarshaller.marshall( this.queryResult );
        assertEquals(JSON, json);
    }

    @Test
    public void shouldImportResult() throws Exception {
        RestQueryResult queryResult = KomodoJsonMarshaller.unmarshall( JSON, RestQueryResult.class );
        assertEquals(columnsData.length, queryResult.getColumns().length);
        assertEquals(rowsData.length, queryResult.getRows().length);
    }
}
