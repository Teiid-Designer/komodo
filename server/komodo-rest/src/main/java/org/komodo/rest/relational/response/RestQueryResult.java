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
package org.komodo.rest.relational.response;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.query.QSColumn;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.query.QSRow;

public class RestQueryResult implements KRestEntity {

    /**
     * Label for columns
     */
    public static final String COLUMNS_LABEL = "columns";

    /**
     * Label for rows
     */
    public static final String ROWS_LABEL = "rows";

    private List<RestQueryColumn> columns = new ArrayList<>();

    private List<RestQueryRow> rows = new ArrayList<>();

    public RestQueryResult() {
        super();
    }

    public RestQueryResult(QSResult result) {
        if (result == null)
            return;

        for (QSColumn column : result.getColumns()) {
            columns.add(new RestQueryColumn(column));
        }

        for (QSRow row : result.getRows()) {
            rows.add(new RestQueryRow(row));
        }
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    public RestQueryRow[] getRows() {
        return rows.toArray(new RestQueryRow[0]);
    }

    public void setRows(RestQueryRow[] rows) {
        this.rows = Arrays.asList(rows);
    }

    public RestQueryColumn[] getColumns() {
        return columns.toArray(new RestQueryColumn[0]);
    }

    public void setColumns(RestQueryColumn[] columns) {
        if (columns == null || columns.length == 0)
            this.columns = Collections.emptyList();

        this.columns = new ArrayList<>();
        for (RestQueryColumn column : columns) {
            this.columns.add(column);
        }
    }
}
