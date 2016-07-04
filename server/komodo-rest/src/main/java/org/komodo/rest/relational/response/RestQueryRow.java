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
import java.util.Collections;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.query.QSRow;
import org.komodo.spi.query.QSRow.QSCell;

public class RestQueryRow implements KRestEntity {

    /**
     * Label for values
     */
    public static final String CELLS_LABEL = "values";

    private List<RestQueryCell> cells;

    /**
     * Constructor for use when deserializing
     */
    public RestQueryRow() {
        super();
        this.cells = Collections.emptyList();
    }

    public RestQueryRow(QSRow qsRow) {
        if (qsRow == null)
            this.cells = Collections.emptyList();
        else {
            this.cells = new ArrayList<>();
            for (QSCell cell : qsRow.getCells()) {
                this.cells.add(new RestQueryCell(cell));
            }
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

    public List<RestQueryCell> getCells() {
        return cells;
    }

    public void setCells(RestQueryCell[] cells) {
        if (cells == null || cells.length == 0)
            this.cells = Collections.emptyList();

        this.cells = new ArrayList<>();
        for (RestQueryCell cell : cells) {
            this.cells.add(cell);
        }
    }
}
