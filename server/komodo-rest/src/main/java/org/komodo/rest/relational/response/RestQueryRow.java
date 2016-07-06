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

public class RestQueryRow implements KRestEntity {

    /**
     * Label for row
     */
    public static final String ROW_LABEL = "row";

    private List<String> values;

    /**
     * Constructor for use when deserializing
     */
    public RestQueryRow() {
        super();
    }

    public RestQueryRow(QSRow qsRow) {
        if (qsRow == null)
            this.values = Collections.emptyList();
        else {
            this.values = new ArrayList<>();
            for (Object value : qsRow.getValues()) {
                this.values.add(value.toString());
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

    public String[] getValues() {
        return values.toArray(new String[0]);
    }

    public void setValues(Object[] values) {
        if (values == null || values.length == 0)
            this.values = Collections.emptyList();

        this.values = new ArrayList<String>();
        for (Object value: values) {
            this.values.add(value.toString());
        }
    }
}
