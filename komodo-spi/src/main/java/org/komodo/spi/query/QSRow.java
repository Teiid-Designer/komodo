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
package org.komodo.spi.query;

import java.util.ArrayList;
import java.util.List;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

public class QSRow {

    public static class QSCell {
        private final Object value;

        private final DataTypeName dataType;

        public QSCell(Object value, DataTypeName dataType) {
            this.value = value;
            this.dataType = dataType;
        }

        public Object getValue() {
            return value;
        }

        public DataTypeName getDataType() {
            return dataType;
        }
    }

    List<QSCell> cells = new ArrayList<QSCell>();

    /**
     * @param value
     * @param dataType
     */
    public void add(Object value, DataTypeName dataType) {
        cells.add(new QSCell(value, dataType));
    }

    /**
     * @return the column value for this row
     */
    public List<QSCell> getCells() {
        return cells;
    }
}
