/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.teiid.query.parser;

public class ParsedDataType {

    private String type;

    private Integer length;

    private Integer scale;

    private Integer precision;

    public ParsedDataType(String type) {
        this.type = type;
    }

    public ParsedDataType(String type, int length, boolean precision) {
        this.type = type;

        if (precision) {
            this.precision = length;
        } else {
            this.length = length;
        }
    }

    public ParsedDataType(String type, int length, int scale, boolean precision) {
        this.type = type;
        this.scale = scale;
        if (precision) {
            this.precision = length;
        } else {
            this.length = length;
        }
    }

    /**
     * @return the type
     */
    public String getType() {
        return this.type;
    }

    /**
     * @param type the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the length
     */
    public Integer getLength() {
        return this.length;
    }

    /**
     * @return the scale
     */
    public Integer getScale() {
        return this.scale;
    }

    /**
     * @return the precision
     */
    public Integer getPrecision() {
        return this.precision;
    }
}
