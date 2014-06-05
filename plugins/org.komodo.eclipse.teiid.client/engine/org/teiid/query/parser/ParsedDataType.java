/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
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
