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
package org.komodo.spi.query.proc.wsdl;

import org.komodo.spi.outcome.Outcome;

/**
 *
 */
public interface WsdlColumnInfo {

    public static final String DEFAULT_DATATYPE = "string"; //$NON-NLS-1$
    public static final String INTEGER_DATATYPE = "integer"; //$NON-NLS-1$
    public static final int DEFAULT_WIDTH = 10;

    /**
     * Get the column name for display in the UI. This removes any quotes for
     * aesthetic reasons. Use {@link #getSymbolName()} for retrieving the 
     * fully validated column name.
     * 
     * @return the column name sans quotes.
     */
    String getName();

    /**
     * Get the fully validated column name. This should be used in SQL string
     * generation.
     *
     * @return name the column name
     */
    String getSymbolName();

    /**
     * 
     * @return datatype the column datatype
     */
    String getDatatype();

    /**
     * 
     * @return name the column name
     */
    int getWidth();

    /**
     * 
     * @return defaultValue the column defaultValue
     */
    String getDefaultValue();

    /**
     * 
     * @return xmlPath the column xmlPath
     */
    String getRelativePath();

    /**
     * 
     * @return xmlPath the column xmlPath
     */
    String getFullXmlPath();

    /**
     * 
     * @return forOrdinality the column forOrdinality
     */
    boolean getOrdinality();

    String getNamespace();
    
    WsdlAttributeInfo[] getAttributeInfoArray();

    String getUniqueAttributeName(String proposedName);

    /**
     * 
     * @return outcome the <code>IOutcome</code> representing the validity of the data in this info object
     */
    Outcome getOutcome();

}
