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
package org.komodo.spi.query.proc;

import org.komodo.spi.outcome.IOutcome;

/**
 *
 */
public interface ITeiidXmlColumnInfo {

    public static final String DEFAULT_DATATYPE = "string"; //$NON-NLS-1$
    
    public static final String INTEGER_DATATYPE = "integer"; //$NON-NLS-1$
    
    public static final int DEFAULT_WIDTH = 10;

    /**
     * Get the fully validated column name. This should be used in SQL string
     * generation.
     * 
     * @return name the column name
     */
    String getSymbolName();

    /**
     * Get the column name for display in the UI. This removes any quotes for
     * aesthetic reasons. Use {@link #getSymbolName()} for retrieving the 
     * fully validated column name.
     * 
     * @return the column name sans quotes.
     */
    String getName();

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

    /**r
     * 
     * @return forOrdinality the column forOrdinality
     */
    boolean getOrdinality();

    /**
     * 
     * @return forOrdinality the column forOrdinality
     */
    boolean isPathOverridden();

    /**
     * 
     * @return outcome the <code>IOutcome</code> representing the validity of the data in this info object
     */
    IOutcome getOutcome();

}
