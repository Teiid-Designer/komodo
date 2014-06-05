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

import java.util.List;

import org.komodo.spi.query.sql.ISQLConstants;
import org.komodo.spi.state.IState;

/**
 *
 */
public interface ITeiidXmlFileInfo<T extends ITeiidXmlColumnInfo> extends ITeiidFileInfo, ISQLConstants {

    /**
     * Is the Xml file an url
     * 
     * @return whether file is an url
     */
    boolean isUrl();

    /**
     * @return Xml file url
     */
    String getXmlFileUrl();

    /**
     * 
     * @return rootPath the root path xquery expression
     */
    String getRootPath();

    /**
     * 
     * @return cachedFirstLines the <code>String[]</code> array from the data file
     */
    String[] getCachedFirstLines();

    /**
     * 
     * @return columnInfoList the <code>TeiidXmlColumnInfo[]</code> array parsed from the header in the data file
     */
    List<T> getColumnInfoList();
    
    /**
     * 
     * @return doProcess the boolean indicator that the user wishes to create view table from this object
     */
    boolean doProcess();

    /**
     * 
     * @return numberOfCachedLines the number of cached lines from data file
     */
    int getNumberOfCachedFileLines();

    /**
     * 
     * @return numberOfCachedLines the total number of lines from data file
     */
    int getNumberOfLinesInFile();

    /**
     * Returns the current generated SQL string based on an unknown relational model name
     * @return the generated SQL string
     */
    String getSqlStringTemplate();

    /**
     * Returns the current generated SQL string based on an unknown relational model name
     * 
     * @param relationalModelName 
     * 
     * @return the generated SQL string based on the values stored on this instance
     */
    String getSqlString(String relationalModelName);

    /**
     * Get the common root path
     * 
     * @return string value of the root path
     */
    String getCommonRootPath();

    /**
     * Get the parsing status
     * 
     * @return whether this XML is parsed successfully
     */
    IState getParsingStatus();

    /**
     * Get the Xml file's namespace
     * 
     * @return
     */
    String getNamespaceString();
    
    /**
     * Parse the Xml file
     * 
     * @return whether parsing was successful
     */
    IState parseXmlFile();

}
