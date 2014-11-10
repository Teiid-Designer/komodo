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
package org.komodo.spi.query.proc;

import java.util.List;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.query.sql.ISQLConstants;

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
    Outcome getParsingStatus();

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
    Outcome parseXmlFile();

}
