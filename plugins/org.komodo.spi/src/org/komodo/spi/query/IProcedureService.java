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
package org.komodo.spi.query;

import java.util.Properties;

import org.komodo.spi.query.proc.ITeiidMetadataFileInfo;
import org.komodo.spi.query.proc.ITeiidXmlFileInfo;
import org.komodo.spi.query.proc.wsdl.IWsdlRequestInfo;
import org.komodo.spi.query.proc.wsdl.IWsdlResponseInfo;
import org.komodo.spi.query.proc.wsdl.IWsdlWrapperInfo;

/**
 * Provides getters for sql syntax strings that differ between runtime client versions
 */
public interface IProcedureService {

    /**
     * Prepare an SQL statement for the given file-based metdata
     * 
     * @param metadataFileInfo
     * @param relationalModelName 
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(ITeiidMetadataFileInfo metadataFileInfo, String relationalModelName);

    /**
     * Prepare an SQL statement for the given xml-based metdata
     * 
     * @param xmlFileInfo
     * @param relationalModelName 
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(ITeiidXmlFileInfo xmlFileInfo, String relationalModelName);
    
    /**
     * Prepare an SQL statement for the given wsdl-based wrapper metadata
     *
     * @param wrapperInfo
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(IWsdlWrapperInfo wrapperInfo);
    
    /**
     * Prepare an SQL statement for the given wsdl-based wrapper procedure metadata
     * 
     * @param wrapperInfo 
     * @param properties
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(IWsdlWrapperInfo wrapperInfo, Properties properties);
    
    /**
     * Prepare an SQL statement for the given wsdl-based request metadata
     * 
     * @param requestInfo 
     * @param properties
     *  
     * @return SQL statement based on the given metadata information 
     */
    String getSQLStatement(IWsdlRequestInfo requestInfo, Properties properties);

    /**
     * Prepare an SQL statement for the given wsdl-based response metadata
     * 
     * @param responseInfo
     * @param properties
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(IWsdlResponseInfo responseInfo, Properties properties);

}
