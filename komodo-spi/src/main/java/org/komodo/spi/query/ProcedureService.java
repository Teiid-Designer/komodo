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
package org.komodo.spi.query;

import java.util.Properties;

import org.komodo.spi.query.proc.TeiidMetadataFileInfo;
import org.komodo.spi.query.proc.TeiidXmlFileInfo;
import org.komodo.spi.query.proc.wsdl.WsdlRequestInfo;
import org.komodo.spi.query.proc.wsdl.WsdlResponseInfo;
import org.komodo.spi.query.proc.wsdl.WsdlWrapperInfo;

/**
 * Provides getters for sql syntax strings that differ between runtime client versions
 */
public interface ProcedureService {

    /**
     * Prepare an SQL statement for the given file-based metdata
     * 
     * @param metadataFileInfo
     * @param relationalModelName 
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(TeiidMetadataFileInfo metadataFileInfo, String relationalModelName);

    /**
     * Prepare an SQL statement for the given xml-based metdata
     * 
     * @param xmlFileInfo
     * @param relationalModelName 
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(TeiidXmlFileInfo xmlFileInfo, String relationalModelName);
    
    /**
     * Prepare an SQL statement for the given wsdl-based wrapper metadata
     *
     * @param wrapperInfo
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(WsdlWrapperInfo wrapperInfo);
    
    /**
     * Prepare an SQL statement for the given wsdl-based wrapper procedure metadata
     * 
     * @param wrapperInfo 
     * @param properties
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(WsdlWrapperInfo wrapperInfo, Properties properties);
    
    /**
     * Prepare an SQL statement for the given wsdl-based request metadata
     * 
     * @param requestInfo 
     * @param properties
     *  
     * @return SQL statement based on the given metadata information 
     */
    String getSQLStatement(WsdlRequestInfo requestInfo, Properties properties);

    /**
     * Prepare an SQL statement for the given wsdl-based response metadata
     * 
     * @param responseInfo
     * @param properties
     * 
     * @return SQL statement based on the given metadata information
     */
    String getSQLStatement(WsdlResponseInfo responseInfo, Properties properties);

}
