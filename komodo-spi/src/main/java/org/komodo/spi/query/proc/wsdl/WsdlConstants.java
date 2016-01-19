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

/**
 *
 */
public interface WsdlConstants {

    String KEY_REQUEST_PROCEDURE_NAME = "requestProcedureName"; //$NON-NLS-1$
    String KEY_RESPONSE_PROCEDURE_NAME = "responseProcedureName"; //$NON-NLS-1$
    String KEY_WRAPPER_PROCEDURE_NAME = "wrapperProcedureName"; //$NON-NLS-1$
    String SQL_BEGIN = "CREATE VIRTUAL PROCEDURE\nBEGIN\n"; //$NON-NLS-1$
    String SQL_END = "\nEND"; //$NON-NLS-1$
    String REQUEST = "REQUEST"; //$NON-NLS-1$
    String RESPONSE = "RESPONSE"; //$NON-NLS-1$
    String REQUEST_LOWER = "request"; //$NON-NLS-1$
    String RESPONSE_LOWER = "response"; //$NON-NLS-1$
    String TABLE_EXEC = "TABLE(EXEC "; //$NON-NLS-1$
    String XMI_EXTENSION = ".xmi"; //$NON-NLS-1$
    String RESULT_LOWER = "result"; //$NON-NLS-1$
    String INVOKE_SEGMENT_1 = "invoke('"; //$NON-NLS-1$
    String INVOKE_SEGMENT_2 = "', null, REQUEST.xml_out, null, TRUE))"; //$NON-NLS-1$
    String NULL_LOWER = "null"; //$NON-NLS-1$
    String XSI_NAMESPACE_PREFIX = "xsi"; //$NON-NLS-1$
    int TYPE_BODY = 0;
    int TYPE_HEADER = 1;
    Object XML_OUT = "xml_out"; //$NON-NLS-1$
    
    enum ProcedureType {
        REQUEST,
        RESPONSE,
        BOTH
    }

}
