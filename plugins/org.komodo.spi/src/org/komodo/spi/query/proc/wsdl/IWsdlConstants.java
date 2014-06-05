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
package org.komodo.spi.query.proc.wsdl;

/**
 *
 */
public interface IWsdlConstants {

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
