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

import java.util.Properties;

/**
 *
 */
public interface IWsdlResponseInfo extends IWsdlProcedureInfo {
    
    String SOAPENVELOPE_ROOTPATH = "/soap:Envelope";//$NON-NLS-1$
    
    String SOAPHEADER_ROOTPATH = "/soap:Header";//$NON-NLS-1$
    
    String SOAPBODY_ROOTPATH = "/soap:Body";//$NON-NLS-1$
    
    String DEFAULT_NS = "ns";//$NON-NLS-1$

    @Override
	String getDefaultProcedureName();

    String getSqlStringTemplate();

    String getSqlString(Properties properties);

}
