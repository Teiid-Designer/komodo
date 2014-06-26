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
package org.komodo.spi.query.proc.wsdl.model;

/**
 * This class represents a port as defined in a WSDL
 *
 *
 */
public interface IPort extends IWsdlElement {
	
	static final String HTTP = "HTTP"; //$NON-NLS-1$
    static final String SOAP11 = "SOAP11"; //$NON-NLS-1$
    static final String SOAP12 = "SOAP12"; //$NON-NLS-1$
	
	static final String HTTP_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/http/"; //$NON-NLS-1$
	static final String SOAP11_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/soap/"; //$NON-NLS-1$
	static final String SOAP12_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/soap12/"; //$NON-NLS-1$

    /**
     * @return a binding defined in this port
     */
    IBinding getBinding();

    /**
     * @return the service that defines this port
     */
    IService getService();
    
    /**
     * @param uri - the binding type (SOAP11, SOAP12 or HTTP). 
     */
    String getBindingType( );
    
    /**
     * @param uri - the binding namespace URI attribute of the <soap:address> element. 
     */
    String getBindingTypeURI();

    /**
     * @return the location attribute of the <soap:address> element. The endpoint URL for the port.
     */
    String getLocationURI();

    String getNamespaceURI();

}
