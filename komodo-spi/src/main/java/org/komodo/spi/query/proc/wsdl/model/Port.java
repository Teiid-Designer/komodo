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
package org.komodo.spi.query.proc.wsdl.model;

/**
 * This class represents a port as defined in a WSDL
 *
 *
 */
public interface Port extends WsdlElement {
	
	static final String HTTP = "HTTP"; //$NON-NLS-1$
    static final String SOAP11 = "SOAP11"; //$NON-NLS-1$
    static final String SOAP12 = "SOAP12"; //$NON-NLS-1$
	
	static final String HTTP_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/http/"; //$NON-NLS-1$
	static final String SOAP11_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/soap/"; //$NON-NLS-1$
	static final String SOAP12_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/soap12/"; //$NON-NLS-1$

    /**
     * @return a binding defined in this port
     */
    Binding getBinding();

    /**
     * @return the service that defines this port
     */
    Service getService();
    
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
