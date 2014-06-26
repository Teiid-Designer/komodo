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
 * This class represents a Binding as defined in a WSDL
 *
 *
 */
public interface IBinding extends IWsdlElement {

    /**
     * @return returns the operations defined within the Binding
     */
    IOperation[] getOperations();

    /**
     * @return the port that contains this binding
     */
    IPort getPort();

    /**
     * @return uri the URI for the SOAP Binding
     */
    String getTransportURI();

    /**
     * This returns the style information returned by the SOAP binding (RPC or DOC)
     * 
     * @return the style for the SOAP web service
     */
    String getStyle();
}
