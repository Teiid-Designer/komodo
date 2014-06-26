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
 * This class represents a services as defined by as WSDL
 *
 *
 */
public interface IService extends IWsdlElement {

    /**
     * @return an array of ports defined by the service
     */
    IPort[] getPorts();

    IModel getModel();

    String getNamespaceURI();

}
