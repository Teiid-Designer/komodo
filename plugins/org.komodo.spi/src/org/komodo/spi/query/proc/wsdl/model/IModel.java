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

import java.util.Map;

/**
 * This class represents the model hierarchy as defined by a give WSDL
 *
 * @since 8.0
 */
public interface IModel {

    /**
     * @return an array of the services defined in the WSDL
     */
    IService[] getServices();

    Map getNamespaces();

    IService getService( String name );

    IPort getPort( String name );

    IOperation getOperation( String name );
    
    IOperation[] getModelableOperations(String portName);
    
    String[] getModelablePortNames();
}
