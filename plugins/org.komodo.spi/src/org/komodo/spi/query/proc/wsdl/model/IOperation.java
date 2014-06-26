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
 * This class represents an Operation as defined in the WSDL It does not contain any information about the messages that are used
 * by the operation as they are of no interest until it is time to actually create an MM model
 *
 *
 */
public interface IOperation extends IWsdlElement {

    /**
     * @return the binding that contains this operation
     */
    IBinding getBinding();

    /**
     * @return the name of the input message
     */
    IMessage getInputMessage();

    /**
     * @return the name of the output message
     */
    IMessage getOutputMessage();

    /**
     * @return the style of the operation
     */
    String getStyle();

    /**
     * @return an array of the names of possible faults
     */
    IFault[] getFaults();

    String getSOAPAction();

    boolean canModel();

    String[] getProblemMessages();
}
