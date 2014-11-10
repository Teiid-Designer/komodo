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
 * This class represents an Operation as defined in the WSDL It does not contain any information about the messages that are used
 * by the operation as they are of no interest until it is time to actually create an MM model
 *
 *
 */
public interface Operation extends WsdlElement {

    /**
     * @return the binding that contains this operation
     */
    Binding getBinding();

    /**
     * @return the name of the input message
     */
    Message getInputMessage();

    /**
     * @return the name of the output message
     */
    Message getOutputMessage();

    /**
     * @return the style of the operation
     */
    String getStyle();

    /**
     * @return an array of the names of possible faults
     */
    Fault[] getFaults();

    String getSOAPAction();

    boolean canModel();

    String[] getProblemMessages();
}
