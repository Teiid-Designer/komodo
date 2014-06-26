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
 *
 */
public interface IMessage extends IWsdlElement {
	
	int REQUEST_TYPE = 0x00;
	int RESPONSE_TYPE = 0x02;
	int FAULT_TYPE = 0x04;
	
	IPart[] getParts();     
    
	IOperation getOperation();
	
	IFault getFault();
	
	boolean isRequest();
	
	boolean isResponse();
	
	boolean isFault();
	
	int getType();

	String getUse();
	
	String getNamespaceURI();
	
	String getEncodingStyle();
	
}
