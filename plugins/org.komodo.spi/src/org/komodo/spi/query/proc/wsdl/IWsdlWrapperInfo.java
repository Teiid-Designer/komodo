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
public interface IWsdlWrapperInfo extends IWsdlConstants {

    IWsdlRequestInfo getRequestInfo();

    IWsdlResponseInfo getResponseInfo();

    String getViewModelName();

    String getWrapperProcedureName();

    String getSoapAction();
    
    String getBindingType();
    
    String getSourceModelName();

    String getNamespaceURI();

    String getWrapperSqlString();
    
    String getWrapperProcedureSqlString(Properties properties);

}
