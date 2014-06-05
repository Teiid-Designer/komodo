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

import java.util.HashMap;
import java.util.Map;

import org.komodo.spi.query.proc.wsdl.model.IOperation;

/**
 *
 */
public interface IWsdlProcedureInfo extends IWsdlConstants {
    
    String getDefaultProcedureName();

    Map<String, String> getNamespaceMap();

    ProcedureType getType();

    IWsdlColumnInfo[] getBodyColumnInfoList();

    IWsdlColumnInfo[] getHeaderColumnInfoList();

    String getProcedureName();
    
    HashMap<String, String> getReverseNSMap();

    /**
     * 
     * @return rootPath the root path xquery expression
     */
    String getRootPath();

    IOperation getOperation();

    String getUniqueBodyColumnName(String proposedName);

    String getUniqueHeaderColumnName(String proposedName);

    IWsdlWrapperInfo getWrapperProcedure();

}
