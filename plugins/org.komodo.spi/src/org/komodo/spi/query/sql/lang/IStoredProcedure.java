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
package org.komodo.spi.query.sql.lang;

import java.util.List;

import org.komodo.spi.query.sql.ILanguageVisitor;

/**
 *
 */
public interface IStoredProcedure<P extends ISPParameter, E extends IExpression, LV extends ILanguageVisitor> 
    extends IProcedureContainer<E, LV> {

    void setProcedureID(Object procedureID);
    
    Object getProcedureID();
    
    List<P> getInputParameters();

    void setParameter(P parameter);
    
    String getProcedureCallableName();

    void setProcedureName(String procFullName);

    void setDisplayNamedParameters(boolean b);

    String getGroupName();

}
