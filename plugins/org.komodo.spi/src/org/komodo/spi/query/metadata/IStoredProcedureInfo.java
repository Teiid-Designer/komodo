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
package org.komodo.spi.query.metadata;

import java.util.List;

import org.komodo.spi.query.sql.lang.ISPParameter;

/**
* This class encapsulates everything needed to pass between runtime metadata
* and the QueryResolver via the facades
*/

public interface IStoredProcedureInfo<P extends ISPParameter, Q extends IQueryNode> {

    String getProcedureCallableName();
    
    void setProcedureCallableName(String callableName);
    
    Object getModelID();
    
    void setModelID(Object modelID);
    
    Object getProcedureID();
    
    void setProcedureID(Object procedureID);
    
    List<P> getParameters();
    
    void setParameters(List<P> parameters);
    
    void addParameter(P parameter);

    boolean returnsResultSet();

    boolean returnsResultParameter();
    
    int getUpdateCount();

    void setUpdateCount(int updateCount);

    Q getQueryPlan();

    void setQueryPlan(Q queryNode);
    
}