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
package org.komodo.spi.query.metadata;

import java.util.List;

import org.komodo.spi.query.sql.lang.SPParameter;

/**
* This class encapsulates everything needed to pass between runtime metadata
* and the QueryResolver via the facades
*/

public interface StoredProcedureInfo<P extends SPParameter, Q extends QueryNode> {

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