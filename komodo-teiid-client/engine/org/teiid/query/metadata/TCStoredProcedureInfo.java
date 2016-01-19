/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

package org.teiid.query.metadata;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.komodo.spi.query.metadata.StoredProcedureInfo;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.teiid.query.mapping.relational.TCQueryNode;
import org.teiid.query.sql.lang.SPParameterImpl;

/**
* This class encapsulates everything needed to pass between runtime metadata
* and the QueryResolver via the facades
*/

public class TCStoredProcedureInfo implements Serializable, StoredProcedureInfo<SPParameterImpl, TCQueryNode> {

    /** Constant identifying an IN parameter */
    public static final int IN = SPParameter.ParameterInfo.IN.index();
    
    /** Constant identifying an OUT parameter */
    public static final int OUT = SPParameter.ParameterInfo.OUT.index();

    /** Constant identifying an INOUT parameter */
    public static final int INOUT = SPParameter.ParameterInfo.INOUT.index();

    /** Constant identifying a RETURN parameter */
    public static final int RETURN_VALUE = SPParameter.ParameterInfo.RETURN_VALUE.index();

    /** Constant identifying a RESULT SET parameter */
    public static final int RESULT_SET = SPParameter.ParameterInfo.RESULT_SET.index();

    private Object modelID;
    private Object procedureID;
    private List<SPParameterImpl> parameters = new ArrayList<SPParameterImpl>();
    private String callableName;
    private TCQueryNode query;
    private int updateCount = -1;

    public String getProcedureCallableName(){
        return this.callableName;
    }
    public void setProcedureCallableName(String callableName){
        this.callableName = callableName;
    }
    public Object getModelID(){
        return this.modelID;
    }
    public void setModelID(Object modelID){
        this.modelID = modelID;
    }
    public Object getProcedureID(){
        return this.procedureID;
    }
    public void setProcedureID(Object procedureID){
        this.procedureID = procedureID;
    }
    public List<SPParameterImpl> getParameters(){
        return this.parameters;
    }
    public void setParameters(List<SPParameterImpl> parameters){
        this.parameters = parameters;
    }
    
    public void addParameter(SPParameterImpl parameter){
        this.parameters.add(parameter);
    }
           
    public TCQueryNode getQueryPlan(){
        return this.query;
    }
    public void setQueryPlan(TCQueryNode queryNode){
        this.query = queryNode;
    }

	public boolean returnsResultSet() {
		for (SPParameterImpl parameter : parameters) {
			if (parameter.getParameterType() == SPParameter.ParameterInfo.RESULT_SET.index()) {
				return true;
			}
		}
		return false;
	}

	public boolean returnsResultParameter() {
		for (SPParameterImpl parameter : parameters) {
			if (parameter.getParameterType() == SPParameter.ParameterInfo.RETURN_VALUE.index()) {
				return true;
			}
		}
		return false;
	}
	
    public int getUpdateCount() {
		return updateCount;
	}

	public void setUpdateCount(int updateCount) {
		this.updateCount = updateCount;
	}

} 