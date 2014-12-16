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
package org.komodo.shell;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import org.komodo.relational.model.legacy.RelationalObject;
import org.komodo.relational.model.legacy.RelationalConstants.TYPES;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * The WorkspaceContext
 */
public class WorkspaceContextImpl implements WorkspaceContext {
	
	private String name;
	private Type type;
	private WorkspaceStatus wsStatus = null;
	private WorkspaceContext parent = null;
	private List<WorkspaceContext> children = new ArrayList<WorkspaceContext>();
	private List<RelationalObject> models = new ArrayList<RelationalObject>();
	private Map<WorkspaceContext.Type,Integer> wsContextToRelObjTypeMap;
	
	/**
	 * Constructor
	 * @param wsStatus the workspace status object
	 * @param parent the parent context
	 * @param name the context name
	 * @param type the context type
	 */
	public WorkspaceContextImpl(WorkspaceStatus wsStatus, WorkspaceContext parent, String name, Type type) {
		super();
		this.wsStatus = wsStatus;
		this.parent = parent;
		this.name = name;
		this.type = type;
		
		// Init the context to relational type mappings
		this.wsContextToRelObjTypeMap = new HashMap<WorkspaceContext.Type,Integer>();
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.TABLE, TYPES.TABLE);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.COLUMN, TYPES.COLUMN);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.PROCEDURE, TYPES.PROCEDURE);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.PARAMETER, TYPES.PARAMETER);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.RESULT_SET, TYPES.RESULT_SET);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.SCHEMA, TYPES.SCHEMA);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.VIEW, TYPES.VIEW);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.UNIQUE_CONSTRAINT, TYPES.UC);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.ACCESS_PATTERN, TYPES.AP);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.PRIMARY_KEY, TYPES.PK);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.FOREIGN_KEY, TYPES.FK);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.INDEX, TYPES.INDEX);
		this.wsContextToRelObjTypeMap.put(WorkspaceContext.Type.MODEL, TYPES.MODEL);
	}
	
	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getName()
	 */
	@Override
	public String getName() {
		return this.name;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#setName(java.lang.String)
	 */
	@Override
	public void setName(String name) {
		this.name = name;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getType()
	 */
	@Override
	public Type getType() {
		return this.type;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#setType(org.komodo.shell.api.WorkspaceContext.Type)
	 */
	@Override
	public void setType(Type type) {
		this.type=type;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getParent()
	 */
	@Override
	public WorkspaceContext getParent() {
		return this.parent;
	}
	
	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getChildren()
	 */
	@Override
	public List<WorkspaceContext> getChildren() {
		return this.children;
	}

	private WorkspaceContext createWorkspaceContext(RelationalObject relObj) {
		WorkspaceContext.Type wsCtxType = getWsContextTypeForRelationalObjType(relObj.getType());
		return new WorkspaceContextImpl(this.wsStatus,this,relObj.getName(),wsCtxType);
	}
	
	/**
	 * Get the full name path for this context.  e.g. home.parentContext.thisContext
	 * @return the full name
	 */
	@Override
	public String getFullName() {
		List<WorkspaceContext> parentContexts = new ArrayList<WorkspaceContext>();
		WorkspaceContext parentContext = this.parent;
		while(parentContext!=null) {
			parentContexts.add(0,parentContext);
			parentContext = parentContext.getParent();
		}
		StringBuffer sb = new StringBuffer();
		for(WorkspaceContext theContext : parentContexts) {
			sb.append(theContext.getName()+"."); //$NON-NLS-1$
		}
		sb.append(getName());
		return sb.toString();
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#isRelational()
	 */
	@Override
	public boolean isRelational() {
		WorkspaceContext.Type wsContextType = getType();
		boolean isRelational = false;
		
		switch(wsContextType) {
		case TABLE:
		case COLUMN:
		case PROCEDURE:
		case PARAMETER:
		case RESULT_SET:
		case SCHEMA:
		case VIEW:
		case UNIQUE_CONSTRAINT:
		case ACCESS_PATTERN:
		case PRIMARY_KEY:
		case FOREIGN_KEY:
		case INDEX:
		case MODEL:
			isRelational=true;
			break;
		case ALL:
		case HOME:
		case PROJECT:
		default:
			break;
		}
		
		return isRelational;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getChild(java.lang.String, org.komodo.shell.api.WorkspaceContext.Type)
	 */
	@Override
	public WorkspaceContext getChild(String name, Type type) {
		WorkspaceContext result = null;
		for(Object child : children) {
			WorkspaceContext ctx = (WorkspaceContext)child;
			Type ctxType = ctx.getType();
			String ctxName = ctx.getName();
			if(ctxType==type && ctxName.equalsIgnoreCase(name)) {
				result = ctx;
				break;
			}
		}
		return result;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getRelationalObjTypeForWsContextType(org.komodo.shell.api.WorkspaceContext.Type)
	 */
	@Override
	public int getRelationalObjTypeForWsContextType(Type ctxType) {
		return this.wsContextToRelObjTypeMap.get(ctxType);
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getWsContextTypeForRelationalObjType(int)
	 */
	@Override
	public Type getWsContextTypeForRelationalObjType(int relObjType) {
		WorkspaceContext.Type ctxType = WorkspaceContext.Type.MODEL;
		for(WorkspaceContext.Type wsCtxType : this.wsContextToRelObjTypeMap.keySet()) {
			int objType = this.wsContextToRelObjTypeMap.get(wsCtxType);
			if(objType==relObjType) {
				ctxType = wsCtxType;
				break;
			}
		}
		return ctxType;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getPropertyNameValueMap()
	 */
	@Override
	public Map<String,String> getPropertyNameValueMap() {
		Map<String,String> propNameValues = new HashMap<String,String>();
		if(getType().equals(WorkspaceContext.Type.HOME)) {
			propNameValues.put(WorkspaceStatus.RECORDING_FILEPATH_KEY,getWorkspaceStatus().getRecordingOutputFile().toString());
		} else if(isRelational()) {
			RelationalObject relObj = getRelationalObj();
			Map<String,String> props = relObj.getProperties();
			for(Object propName : props.keySet()) {
				propNameValues.put(propName.toString(),props.get(propName.toString()));
			}
		}
		return propNameValues;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getValidTypesForCreate()
	 */
	@Override
	public List<String> getValidTypesForCreate() {
		List<String> types = new ArrayList<String>();
		if(getType().equals(WorkspaceContext.Type.HOME)) {
			types.add(WorkspaceContext.Type.PROJECT.toString());
		} else if(getType().equals(WorkspaceContext.Type.PROJECT)) {
			types.add(WorkspaceContext.Type.MODEL.toString());
		} else if(getType().equals(WorkspaceContext.Type.MODEL)) {
			types.add(WorkspaceContext.Type.TABLE.toString());
		} else if(getType().equals(WorkspaceContext.Type.TABLE)) {
			types.add(WorkspaceContext.Type.COLUMN.toString());
			types.add(WorkspaceContext.Type.ACCESS_PATTERN.toString());
			types.add(WorkspaceContext.Type.PRIMARY_KEY.toString());
			types.add(WorkspaceContext.Type.FOREIGN_KEY.toString());
			types.add(WorkspaceContext.Type.INDEX.toString());
		}
		return types;
	}
	
	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getRelationalObj()
	 */
	@Override
	public RelationalObject getRelationalObj() {
		WorkspaceContext.Type currentCtxType = getType();
		RelationalObject relObj = null;
		
		if(isRelational()) {
			String fullName = getFullName();
			String projName = getProjectName(fullName);
			String modelName = getModelName(fullName);
			
			WorkspaceContext homeContext = this;
			WorkspaceContext parentContext = getParent();
			while(parentContext!=null) {
				homeContext = parentContext;
				parentContext = homeContext.getParent();
			}
			WorkspaceContext projContext = homeContext.getChild(projName, WorkspaceContext.Type.PROJECT);
			RelationalObject modelObj = getModelObject(projContext,modelName);
			if(projContext!=null) {
				List<String> objPathElems = getModelObjPath(fullName);
				if(!objPathElems.isEmpty()) {
					int relObjType = homeContext.getRelationalObjTypeForWsContextType(currentCtxType);
					relObj = modelObj.getChildAtPath(objPathElems,relObjType);
				} else {
					relObj = modelObj;
				}
			}
		}
		
		return relObj;
	}
	
	private RelationalObject getModelObject(WorkspaceContext projContext,String modelName) {
		RelationalObject modelObject = null;
		if(projContext!=null && projContext.getType()==WorkspaceContext.Type.PROJECT) {
			for(RelationalObject modelObj : projContext.getModels()) {
				if(modelObj.getType()==TYPES.MODEL && modelObj.getName().equalsIgnoreCase(modelName)) {
					modelObject = modelObj;
					break;
				}
			}
		}
		return modelObject;
	}
	
	private String getProjectName(String fullCtx) {
		// Get list of ctx elements
        List<String> ctxList = getContextList(fullCtx);
        if(ctxList.size()>1) return ctxList.get(1);
		return null;
	}

	private String getModelName(String fullCtx) {
		// Get list of ctx elements
        List<String> ctxList = getContextList(fullCtx);
        if(ctxList.size()>2) return ctxList.get(2);
		return null;
	}

	private List<String> getModelObjPath(String fullCtx) {
		List<String> pathElems = new ArrayList<String>();
		
		// Get list of ctx elements
        List<String> ctxList = getContextList(fullCtx);
        if(ctxList.size()>3) {
            for(int i=3; i<ctxList.size(); i++) {
             	pathElems.add(ctxList.get(i));
            }
        }
        return pathElems;
	}
	
	private List<String> getContextList(String fullCtx) {
		// Break down the full path into a list
        List<String> ctxList = new ArrayList<String>();
        StringTokenizer tokenizer = new StringTokenizer( fullCtx, "." ); //$NON-NLS-1$
        while(tokenizer.hasMoreTokens()){
            String ctxElement = tokenizer.nextToken();
            ctxList.add(ctxElement);
        } 
        return ctxList;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getWorkspaceStatus()
	 */
	@Override
	public WorkspaceStatus getWorkspaceStatus() {
		return this.wsStatus;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#addChild(org.komodo.shell.api.WorkspaceContext)
	 */
	@Override
	public void addChild(Object child) {
		if(child instanceof WorkspaceContext) {
			this.children.add((WorkspaceContext)child);
		} else if(child instanceof RelationalObject) {
			WorkspaceContext wCtx = createWorkspaceContext((RelationalObject)child);
			this.children.add(wCtx);
			// Maintain Model list if this is a project
			if(this.getType()==WorkspaceContext.Type.PROJECT && ((RelationalObject)child).getType()==TYPES.MODEL) {
				this.models.add((RelationalObject)child);
			}
			addChildren(wCtx,((RelationalObject)child).getChildren());
		}
	}
	
	private void addChildren(WorkspaceContext wCtx, Collection<RelationalObject> children) {
		for(RelationalObject rObj : children) {
			wCtx.addChild(rObj); 
		}
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getModels()
	 */
	@Override
	public List<RelationalObject> getModels() {
		if(this.getType()==WorkspaceContext.Type.PROJECT) {
			return this.models;
		}
		return Collections.emptyList();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(this.getClass().getName());
		sb.append(" : name = ").append(getFullName()); //$NON-NLS-1$
		return sb.toString();
	}

}
