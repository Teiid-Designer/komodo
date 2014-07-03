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

import java.util.HashSet;
import java.util.Set;

import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.api.WorkspaceStatusEventHandler;


/**
 * Test implementation of WorkspaceStatus
 */
public class TestWorkspaceStatus implements WorkspaceStatus {

	private WorkspaceContext rootContext;
	private WorkspaceContext currentContext;
	private Set<WorkspaceStatusEventHandler> eventHandlers = new HashSet<WorkspaceStatusEventHandler>();
	
	/**
	 * Constructor
	 */
	public TestWorkspaceStatus() {
		initSample();
	}
	
	private void initSample() {
		rootContext = new WorkspaceContext(null,"root",WorkspaceContext.Type.ROOT); //$NON-NLS-1$
		
		WorkspaceContext projContext1 = new WorkspaceContext(rootContext,"Project1",WorkspaceContext.Type.PROJECT); //$NON-NLS-1$
		projContext1.addChild(new WorkspaceContext(projContext1,"SrcModel1",WorkspaceContext.Type.SOURCE_MODEL)); //$NON-NLS-1$
		projContext1.addChild(new WorkspaceContext(projContext1,"SrcModel2",WorkspaceContext.Type.SOURCE_MODEL)); //$NON-NLS-1$
		projContext1.addChild(new WorkspaceContext(projContext1,"ViewModel1",WorkspaceContext.Type.VIEW_MODEL)); //$NON-NLS-1$

		WorkspaceContext projContext2 = new WorkspaceContext(rootContext,"Project2",WorkspaceContext.Type.PROJECT); //$NON-NLS-1$
		projContext2.addChild(new WorkspaceContext(projContext2,"SrcModel1",WorkspaceContext.Type.SOURCE_MODEL)); //$NON-NLS-1$
		projContext2.addChild(new WorkspaceContext(projContext2,"ViewModel1",WorkspaceContext.Type.VIEW_MODEL)); //$NON-NLS-1$
		
		rootContext.addChild(projContext1);
		rootContext.addChild(projContext2);
		
		currentContext = rootContext;
	}
	
	@Override
	public WorkspaceContext getRootContext() {
		return rootContext;
	}
	
	@Override
	public void setCurrentContext(WorkspaceContext context) {
		currentContext = context;
		fireContextChangeEvent();
	}
	
	@Override
	public WorkspaceContext getCurrentContext() {
		return currentContext;
	}
	
	/**
	 * @see org.komodo.shell.api.WorkspaceStatus#addHandler(org.komodo.shell.api.WorkspaceStatusEventHandler)
	 */
	@Override
	public void addHandler(WorkspaceStatusEventHandler handler) {
		this.eventHandlers.add(handler);
	}

	/**
	 * @see org.komodo.shell.api.WorkspaceStatus#removeHandler(org.komodo.shell.api.WorkspaceStatusEventHandler)
	 */
	@Override
	public void removeHandler(WorkspaceStatusEventHandler handler) {
		this.eventHandlers.remove(handler);
	}
	
	/**
	 * Fires the context change event.
	 */
	private void fireContextChangeEvent( ) {
		for (WorkspaceStatusEventHandler handler : eventHandlers) {
			handler.workspaceContextChanged();
		}
	}

}
