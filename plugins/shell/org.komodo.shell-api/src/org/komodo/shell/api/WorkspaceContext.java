/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.api;

import java.util.ArrayList;
import java.util.List;

/**
 * The WorkspaceContext
 */
public class WorkspaceContext {
	
	/**
	 * The context type
	 */
	public enum Type {
		@SuppressWarnings("javadoc")
		ALL,
		@SuppressWarnings("javadoc")
		ROOT,
		@SuppressWarnings("javadoc")
		PROJECT,
		@SuppressWarnings("javadoc")
		SOURCE_MODEL,
		@SuppressWarnings("javadoc")
		VIEW_MODEL
	}
	
	private String name;
	private Type type;
	private WorkspaceContext parent = null;
	private List<WorkspaceContext> children = new ArrayList<WorkspaceContext>();
	
	/**
	 * Constructor
	 * @param parent the parent context
	 * @param name the context name
	 * @param type the context type
	 */
	public WorkspaceContext(WorkspaceContext parent, String name, Type type) {
		super();
		this.parent = parent;
		this.name = name;
		this.type = type;
	}
	
	/**
	 * Get the name
	 * @return the context name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Set the name
	 * @param name the context name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Get the type
	 * @return the type
	 */
	public Type getType() {
		return type;
	}

	/**
	 * Set the type
	 * @param type the type
	 */
	public void setType(Type type) {
		this.type = type;
	}

	/**
	 * Get the parent context
	 * @return the parent
	 */
	public WorkspaceContext getParent() {
		return this.parent;
	}
	
	/**
	 * Get all children
	 * @return the list of children
	 */
	public List<WorkspaceContext> getChildren() {
		return children;
	}

	/**
	 * Set the children for this context
	 * @param children the children
	 */
	public void setChildren(List<WorkspaceContext> children) {
		this.children = children;
	}
	
	/**
	 * Add a child to this context
	 * @param child the child
	 */
	public void addChild(WorkspaceContext child) {
		this.children.add(child);
	}
	
	/**
	 * Get the full name path for this context.  e.g. root.parentContext.thisContext
	 * @return the full name
	 */
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

}
