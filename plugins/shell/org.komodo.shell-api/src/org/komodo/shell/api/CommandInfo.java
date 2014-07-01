/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.api;

import java.util.List;

/**
 * CommandInfo - contains details about a command.
 * The command name and the List of Workspace Context types for which it is valid
 */
public class CommandInfo {
	
	private String name;
	private List<WorkspaceContext.Type> types;
	
	/**
	 * Constructor
	 * @param name the command name
	 * @param types workspace context types the command is valid for
	 */
	public CommandInfo(String name, List<WorkspaceContext.Type> types) {
		super();
		this.name = name;
		this.types = types;
	}
	
	/**
	 * Get the name
	 * @return the name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Set the name
	 * @param name the name
	 */
	public void setName(String name) {
		this.name = name;
	}
	
	/**
	 * Get the list of context types
	 * @return the List of context types
	 */
	public List<WorkspaceContext.Type> getContextTypes() {
		return types;
	}
	
	/**
	 * Set the list of context types
	 * @param contextTypes the List of context types
	 */
	public void setContextTypes(List<WorkspaceContext.Type> contextTypes) {
		this.types = contextTypes;
	}	
	
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        CommandInfo other = (CommandInfo)obj;
        
        if (this.name == null) {
            if (other.name != null) return false;
        } else if (!this.name.equals(other.name)) return false;
        
        return true;
    }

}
