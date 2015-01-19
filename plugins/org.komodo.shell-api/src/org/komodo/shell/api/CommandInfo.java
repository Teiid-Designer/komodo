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
package org.komodo.shell.api;

import java.util.List;

/**
 * CommandInfo - contains details about a command.
 * The command name and the List of Workspace Context types for which it is valid
 */
public class CommandInfo {
	
	private String name;
	private List<String> types;
	
	/**
	 * Constructor
	 * @param name the command name
	 * @param types workspace context types the command is valid for
	 */
	public CommandInfo(String name, List<String> types) {
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
	public List<String> getContextTypes() {
		return types;
	}
	
	/**
	 * Set the list of context types
	 * @param contextTypes the List of context types
	 */
	public void setContextTypes(List<String> contextTypes) {
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
