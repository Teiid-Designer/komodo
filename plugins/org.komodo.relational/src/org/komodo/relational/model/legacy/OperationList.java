/*
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
*/
package org.komodo.relational.model.legacy;

import java.util.List;

/**
 * List of Operations used in the DifferenceReport
 * 1) Create, 2)Delete, 3)Update
 */
public class OperationList {
	
	@SuppressWarnings("javadoc")
	public enum OperationType {
		CREATE,
		DELETE,
		UPDATE
	}
	private List<RelationalObject> refList;
	private OperationType operationType;
	
	/**
	 * OperationList constructor
	 * @param references the list of RelationalReference objects for this operation
	 * @param operationType the type of operation
	 */
	public OperationList(List<RelationalObject> references, OperationType operationType) {
		this.refList = references;
		this.operationType = operationType;
	}
	
	/**
	 * Get the list of RelationalReferences
	 * @return the list of RelationalReferences
	 */
	public List<RelationalObject> getList() {
		return this.refList;
	}
	
	/**
	 * Get the type of operation for this list
	 * @return the type of operation
	 */
	public OperationType getOperationType() {
		return this.operationType;
	}

}
