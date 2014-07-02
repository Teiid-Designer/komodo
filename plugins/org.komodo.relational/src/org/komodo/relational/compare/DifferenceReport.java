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
package org.komodo.relational.compare;

import java.util.List;

import org.komodo.relational.model.RelationalObject;

/**
 * DifferenceReport keeps track of the differences between two RelationalModels
 */
public class DifferenceReport {
	
	private OperationList objectsToCreate;
	private OperationList objectsToDelete;
	private OperationList objectsToUpdate;
	
	/**
	 * Set the RelationalReference objects to Create
	 * @param objsToCreate the list of objects for create
	 */
	public void setObjectsToCreate(List<RelationalObject> objsToCreate) {
		this.objectsToCreate = new OperationList(objsToCreate,OperationList.OperationType.CREATE);
	}
	
	/**
	 * Set the RelationalReference objects to Delete
	 * @param objsToDelete the list of objects for delete
	 */
	public void setObjectsToDelete(List<RelationalObject> objsToDelete) {
		this.objectsToDelete = new OperationList(objsToDelete,OperationList.OperationType.DELETE);
	}
	
	/**
	 * Set the RelationalReference objects to Update
	 * @param objsToUpdate the list of objects for update
	 */
	public void setObjectsToUpdate(List<RelationalObject> objsToUpdate) {
		this.objectsToUpdate = new OperationList(objsToUpdate,OperationList.OperationType.UPDATE);
	}
	
	/**
	 * Get the RelationalReference objects for Create
	 * @return the list of objects for create
	 */
	public OperationList getObjectsToCreate() {
		return objectsToCreate;
	}
	
	/**
	 * Get the RelationalReference objects for Delete
	 * @return the list of objects for delete
	 */
	public OperationList getObjectsToDelete() {
		return objectsToDelete;
	}
	
	/**
	 * Get the RelationalReference objects for Update
	 * @return the list of objects for update
	 */
	public OperationList getObjectsToUpdate() {
		return objectsToUpdate;
	}
	
	/**
	 * Determine if the DifferenceReport has any operations
	 * @return 'true' if any operations to process, 'false' if not
	 */
	public boolean hasOperations() {
		if( !this.objectsToCreate.getList().isEmpty() || 
			!this.objectsToDelete.getList().isEmpty() ||
			!this.objectsToUpdate.getList().isEmpty()) {
			return true;
		}
		return false;
	}
	
	/**
	 * Determine if the Difference report has any selected operations
	 * @return 'true' if any operations are selected, 'false' if not
	 */
	public boolean hasSelectedOperations() {
		boolean hasSelectedOp = false;
		List<RelationalObject> createList = this.objectsToCreate.getList();
		for(RelationalObject createRef: createList) {
			if(createRef.isChecked()) {
				hasSelectedOp = true;
				break;
			}
		}
		if(!hasSelectedOp) {
			List<RelationalObject> deleteList = this.objectsToDelete.getList();
			for(RelationalObject deleteRef: deleteList) {
				if(deleteRef.isChecked()) {
					hasSelectedOp = true;
					break;
				}
			}
		}
		if(!hasSelectedOp) {
			List<RelationalObject> updateList = this.objectsToUpdate.getList();
			for(RelationalObject updateRef: updateList) {
				if(updateRef.isChecked()) {
					hasSelectedOp = true;
					break;
				}
			}
		}
		return hasSelectedOp;
	}

}
