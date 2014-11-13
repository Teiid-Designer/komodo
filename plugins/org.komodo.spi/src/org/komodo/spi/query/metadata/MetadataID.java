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

/**
 *
 */
public interface MetadataID<M extends MetadataID> {
    
    /**
     * Get ID value 
     * 
     * @return ID value
     */
    String getID();
    
    /** 
     * @return Returns the originalMetadataID.
     */
    Object getOriginalMetadataID();
    
    /**
     * Get elements - only valid for groups
     * 
     * @return List of TempMetadataID for groups, null for elements
     */
    List<M> getElements();
    
    /**
     * Get type - only valid for elements
     *
     * @return Type for elements, null for groups
     */
    Class<?> getType();
}
