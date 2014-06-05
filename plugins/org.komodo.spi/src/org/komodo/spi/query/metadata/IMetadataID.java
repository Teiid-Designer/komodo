/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.query.metadata;

import java.util.List;

/**
 *
 */
public interface IMetadataID<M extends IMetadataID> {
    
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
