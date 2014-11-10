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
package org.komodo.spi.xml;

/**
 *
 */
public interface MappingNode {
    
    /** The default build in type */
    String DEFAULT_BUILT_IN_TYPE = ""; //$NON-NLS-1$
    
    /** The default minimum bound of the cardinality of a node. */
    Integer DEFAULT_CARDINALITY_MINIMUM_BOUND = new Integer(1);
    
    /** The default maximum bound of the cardinality of a node. */
    Integer DEFAULT_CARDINALITY_MAXIMUM_BOUND = new Integer(1);
    
    /** The default value for recursion limit */
    Integer DEFAULT_RECURSION_LIMIT = new Integer(10);
    
    /** The default value for is nillable */
    Boolean DEFAULT_IS_NILLABLE = Boolean.FALSE;
    
    /** The default value for exception on recursion limit */
    Boolean DEFAULT_EXCEPTION_ON_RECURSION_LIMIT = Boolean.FALSE;
    
    /**
     * PRESERVE -No normalization is done, the value is not changed.
     * REPLACE - All occurrences of tab, line feed and carriage return are replaced with space
     * COLLAPSE - After the processing implied by replace, contiguous sequences of space are 
     * collapsed to a single space, and leading and trailing spaces are removed.
     */
    String NORMALIZE_TEXT_PRESERVE = "preserve"; //$NON-NLS-1$
    String NORMALIZE_TEXT_REPLACE  = "replace"; //$NON-NLS-1$
    String NORMALIZE_TEXT_COLLAPSE = "collapse"; //$NON-NLS-1$
    
    /** The default value for normalize text */
    String DEFAULT_NORMALIZE_TEXT = NORMALIZE_TEXT_PRESERVE; 

    /**
     * @param excludeFromDocument
     */
    void setExclude(boolean excludeFromDocument);

    /**
     * @return
     */
    MappingNode clone();

}
