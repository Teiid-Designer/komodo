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
package org.komodo.spi.query.sql.symbol;

import org.komodo.spi.query.sql.LanguageVisitor;


/**
 *
 */
public interface IGroupSymbol<LV extends LanguageVisitor> extends ISymbol<LV>{


    /**
     * Set the name of the group symbol
     * 
     * @param newName
     */
    void setName(String newName);
    
    /**
     * @return
     */
    String getDefinition();
    
    /**
     * @param newName
     */
    void setDefinition(String newName);

    /**
     * @return
     */
    boolean isProcedure();
    
    /**
     * Get the metadata ID that this group symbol resolves to.  If
     * the group symbol has not been resolved yet, this will be null.
     * If the symbol has been resolved, this will never be null.
     * 
     * @return Metadata ID object
     */
    Object getMetadataID();
    
    /**
     * Set the metadata ID that this group symbol resolves to.  It cannot
     * be null.
     * 
     * @param meatdataID Metadata ID object
     * 
     * @throws IllegalArgumentException If metadataID is null
     */
    void setMetadataID(Object metadataID);

}
