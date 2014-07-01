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

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.IExpression;


/**
 *
 */
public interface IElementSymbol<G extends IGroupSymbol, LV extends ILanguageVisitor>
    extends ISymbol<LV>, IExpression<LV> {

    public enum DisplayMode {

        // symbol name
        FULLY_QUALIFIED,

        // default
        OUTPUT_NAME,

        // short name
        SHORT_OUTPUT_NAME
    }
    
    /**
     * @return
     */
    G getGroupSymbol();

    /**
     * @param groupSymbol
     */
    void setGroupSymbol(G groupSymbol);

    /**
     * @return
     */
    boolean isExternalReference();
    
    /**
     * @param value
     */
    void setDisplayFullyQualified(boolean value);
    
    /**
     * 
     * @return
     */
    DisplayMode getDisplayMode();
    
    /**
     * @param outputName
     */
    void setDisplayMode(DisplayMode outputName);
    
    /**
     * @param targetType
     */
    void setType(Class<Object> targetType);
    
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
     * @param metadataID Metadata ID object
     * 
     */
    void setMetadataID(Object metadataID);

}
