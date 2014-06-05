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
