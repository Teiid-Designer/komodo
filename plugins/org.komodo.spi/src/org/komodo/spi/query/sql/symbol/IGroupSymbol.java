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


/**
 *
 */
public interface IGroupSymbol<LV extends ILanguageVisitor> extends ISymbol<LV>{


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
