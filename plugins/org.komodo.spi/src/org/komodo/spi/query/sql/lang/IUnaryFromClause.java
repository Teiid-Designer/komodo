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
package org.komodo.spi.query.sql.lang;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;


/**
 *
 */
public interface IUnaryFromClause<GS extends IGroupSymbol, LV extends ILanguageVisitor> extends IFromClause<LV> {

    /**
     * Set the group held by the clause
     * 
     * @param group Group to hold
     */
    void setGroup(GS group);
    
    /**
     * Get group held by clause
     * 
     * @return Group held by clause
     */
    GS getGroup();
}
