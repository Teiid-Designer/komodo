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
public interface IDelete<C extends ICriteria, 
                                           G extends IGroupSymbol,
                                           E extends IExpression,
                                           LV extends ILanguageVisitor> extends IProcedureContainer<E, LV> {

    /**
     * Returns the group being deleted from
     * 
     * @return Group symbol
     */
    G getGroup();
    
    /**
     * Set the group for this Delete command
     * 
     * @param group Group to be associated with this command
     */
    void setGroup(G group);
    
    /**
     * Returns the criteria object for this command.
     * 
     * @return criteria
     */
    C getCriteria();
    
    /**
     * Set the criteria for this Delete command
     * 
     * @param criteria Criteria to be associated with this command
     */
    void setCriteria(C criteria);
    
}
