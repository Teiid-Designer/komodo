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
