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
package org.komodo.spi.query.sql.proc;

import org.komodo.spi.query.sql.LanguageVisitor;
import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;

/**
 * @param <B> 
 * @param <GS> 
 * @param <E> 
 * @param <LV> 
 *
 */
public interface ICreateProcedureCommand<B extends IBlock, GS extends IGroupSymbol, E extends IExpression, LV extends LanguageVisitor>
    extends ICommand<E, LV> {

    /**
     * Get the block on this command.
     * 
     * @return The <code>Block</code> on this command
     */
    B getBlock();
    
    /**
     * Set the block on this command.
     * 
     * @param block The <code>Block</code> on this command
     */
    void setBlock(B block);

    /**
     * @return virtual group on this command
     */
    GS getVirtualGroup();

    /**
     * Set the virtual group on this command
     *
     * @param view
     */
    void setVirtualGroup(GS view);

}
