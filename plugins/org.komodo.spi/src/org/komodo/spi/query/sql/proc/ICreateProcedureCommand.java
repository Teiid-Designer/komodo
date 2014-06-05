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
package org.komodo.spi.query.sql.proc;

import org.komodo.spi.query.sql.ILanguageVisitor;
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
public interface ICreateProcedureCommand<B extends IBlock, GS extends IGroupSymbol, E extends IExpression, LV extends ILanguageVisitor>
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
