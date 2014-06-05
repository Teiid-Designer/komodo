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
package org.komodo.spi.query;

import java.util.List;

import org.komodo.spi.query.metadata.IQueryMetadataInterface;
import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;

/**
 *
 */
public interface IQueryResolver<C extends ICommand, GS extends IGroupSymbol, E extends IExpression> {

    /**
     * @param command
     * @param gSymbol
     * @param teiidCommandType
     * @param metadata
     * @throws Exception 
     */
    void resolveCommand(C command, GS gSymbol, int teiidCommandType, IQueryMetadataInterface metadata) throws Exception;

    /**
     * @param command
     * @param gSymbol
     * @param commandType
     * @param metadata
     * @param projectedSymbols
     */
    void postResolveCommand(C command, GS gSymbol, int teiidCommandType, IQueryMetadataInterface metadata,
                                               List<E> projectedSymbols);

}
