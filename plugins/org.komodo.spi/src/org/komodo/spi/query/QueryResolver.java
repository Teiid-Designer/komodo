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
package org.komodo.spi.query;

import java.util.List;

import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;

/**
 *
 */
public interface QueryResolver<C extends ICommand, GS extends IGroupSymbol, E extends IExpression> {

    /**
     * @param command
     * @param gSymbol
     * @param teiidCommandType
     * @param metadata
     * @throws Exception 
     */
    void resolveCommand(C command, GS gSymbol, int teiidCommandType, QueryMetadataInterface metadata) throws Exception;

    /**
     * @param command
     * @param gSymbol
     * @param commandType
     * @param metadata
     * @param projectedSymbols
     */
    void postResolveCommand(C command, GS gSymbol, int teiidCommandType, QueryMetadataInterface metadata,
                                               List<E> projectedSymbols);

}
