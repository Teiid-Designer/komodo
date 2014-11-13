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
package org.komodo.spi.query.sql.lang.util;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.query.sql.lang.Expression;

/**
 * Utility class for helping condense or find SQL command info
 */
public class CommandHelper {

	/**
	 * Combines the command projected symbols and result set columns into projected symbols
	 * 
	 * @param command
	 * @return
	 */
	public static List<Expression> getProjectedSymbols(final Command command) {
        Set<Expression> theSymbols = new LinkedHashSet<Expression>();
        
        theSymbols.addAll(command.getProjectedSymbols());
        
        // Teiid ICommand.getResultSetColumns() may return NULL so need to check before adding to Set
        if( command.getResultSetColumns() != null ) {
        	theSymbols.addAll(command.getResultSetColumns());
        }
        
        List<Expression> symbols = new ArrayList(theSymbols.size());
        symbols.addAll(theSymbols);
        
        return symbols;
	}

}
