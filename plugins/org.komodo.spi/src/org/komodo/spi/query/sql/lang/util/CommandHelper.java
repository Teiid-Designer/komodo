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
package org.komodo.spi.query.sql.lang.util;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.query.sql.lang.IExpression;

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
	public static List<IExpression> getProjectedSymbols(final ICommand command) {
        Set<IExpression> theSymbols = new LinkedHashSet<IExpression>();
        
        theSymbols.addAll(command.getProjectedSymbols());
        
        // Teiid ICommand.getResultSetColumns() may return NULL so need to check before adding to Set
        if( command.getResultSetColumns() != null ) {
        	theSymbols.addAll(command.getResultSetColumns());
        }
        
        List<IExpression> symbols = new ArrayList(theSymbols.size());
        symbols.addAll(theSymbols);
        
        return symbols;
	}

}
