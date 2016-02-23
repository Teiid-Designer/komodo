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
import java.util.Set;

/**
 *
 */
public interface QueryService {

    /**
     * Get the query parser
     * 
     * @return implementation of {@link QueryParser}
     */
    QueryParser getQueryParser();

    /**
     * Is the given word a reserved part of the SQL syntax
     * 
     * @param word
     * 
     * @return true if the word is reserved.
     */
    boolean isReservedWord(final String word);

    /**
     * Is the given word a reserved part of the Procedure SQL syntax
     * 
     * @param word
     * 
     * @return true if the word is reserved.
     */
    boolean isProcedureReservedWord(final String word);

    /**
     * Get the SQL reserved words
     * 
     * @return set of reserved words
     */
    Set<String> getReservedWords();

    /**
     * Get the SQL non-reserved words
     * 
     * @return set of non-reserved words
     */
    Set<String> getNonReservedWords();

    /**
     * Get the name of the JDCB type that conforms to the
     * given index number
     * 
     * @param jdbcType
     * 
     * @return type name
     */
    String getJDBCSQLTypeName(int jdbcType);

    /**
     * Get the symbol short name version of the
     * given name
     * 
     * @param name
     * 
     * @return short name of given name
     */
    String getSymbolShortName(String name);
    
    /**
     * Get the procedure service
     * 
     * @return instance of {@link ProcedureService}
     */
    ProcedureService getProcedureService();

}
