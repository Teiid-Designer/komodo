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
package org.komodo.spi.query.sql.symbol;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.udf.IFunctionDescriptor;

/**
 *
 */
public interface IFunction<F extends IFunctionDescriptor, LV extends ILanguageVisitor>
    extends IExpression<LV> {

    /**
     * Get name of function
     * 
     * @return name
     */
    String getName();
    
    /**
     * Get function arguments
     * 
     * @return array of arguments
     */
    IExpression[] getArgs();

    /**
     * Get argument at given index
     * 
     * @param index
     * 
     * @return argument
     */
    IExpression getArg(int index);
    
    /**
     * Is function implicit
     * 
     * @return true if implicit
     */
    boolean isImplicit();

    /**
     * Get a descriptor for his function
     * 
     * @return descriptor
     */
    F getFunctionDescriptor();

    /**
     * Set the function descriptor
     * 
     * @param fd
     */
    void setFunctionDescriptor(F fd);

    /**
     * Set type of function
     * 
     * @param type New type
     */
    void setType(Class<?> type);

}
