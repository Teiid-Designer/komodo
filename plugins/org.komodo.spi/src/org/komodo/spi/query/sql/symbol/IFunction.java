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
