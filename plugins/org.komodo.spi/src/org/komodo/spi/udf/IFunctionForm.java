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
package org.komodo.spi.udf;

import java.util.List;

/**
 *
 */
public interface IFunctionForm {


    /**
     * Get the function name
     * 
     * @return name
     */
    String getName();
    
    /**
     * Get the display name of the function
     * 
     * @return display name
     */
    String getDisplayString();

    /**
     * Get the function description
     *  
     * @return description
     */
    String getDescription();

    /**
     * Get the category this function belongs to
     * 
     * @return name of the owning category
     */
    String getCategory();

    /**
     * Get the arguments
     * 
     * @return list of the function's arguments
     */
    List<String> getArgNames();

}
