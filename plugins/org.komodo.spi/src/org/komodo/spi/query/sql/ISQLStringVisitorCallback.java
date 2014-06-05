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
package org.komodo.spi.query.sql;

import org.komodo.spi.query.sql.lang.ICriteria;
import org.komodo.spi.query.sql.lang.ILanguageObject;


/**
 *
 */
public interface ISQLStringVisitorCallback {

    /**
     * @param obj
     */
    void visitNode(ILanguageObject obj);
    
    /**
     * @param obj
     */
    void append(Object obj);

    /**
     * @param level
     */
    void addTabs(int level);

    /**
     * Allows for the creation of having/where nodes
     * even though there isn't a direct language representation
     * 
     * @param keyWord
     * @param crit
     */
    void visitCriteria(String keyWord, ICriteria crit);

    /**
     * @param level
     */
    void beginClause(int level);

}
