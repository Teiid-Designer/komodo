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

import java.util.List;

import org.komodo.spi.query.sql.ILanguageVisitor;

/**
 *
 */
public interface IBlock<S extends IStatement, LV extends ILanguageVisitor> extends IStatement<LV> {

    /**
     * Get all the statements contained on this block.
     * 
     * @return A list of <code>Statement</code>s contained in this block
     */
    List<S> getStatements();

    /**
     * Add a <code>Statement</code> to this block.
     * 
     * @param statement The <code>Statement</code> to be added to the block
     */
    void addStatement(S statement);

}
