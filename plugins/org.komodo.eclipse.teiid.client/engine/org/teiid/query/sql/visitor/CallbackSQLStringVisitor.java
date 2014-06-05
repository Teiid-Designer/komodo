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
package org.teiid.query.sql.visitor;

import org.komodo.spi.query.sql.ISQLStringVisitorCallback;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.teiid.query.sql.lang.Criteria;
import org.teiid.query.sql.lang.LanguageObject;

/**
 *
 */
public class CallbackSQLStringVisitor extends SQLStringVisitor {

    private final ISQLStringVisitorCallback callback;

    /**
     * @param teiidVersion
     * @param callback 
     */
    public CallbackSQLStringVisitor(ITeiidVersion teiidVersion, ISQLStringVisitorCallback callback) {
        super(teiidVersion);
        this.callback = callback;
    }

    @Override
    protected void visitNode(LanguageObject languageObject) {
        callback.visitNode(languageObject);
    }

    @Override
    protected void addTabs(int level) {
        callback.addTabs(level);
    }

    @Override
    protected void visitCriteria(String keyWord, Criteria criteria) {
        callback.visitCriteria(keyWord, criteria);
    }

    @Override
    protected void append(Object value) {
        callback.append(value);
    }

    @Override
    protected void beginClause(int level) {
        callback.beginClause(level);
    }
}
