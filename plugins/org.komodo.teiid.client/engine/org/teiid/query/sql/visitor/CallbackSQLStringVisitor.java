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
package org.teiid.query.sql.visitor;

import org.komodo.spi.query.sql.SQLStringVisitorCallback;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;

/**
 *
 */
public class CallbackSQLStringVisitor extends SQLStringVisitorImpl {

    private final SQLStringVisitorCallback callback;

    /**
     * @param teiidVersion
     * @param callback 
     */
    public CallbackSQLStringVisitor(TeiidVersion teiidVersion, SQLStringVisitorCallback callback) {
        super(teiidVersion);
        this.callback = callback;
    }

    @Override
    protected void visitNode(BaseLanguageObject languageObject) {
        callback.visitNode(languageObject);
    }

    @Override
    protected void addTabs(int level) {
        callback.addTabs(level);
    }

    @Override
    protected void visitCriteria(String keyWord, CriteriaImpl criteria) {
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
