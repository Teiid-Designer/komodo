/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

package org.komodo.modeshape.teiid.sql.symbol;

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.QueryCommandImpl;
import org.komodo.modeshape.teiid.sql.lang.BaseSubqueryContainer;
import org.komodo.spi.query.sql.symbol.ScalarSubquery;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class ScalarSubqueryImpl extends ASTNode implements BaseExpression, BaseSubqueryContainer<QueryCommandImpl>, ScalarSubquery<SQLanguageVisitorImpl, QueryCommandImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public ScalarSubqueryImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    public void setType(Class<?> type) {
        DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(type);
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, dataTypeName.name());
    }

    @Override
    public QueryCommandImpl getCommand() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, QueryCommandImpl.class);
    }

    @Override
    public void setCommand(QueryCommandImpl command) {
        setChild(TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, command);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
        result = prime * result + ((this.getType() == null) ? 0 : this.getType().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        ScalarSubqueryImpl other = (ScalarSubqueryImpl)obj;
        if (this.getCommand() == null) {
            if (other.getCommand() != null)
                return false;
        } else if (!this.getCommand().equals(other.getCommand()))
            return false;
        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public ScalarSubqueryImpl clone() {
        ScalarSubqueryImpl clone = new ScalarSubqueryImpl(this.getTeiidParser(), this.getId());

        if (getType() != null)
            clone.setType(getType());
        if (getCommand() != null)
            clone.setCommand(getCommand().clone());

        return clone;
    }

}
