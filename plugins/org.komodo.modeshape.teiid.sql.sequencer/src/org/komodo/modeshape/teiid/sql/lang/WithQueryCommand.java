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

package org.komodo.modeshape.teiid.sql.lang;

import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.IWithQueryCommand;

public class WithQueryCommand extends ASTNode
    implements SubqueryContainer<QueryCommand>, IWithQueryCommand<LanguageVisitor, QueryCommand> {

    public WithQueryCommand(TeiidParser p, int id) {
        super(p, id);
    }

    public GroupSymbol getGroupSymbol() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.WithQueryCommand.GROUP_SYMBOL_REF_NAME, GroupSymbol.class);
    }

    public void setGroupSymbol(GroupSymbol groupSymbol) {
        addLastChild(TeiidSqlLexicon.WithQueryCommand.GROUP_SYMBOL_REF_NAME, groupSymbol);
    }

    public List<ElementSymbol> getColumns() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.WithQueryCommand.COLUMNS_REF_NAME, ElementSymbol.class);
    }

    public void setColumns(List<ElementSymbol> columns) {
        setChildren(TeiidSqlLexicon.WithQueryCommand.COLUMNS_REF_NAME, columns);
    }

    public QueryCommand getQueryExpression() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.WithQueryCommand.QUERY_EXPRESSION_REF_NAME, QueryCommand.class);
    }

    public void setQueryExpression(QueryCommand queryExpression) {
        addLastChild(TeiidSqlLexicon.WithQueryCommand.QUERY_EXPRESSION_REF_NAME, queryExpression);
    }

    @Override
    public QueryCommand getCommand() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, QueryCommand.class);
    }

    @Override
    public void setCommand(QueryCommand command) {
        addLastChild(TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, command);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getColumns() == null) ? 0 : this.getColumns().hashCode());
        result = prime * result + ((this.getGroupSymbol() == null) ? 0 : this.getGroupSymbol().hashCode());
        result = prime * result + ((this.getQueryExpression() == null) ? 0 : this.getQueryExpression().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        WithQueryCommand other = (WithQueryCommand)obj;
        if (this.getColumns() == null) {
            if (other.getColumns() != null) return false;
        } else if (!this.getColumns().equals(other.getColumns())) return false;
        if (this.getGroupSymbol() == null) {
            if (other.getGroupSymbol() != null) return false;
        } else if (!this.getGroupSymbol().equals(other.getGroupSymbol())) return false;
        if (this.getQueryExpression() == null) {
            if (other.getQueryExpression() != null) return false;
        } else if (!this.getQueryExpression().equals(other.getQueryExpression())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public WithQueryCommand clone() {
        WithQueryCommand clone = new WithQueryCommand(this.getTeiidParser(), this.getId());

        if(getColumns() != null)
            clone.setColumns(cloneList(getColumns()));
        if(getGroupSymbol() != null)
            clone.setGroupSymbol(getGroupSymbol().clone());
        if(getQueryExpression() != null)
            clone.setQueryExpression(getQueryExpression().clone());

        return clone;
    }

}
