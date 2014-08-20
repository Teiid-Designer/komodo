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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.spi.query.sql.lang.IExistsCriteria;

public class ExistsCriteria extends Criteria implements PredicateCriteria, SubqueryContainer<QueryCommand>, IExistsCriteria<LanguageVisitor, QueryCommand> {

    public ExistsCriteria(TeiidParser p, int id) {
        super(p, id);

        SubqueryHint subqueryHint = getTeiidParser().createASTNode(ASTNodes.SUBQUERY_HINT);
        setSubqueryHint(subqueryHint);
    }

    @Override
    public boolean isNegated() {
        Object property = getProperty(TeiidSqlLexicon.ExistsCriteria.NEGATED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    @Override
    public void setNegated(boolean value) {
        setProperty(TeiidSqlLexicon.ExistsCriteria.NEGATED_PROP_NAME, value);
    }

    @Override
    public QueryCommand getCommand() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, QueryCommand.class);
    }

    @Override
    public void setCommand(QueryCommand command) {
        addLastChild(TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, command);
    }

    public SubqueryHint getSubqueryHint() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.ExistsCriteria.SUBQUERY_HINT_REF_NAME, SubqueryHint.class);
    }

    /**
     * @param hint
     */
    public void setSubqueryHint(SubqueryHint hint) {
        addLastChild(TeiidSqlLexicon.ExistsCriteria.SUBQUERY_HINT_REF_NAME, hint);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
        result = prime * result + (this.isNegated() ? 1231 : 1237);
        result = prime * result + ((this.getSubqueryHint() == null) ? 0 : this.getSubqueryHint().hashCode());
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
        ExistsCriteria other = (ExistsCriteria)obj;
        if (this.getCommand() == null) {
            if (other.getCommand() != null)
                return false;
        } else if (!this.getCommand().equals(other.getCommand()))
            return false;
        if (this.isNegated() != other.isNegated())
            return false;
        if (this.getSubqueryHint() == null) {
            if (other.getSubqueryHint() != null)
                return false;
        } else if (!this.getSubqueryHint().equals(other.getSubqueryHint()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ExistsCriteria clone() {
        ExistsCriteria clone = new ExistsCriteria(this.getTeiidParser(), this.getId());

        if (getCommand() != null)
            clone.setCommand(getCommand().clone());
        clone.setNegated(isNegated());
        if (getSubqueryHint() != null)
            clone.setSubqueryHint(getSubqueryHint().clone());

        return clone;
    }

}
