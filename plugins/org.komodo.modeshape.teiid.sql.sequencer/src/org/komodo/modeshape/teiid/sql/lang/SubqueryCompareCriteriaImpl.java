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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.ScalarSubqueryImpl;
import org.komodo.spi.query.sql.lang.SubqueryCompareCriteria;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;

/**
 *
 */
public class SubqueryCompareCriteriaImpl extends AbstractCompareCriteria implements BaseSubqueryContainer<QueryCommandImpl>, SubqueryCompareCriteria<SQLanguageVisitorImpl, QueryCommandImpl> {

    /**
     * Predicate quantifiers
     */
    public enum PredicateQuantifier {

        /** "Some" predicate quantifier (equivalent to "Any") */
        SOME,

        /** "Any" predicate quantifier (equivalent to "Some") */
        ANY,

        /** "All" predicate quantifier */
        ALL;

        /**
         * @return index of predicate
         */
        public int getQuantifier() {
            return ordinal() + 2;
        }

        /**
         * @param quantifier
         * @return {@link PredicateQuantifier} with the given quantifier index
         */
        public static PredicateQuantifier findQuantifier(int quantifier) {
            for (PredicateQuantifier pq : values()) {
                if (pq.getQuantifier() == quantifier)
                    return pq;
            }

            throw new IllegalStateException();
        }

        /**
         * @param name
         * @return PredicateQuantifier with given name
         */
        public static PredicateQuantifier findPredicateQuantifier(String name) {
            if (name == null)
                return null;

            name = name.toUpperCase();
            for (PredicateQuantifier pq : values()) {
                if (pq.name().equals(name))
                    return pq;
            }
            return null;
        }
    }

    public SubqueryCompareCriteriaImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public QueryCommandImpl getCommand() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, QueryCommandImpl.class);
    }

    @Override
    public void setCommand(QueryCommandImpl command) {
        setChild(TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, command);
    }

    @Override
    public BaseExpression getRightExpression() {
        ScalarSubqueryImpl scalarSubquery = getTeiidParser().createASTNode(ASTNodes.SCALAR_SUBQUERY);
        scalarSubquery.setCommand(getCommand());
        return scalarSubquery;
    }

    public PredicateQuantifier getPredicateQuantifier() {
        Object property = getProperty(TeiidSqlLexicon.SubqueryCompareCriteria.PREDICATE_QUANTIFIER_PROP_NAME);
        return property == null ? PredicateQuantifier.ALL : PredicateQuantifier.findPredicateQuantifier(property.toString());
    }

    public void setPredicateQuantifier(PredicateQuantifier any) {
        setProperty(TeiidSqlLexicon.SubqueryCompareCriteria.PREDICATE_QUANTIFIER_PROP_NAME, any.name());
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
        result = prime * result + ((this.getPredicateQuantifier() == null) ? 0 : this.getPredicateQuantifier().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SubqueryCompareCriteriaImpl other = (SubqueryCompareCriteriaImpl)obj;
        if (this.getCommand() == null) {
            if (other.getCommand() != null)
                return false;
        } else if (!this.getCommand().equals(other.getCommand()))
            return false;
        if (this.getPredicateQuantifier() != other.getPredicateQuantifier())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public SubqueryCompareCriteriaImpl clone() {
        SubqueryCompareCriteriaImpl clone = new SubqueryCompareCriteriaImpl(this.getTeiidParser(), this.getId());

        if (getCommand() != null)
            clone.setCommand(getCommand().clone());
        if (getPredicateQuantifier() != null)
            clone.setPredicateQuantifier(getPredicateQuantifier());
        clone.setOperator(CriteriaOperator.Operator.findOperator(getOperator()));
        if (getLeftExpression() != null)
            clone.setLeftExpression(getLeftExpression().clone());

        return clone;
    }

}
