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

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ISubqueryCompareCriteria;

public class SubqueryCompareCriteria extends AbstractCompareCriteria implements SubqueryContainer<QueryCommand>, ISubqueryCompareCriteria<LanguageVisitor, QueryCommand> {

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
    }

    public SubqueryCompareCriteria(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public QueryCommand getCommand() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setCommand(QueryCommand command) {
    }

    @Override
    public Expression getRightExpression() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    private PredicateQuantifier getPredicateQuantifier() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param any
     */
    public void setPredicateQuantifier(PredicateQuantifier any) {
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
        SubqueryCompareCriteria other = (SubqueryCompareCriteria)obj;
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
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public SubqueryCompareCriteria clone() {
        SubqueryCompareCriteria clone = new SubqueryCompareCriteria(this.getTeiidParser(), this.getId());

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
