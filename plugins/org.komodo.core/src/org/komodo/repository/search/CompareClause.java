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
package org.komodo.repository.search;

import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Comparison clause
 *
 * tagged.[acme:tagName] = 'foo'
 *
 */
class CompareClause extends Clause implements PropertyClause {

    private final ComparisonOperator compareOperator;
    private final String value;


    /**
     * Constructor
     * @param parent parent searcher
     * @param logicalOperator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias
     * @param property the property
     * @param compareOperator the comparison operator
     * @param value the value for comparison
     */
    public CompareClause(ObjectSearcher parent, LogicalOperator logicalOperator,
                                         String alias, String property, ComparisonOperator compareOperator,
                                         String value) {
        super(parent, logicalOperator);

        ArgCheck.isNotNull(property);
        ArgCheck.isNotNull(compareOperator);
        ArgCheck.isNotNull(value);

        setAlias(alias);
        setProperty(PROPERTY, property);

        this.compareOperator = compareOperator;
        this.value = value;
    }

    /**
     * @return the property
     */
    @Override
    public String getProperty() {
        return properties.get(PROPERTY);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.compareOperator == null) ? 0 : this.compareOperator.hashCode());
        result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
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
        CompareClause other = (CompareClause)obj;
        if (this.compareOperator != other.compareOperator)
            return false;
        if (this.value == null) {
            if (other.value != null)
                return false;
        } else
            if (!this.value.equals(other.value))
                return false;
        return true;
    }

    @Override
    public String clauseString(int index) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(index, buffer);

        setAlias(checkWhereAlias(getAlias()));

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias());
            buffer.append(DOT);
        }

        buffer.append(OPEN_SQUARE_BRACKET);
        buffer.append(getProperty());
        buffer.append(CLOSE_SQUARE_BRACKET);

        buffer.append(SPACE);
        buffer.append(compareOperator);
        buffer.append(SPACE);

        buffer.append(QUOTE_MARK);
        buffer.append(value);
        buffer.append(QUOTE_MARK);

        return buffer.toString();
    }
}