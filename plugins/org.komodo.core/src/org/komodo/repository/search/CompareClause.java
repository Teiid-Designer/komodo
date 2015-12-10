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

import org.komodo.core.KomodoLexicon.Search;
import org.komodo.core.KomodoLexicon.Search.WhereCompareClause;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Comparison clause
 *
 * tagged.[acme:tagName] = 'foo'
 *
 */
class CompareClause extends Clause implements PropertyClause {

    private ComparisonOperator compareOperator;
    private String value;


    /**
     * Constructor
     * @param logicalOperator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias
     * @param property the property
     * @param compareOperator the comparison operator
     * @param value the value for comparison
     */
    public CompareClause(LogicalOperator logicalOperator,
                                         String alias, String property, ComparisonOperator compareOperator,
                                         String value) {
        super(logicalOperator);

        ArgCheck.isNotNull(property);
        ArgCheck.isNotNull(compareOperator);
        ArgCheck.isNotNull(value);

        setAlias(alias);
        setProperty(PROPERTY, property);

        setCompareOperator(compareOperator);
        this.value = value;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected CompareClause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        super(uow, whereClause);

        if (whereClause.hasProperty(uow, Search.WhereCompareClause.PROPERTY)) {
            setProperty(whereClause.getProperty(uow, Search.WhereCompareClause.PROPERTY).getStringValue(uow));
        }

        if (whereClause.hasProperty(uow, WhereCompareClause.COMPARE_OPERATOR)) {
            String compareOpValue = whereClause.getProperty(uow, Search.WhereCompareClause.COMPARE_OPERATOR).getStringValue(uow);
            setCompareOperator(ComparisonOperator.findOperator(compareOpValue));
        }

        if (whereClause.hasProperty(uow, WhereCompareClause.VALUE)) {
            setValue(whereClause.getProperty(uow, Search.WhereCompareClause.VALUE).getStringValue(uow));
        }
    }

    /**
     * @return the property
     */
    @Override
    public String getProperty() {
        return properties.get(PROPERTY);
    }

    protected void setProperty(String propertyValue) {
        properties.put(PROPERTY, propertyValue);
    }

    /**
     * @return the compareOperator
     */
    public ComparisonOperator getCompareOperator() {
        return this.compareOperator;
    }

    protected void setCompareOperator(ComparisonOperator compareOperator) {
        this.compareOperator = compareOperator;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return this.value;
    }

    protected void setValue(String value) {
        this.value = value;
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

    @Override
    void write(UnitOfWork uow, KomodoObject searchObject) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(searchObject, "searchObject"); //$NON-NLS-1$

        Repository repository = searchObject.getRepository();
        KomodoObject whereObject = repository.add(uow, searchObject.getAbsolutePath(),
                                                  Search.WHERE_CLAUSE,
                                                  Search.WhereCompareClause.NODE_TYPE);

        writeProperties(uow, whereObject);

        whereObject.setProperty(uow, Search.WhereCompareClause.PROPERTY, getProperty());
        whereObject.setProperty(uow, Search.WhereCompareClause.COMPARE_OPERATOR, compareOperator.toString());
        whereObject.setProperty(uow, Search.WhereCompareClause.VALUE, value);
    }
}