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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.komodo.core.KomodoLexicon.Search;
import org.komodo.core.KomodoLexicon.Search.WhereClause;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * Abstract Clause implementation
 */
public abstract class Clause implements StringConstants {

    /**
     * Alias property
     */
    protected static final String ALIAS = "alias"; //$NON-NLS-1$

    private ObjectSearcher parent;

    private LogicalOperator preClauseOperator;

    protected final Map<String, String> properties = new HashMap<String, String>();

    static Clause createClause(UnitOfWork uow, KomodoObject whereClauseObject) throws KException {
        String primaryType = whereClauseObject.getPrimaryType(uow).getName();

        Clause clause = null;
        if (Search.WhereCompareClause.NODE_TYPE.equals(primaryType)) {
            clause = new CompareClause(uow, whereClauseObject);
        } else if (Search.WhereContainsClause.NODE_TYPE.equals(primaryType)) {
            clause = new ContainsClause(uow, whereClauseObject);
        } else if (Search.WhereSetClause.NODE_TYPE.equals(primaryType)) {
            clause = new SetClause(uow, whereClauseObject);
        } else if (Search.WherePathClause.NODE_TYPE.equals(primaryType)) {
            clause = new PathClause(uow, whereClauseObject);
        } else if (Search.WhereParentPathClause.NODE_TYPE.equals(primaryType)) {
            clause = new ParentPathClause(uow, whereClauseObject);
        } else if (Search.WhereParanthesisClause.NODE_TYPE.equals(primaryType)) {
            clause = new ParanthesisClause(uow, whereClauseObject);
        }
        return clause;
    }

    /**
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     */
    public Clause(LogicalOperator operator) {
        this.preClauseOperator = operator;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected Clause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(whereClause, "whereClause"); //$NON-NLS-1$

        if (whereClause.hasProperty(uow, Search.WhereClause.PRE_CLAUSE_OPERATOR)) {
            String preClauseOperatorValue = whereClause.getProperty(uow, Search.WhereClause.PRE_CLAUSE_OPERATOR).getStringValue(uow);
            setPreClauseOperator(LogicalOperator.valueOf(preClauseOperatorValue));
        }

        if (whereClause.hasProperty(uow, Search.WhereClause.ALIAS)) {
            setAlias(whereClause.getProperty(uow, Search.WhereClause.ALIAS).getStringValue(uow));
        }
    }

    /**
     * @return the parent
     */
    public ObjectSearcher getParent() {
        return this.parent;
    }

    /**
     * @param parent the parent to set
     */
    protected void setParent(ObjectSearcher parent) {
        this.parent = parent;
    }

    /**
     * @return the preClauseOperator
     */
    public LogicalOperator getPreClauseOperator() {
        return this.preClauseOperator;
    }

    protected void setPreClauseOperator(LogicalOperator clauseOperator) {
        this.preClauseOperator = clauseOperator;
    }

    protected void setProperty(String key, String value) {
        properties.put(key, value);
    }

    /**
     * @return the alias
     */
    public String getAlias() {
        return properties.get(ALIAS);
    }

    /**
     * @param alias the alias to set
     */
    protected void setAlias(String alias) {
        setProperty(ALIAS, alias);
    }

    protected void appendStringValues(StringBuffer buffer, Iterator<String> valuesIter) {
        while(valuesIter.hasNext()) {
            String value = valuesIter.next();
            buffer.append(QUOTE_MARK);
            buffer.append(value);
            buffer.append(QUOTE_MARK);

            if (valuesIter.hasNext()) {
                buffer.append(COMMA);
                buffer.append(SPACE);
            }
        }
    }

    protected void appendLogicalOperator(int position, StringBuffer buffer) {
        if (position > 0) {
            LogicalOperator operator = getPreClauseOperator();
            buffer.append(operator != null ? operator : LogicalOperator.OR);
            buffer.append(SPACE);
        }
    }

    protected String checkWhereAlias(String alias) {
        if (getParent() == null)
            return alias;

        //
        // Check that where type alias is valid for set of from types
        //
        FromType fromType = getParent().getFromType();
        if (fromType != null) {
            if (StringUtils.isEmpty(alias)) {
                // Only 1 from type and alias is empty so assume alias of single from type
                alias = fromType.getAlias();
            } else {
                ArgCheck.isTrue(fromType.getAlias().equals(alias), "Where clause alias is unknown to from clause");
            }
        }

        return alias;
    }

    /**
     * @param index the index of this clause
     * @return the string representation for this clause
     */
    public abstract String clauseString(int index);

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.preClauseOperator == null) ? 0 : this.preClauseOperator.hashCode());
        result = prime * result + ((this.properties == null) ? 0 : this.properties.hashCode());
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
        Clause other = (Clause)obj;
        if (this.preClauseOperator != other.preClauseOperator)
            return false;
        if (this.properties == null) {
            if (other.properties != null)
                return false;
        } else
            if (!this.properties.equals(other.properties))
                return false;
        return true;
    }

    protected void writeProperties(UnitOfWork uow, KomodoObject whereObject) throws KException {
        whereObject.setProperty(uow, WhereClause.PRE_CLAUSE_OPERATOR, getPreClauseOperator());
        whereObject.setProperty(uow, WhereClause.ALIAS, getAlias());
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param searchObject the parent searchObject
     * @throws KException
     *         if an error occurs
     */
    abstract void write(UnitOfWork uow, KomodoObject searchObject) throws KException;
}
