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
import org.komodo.spi.query.sql.SQLConstants;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * Abstract Clause implementation
 */
abstract class Clause implements SQLConstants {

    public enum LogicalOperator {

        OR,

        AND
    }

    private final ObjectSearcher parent;

    private final LogicalOperator preClauseOperator;

    protected final Map<String, String> properties = new HashMap<String, String>();

    /**
     * @param parent the parent search of this clause
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     */
    public Clause(ObjectSearcher parent, LogicalOperator operator) {
        this.parent = parent;
        this.preClauseOperator = operator;
    }

    /**
     * @return the parent
     */
    public ObjectSearcher getParent() {
        return this.parent;
    }

    /**
     * @return the preClauseOperator
     */
    public LogicalOperator getPreClauseOperator() {
        return this.preClauseOperator;
    }

    protected void setProperty(String key, String value) {
        properties.put(key, value);
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
        //
        // Check that where type alias is valid for set of from types
        //
        if (parent.getFromTypes().size() == 1 && StringUtils.isEmpty(alias)) {
            // Only 1 from type and alias is empty so assume alias of single from type
            alias = parent.getFromTypes().iterator().next().getAlias();
        } else {
            //
            // More than 1 from type or alias is not empty
            //
            ArgCheck.isNotEmpty(alias);

            boolean aliasTypeFound = false;
            for (FromType fromType : parent.getFromTypes()) {
                if (fromType.getAlias().equals(alias)) {
                    aliasTypeFound = true;
                    break;
                }
            }

            ArgCheck.isTrue(aliasTypeFound, "Where clause alias is unknown to from clause"); //$NON-NLS-1$
        }

        return alias;
    }

    public abstract String clauseString(int index);

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.parent == null) ? 0 : this.parent.hashCode());
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
        if (this.parent == null) {
            if (other.parent != null)
                return false;
        } else if (!this.parent.equals(other.parent))
            return false;
        if (this.preClauseOperator != other.preClauseOperator)
            return false;
        if (this.properties == null) {
            if (other.properties != null)
                return false;
        } else if (!this.properties.equals(other.properties))
            return false;
        return true;
    }
}
