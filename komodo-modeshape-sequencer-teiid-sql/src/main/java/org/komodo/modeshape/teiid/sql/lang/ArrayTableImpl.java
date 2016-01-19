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
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.query.sql.lang.ArrayTable;

/**
 *
 */
public class ArrayTableImpl extends TableFunctionReferenceImpl implements ArrayTable<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public ArrayTableImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    /**
     * @return columns
     */
    @Override
    public List<ProjectedColumnImpl> getColumns() {
        return getChildrenforIdentifierAndRefType(
                                               TeiidSqlLexicon.ArrayTable.COLUMNS_REF_NAME, ProjectedColumnImpl.class);
    }

    /**
     * @param columns columns to set
     */
    public void setColumns(List<ProjectedColumnImpl> columns) {
        setChildren(TeiidSqlLexicon.ArrayTable.COLUMNS_REF_NAME, columns);
    }

    /**
     * @return array value
     */
    public BaseExpression getArrayValue() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.ArrayTable.ARRAY_VALUE_REF_NAME, BaseExpression.class);
    }

    /**
     * @param arrayValue expression to set
     */
    public void setArrayValue(BaseExpression arrayValue) {
        setChild(TeiidSqlLexicon.ArrayTable.ARRAY_VALUE_REF_NAME, arrayValue);
    }

    

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getArrayValue() == null) ? 0 : this.getArrayValue().hashCode());
        result = prime * result + ((this.getColumns() == null) ? 0 : this.getColumns().hashCode());
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
        ArrayTableImpl other = (ArrayTableImpl)obj;
        if (this.getArrayValue() == null) {
            if (other.getArrayValue() != null)
                return false;
        } else if (!this.getArrayValue().equals(other.getArrayValue()))
            return false;
        if (this.getColumns() == null) {
            if (other.getColumns() != null)
                return false;
        } else if (!this.getColumns().equals(other.getColumns()))
            return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public ArrayTableImpl clone() {
        ArrayTableImpl clone = new ArrayTableImpl(this.getTeiidParser(), this.getId());

        if(getColumns() != null)
            clone.setColumns(cloneList(getColumns()));
        if(getArrayValue() != null)
            clone.setArrayValue(getArrayValue().clone());
        if(getName() != null)
            clone.setName(getName());
        clone.setOptional(isOptional());
        clone.setMakeInd(isMakeInd());
        clone.setNoUnnest(isNoUnnest());
        if (getMakeDependency() != null)
            clone.setMakeDependency(getMakeDependency().clone());
        clone.setMakeNotDep(isMakeNotDep());
        clone.setPreserve(isPreserve());

        return clone;
    }

}
