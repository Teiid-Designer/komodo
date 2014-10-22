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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.IWindowFunction;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;

/**
 *
 */
public class WindowFunction extends ASTNode implements Expression, IWindowFunction<LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public WindowFunction(ITeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    public AggregateSymbol getFunction() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.WindowFunction.FUNCTION_REF_NAME, AggregateSymbol.class);
    }

    public void setFunction(AggregateSymbol agg) {
        setChild(TeiidSqlLexicon.WindowFunction.FUNCTION_REF_NAME, agg);

        DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(agg.getType());
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, dataTypeName.name());
    }

    public WindowSpecification getWindowSpecification() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.WindowFunction.WINDOW_SPECIFICATION_REF_NAME, WindowSpecification.class);
    }

    public void setWindowSpecification(WindowSpecification ws) {
        setChild(TeiidSqlLexicon.WindowFunction.WINDOW_SPECIFICATION_REF_NAME, ws);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getFunction() == null) ? 0 : this.getFunction().hashCode());
        result = prime * result + ((this.getWindowSpecification() == null) ? 0 : this.getWindowSpecification().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        WindowFunction other = (WindowFunction)obj;
        if (this.getFunction() == null) {
            if (other.getFunction() != null) return false;
        } else if (!this.getFunction().equals(other.getFunction())) return false;
        if (this.getWindowSpecification() == null) {
            if (other.getWindowSpecification() != null) return false;
        } else if (!this.getWindowSpecification().equals(other.getWindowSpecification())) return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public WindowFunction clone() {
        WindowFunction clone = new WindowFunction(this.getTeiidParser(), this.getId());

        if(getFunction() != null)
            clone.setFunction(getFunction().clone());
        if(getWindowSpecification() != null)
            clone.setWindowSpecification(getWindowSpecification().clone());

        return clone;
    }

}
