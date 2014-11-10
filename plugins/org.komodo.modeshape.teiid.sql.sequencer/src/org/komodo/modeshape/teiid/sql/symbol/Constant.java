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

import java.math.BigDecimal;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.IConstant;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class Constant extends ASTNode implements Expression, IConstant<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public Constant(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    private void assignType(DataTypeName dataTypeName) {
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, dataTypeName.name());
    }

    public void setType(Class<?> type) {
        DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(type);
        assignType(dataTypeName);
    }

    @Override
    public Object getValue() {
        Object property = getProperty(TeiidSqlLexicon.Constant.VALUE_PROP_NAME);
        return property == null ? null : property;
    }

    public void setValue(Object arg) {
        setProperty(TeiidSqlLexicon.Constant.VALUE_PROP_NAME, arg);
        if (arg == null) {
            assignType(DataTypeName.NULL);
        } else { 
            Class<?> type = arg.getClass();

            Class<?> originalType = type;
            while (type.isArray() && !type.getComponentType().isPrimitive()) {
                type = type.getComponentType();
            }

            DataTypeName dataTypeName = getTeiidParser().getDataTypeService().retrieveDataTypeName(type);
            if (dataTypeName != null && originalType.isArray()) {
                //array of a runtime-type
                assignType(dataTypeName.getArrayType());
            } else if (dataTypeName != null) {
                assignType(dataTypeName);
            } else if (originalType.isArray() && !originalType.getComponentType().isPrimitive())
                assignType(DataTypeName.OBJECT_ARRAY);
            else
                assignType(DataTypeName.OBJECT);
        }
    }

    @Override
    public boolean isMultiValued() {
        Object property = getProperty(TeiidSqlLexicon.Constant.MULTI_VALUED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setMultiValued(boolean multiValued) {
        setProperty(TeiidSqlLexicon.Constant.MULTI_VALUED_PROP_NAME, multiValued);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();

        if (this.getValue() != null && !isMultiValued()) {
            if (this.getValue() instanceof BigDecimal) {
                BigDecimal bd = (BigDecimal)this.getValue();
                int xsign = bd.signum();
                if (xsign == 0)
                    return 0;
                bd = bd.stripTrailingZeros();
                return prime * result + bd.hashCode();
            }

            result = prime * result + this.getValue().hashCode();
        }

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
        Constant other = (Constant)obj;

        if (this.getValue() == null && other.getValue() == null) {
            // Only consider type information if values are not null
            return true;
        }

        if (this.getValue() instanceof BigDecimal) {
            if (this.getValue() == other.getValue()) {
                return true;
            }
            if (!(other.getValue() instanceof BigDecimal)) {
                return false;
            }
            return ((BigDecimal)this.getValue()).compareTo((BigDecimal)other.getValue()) == 0;
        }

        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;

        return isMultiValued() == other.isMultiValued() && other.getValue().equals(this.getValue());
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public Constant clone() {
        Constant clone = new Constant(this.getTeiidParser(), this.getId());

        if (getType() != null)
            clone.setType(getType());
        clone.setMultiValued(isMultiValued());
        if (getValue() != null)
            clone.setValue(getValue());

        return clone;
    }

}
