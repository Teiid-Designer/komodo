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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ICriteria;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;

/**
 *
 */
public class Criteria extends ASTNode implements Expression, ICriteria<LanguageVisitor> {

    /**
     * @param p
     * @param id
     */
    public Criteria(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return getDataTypeService().getDefaultDataClass(DataTypeName.BOOLEAN);
    }

    /**
     * @param onCriteria
     * @return
     */
    public static List<Criteria> separateCriteriaByAnd(Criteria onCriteria) {
        throw new UnsupportedOperationException();
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Criteria clone() {
        Criteria clone = new Criteria(this.getTeiidParser(), this.getId());
        return clone;
    }
}
