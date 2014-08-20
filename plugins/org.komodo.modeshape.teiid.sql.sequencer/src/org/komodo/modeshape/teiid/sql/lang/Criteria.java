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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ICompoundCriteria;
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
        assignTypeName(DataTypeName.BOOLEAN);
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    private void assignTypeName(DataTypeName dataTypeName) {
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, dataTypeName.name());
    }

    /**
     * Helper method for {@link #separateCriteriaByAnd(Criteria)} that 
     * can be called recursively to collect parts.
     * @param crit Crit to break apart
     * @param parts Collection to add parts to
     */
    private static void separateCriteria(Criteria crit, Collection<Criteria> parts) {
        if(crit instanceof CompoundCriteria) {
            CompoundCriteria compCrit = (CompoundCriteria) crit;
            if(compCrit.getOperator() == ICompoundCriteria.AND) {
                for (Criteria conjunct : compCrit.getCriteria()) {
                    separateCriteria(conjunct, parts);
                }
            } else {
                parts.add(crit);    
            }
        } else {
            parts.add(crit);        
        }   
    }

    /**
     * This utility method will pull apart a tree of criteria by breaking all
     * compound AND criteria apart.  For instance, ((A=1 AND B=2) AND C=3) 
     * will be broken into A=1, B=2, C=3.  
     * @param crit Criteria to break apart
     * @return List of Criteria, empty list if crit is null
     */     
    public static List<Criteria> separateCriteriaByAnd(Criteria crit) {
        if(crit == null) { 
            return Collections.emptyList();
        }
        
        List<Criteria> parts = new ArrayList<Criteria>();
        separateCriteria(crit, parts);
        return parts;           
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
