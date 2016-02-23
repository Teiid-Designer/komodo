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
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.lang.CompoundCriteria;
import org.komodo.spi.query.sql.lang.Criteria;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class CriteriaImpl extends ASTNode implements BaseExpression, Criteria<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public CriteriaImpl(TeiidSeqParser p, int id) {
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
     * Helper method for {@link #separateCriteriaByAnd(CriteriaImpl)} that 
     * can be called recursively to collect parts.
     * @param crit Crit to break apart
     * @param parts Collection to add parts to
     */
    private static void separateCriteria(CriteriaImpl crit, Collection<CriteriaImpl> parts) {
        if(crit instanceof CompoundCriteriaImpl) {
            CompoundCriteriaImpl compCrit = (CompoundCriteriaImpl) crit;
            if(compCrit.getOperator() == CompoundCriteria.AND) {
                for (CriteriaImpl conjunct : compCrit.getCriteria()) {
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
    public static List<CriteriaImpl> separateCriteriaByAnd(CriteriaImpl crit) {
        if(crit == null) { 
            return Collections.emptyList();
        }
        
        List<CriteriaImpl> parts = new ArrayList<CriteriaImpl>();
        separateCriteria(crit, parts);
        return parts;           
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public CriteriaImpl clone() {
        CriteriaImpl clone = new CriteriaImpl(this.getTeiidParser(), this.getId());
        return clone;
    }
}
