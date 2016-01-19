/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.modeshape.teiid.sql.lang;

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants.NonReserved;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants.Reserved;

/**
 *
 */
public class SubqueryHint extends ASTNode implements NonReserved, Reserved {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public SubqueryHint(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public boolean isMergeJoin() {
        Object property = getProperty(TeiidSqlLexicon.SubqueryHint.MERGE_JOIN_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setMergeJoin(boolean semiJoin) {
        setProperty(TeiidSqlLexicon.SubqueryHint.MERGE_JOIN_PROP_NAME, semiJoin);
    }

    public boolean isNoUnnest() {
        Object property = getProperty(TeiidSqlLexicon.SubqueryHint.NO_UNNEST_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setNoUnnest(boolean noUnnest) {
        setProperty(TeiidSqlLexicon.SubqueryHint.NO_UNNEST_PROP_NAME, noUnnest);
    }

    public boolean isDepJoin() {
        Object property = getProperty(TeiidSqlLexicon.SubqueryHint.DEP_JOIN_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setDepJoin(boolean depJoin) {
        setProperty(TeiidSqlLexicon.SubqueryHint.DEP_JOIN_PROP_NAME, depJoin);
    }    

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof SubqueryHint)) {
            return false;
        }
        SubqueryHint other = (SubqueryHint)obj;
        return isMergeJoin() == other.isMergeJoin() && isNoUnnest() == other.isNoUnnest() && isDepJoin() == other.isDepJoin();
    }

    @Override
    public SubqueryHint clone() {
        SubqueryHint clone = new SubqueryHint(getTeiidParser(), getId());
        clone.setMergeJoin(this.isMergeJoin());
        clone.setNoUnnest(this.isNoUnnest());
        clone.setDepJoin(this.isDepJoin());
        return clone;
    }

}
