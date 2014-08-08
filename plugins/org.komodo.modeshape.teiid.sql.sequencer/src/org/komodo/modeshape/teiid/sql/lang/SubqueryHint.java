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

import org.komodo.modeshape.teiid.parser.TeiidParser;

public class SubqueryHint extends ASTNode {

    public static String MJ = "MJ"; //$NON-NLS-1$

    public static String NOUNNEST = "NO_UNNEST"; //$NON-NLS-1$

    public static String DJ = "DJ"; //$NON-NLS-1$

    /**
     * @param p
     * @param id
     */
    public SubqueryHint(TeiidParser p, int id) {
        super(p, id);
    }

    public void setMergeJoin(boolean semiJoin) {
    }

    public boolean isMergeJoin() {
        return false;
    }

    public void setNoUnnest(boolean noUnnest) {

    }

    public boolean isNoUnnest() {
        return false;

    }

    public void setDepJoin(boolean b) {
    }

    public boolean isDepJoin() {
        return false;
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
