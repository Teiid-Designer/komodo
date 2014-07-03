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
package org.teiid.query.sql.lang;

public class SubqueryHint {
    public static String MJ = "MJ"; //$NON-NLS-1$
    public static String NOUNNEST = "NO_UNNEST"; //$NON-NLS-1$
    public static String DJ = "DJ"; //$NON-NLS-1$

    private boolean mergeJoin;
    private boolean noUnnest;
    private boolean depJoin;
    
    public void setMergeJoin(boolean semiJoin) {
        this.mergeJoin = semiJoin;
    }
    
    public boolean isMergeJoin() {
        return mergeJoin;
    }
    
    public void setNoUnnest(boolean noUnnest) {
        this.noUnnest = noUnnest;
    }
    
    public boolean isNoUnnest() {
        return noUnnest;
    }
    
    public void setDepJoin() {
        this.depJoin = true;
        this.mergeJoin = true;
    }
    
    public boolean isDepJoin() {
        return depJoin;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof SubqueryHint)) {
            return false;
        }
        SubqueryHint other = (SubqueryHint) obj;
        return mergeJoin == other.mergeJoin 
        && noUnnest == other.noUnnest 
        && depJoin == other.depJoin;
    }
    
    public SubqueryHint clone() {
        SubqueryHint clone = new SubqueryHint();
        clone.mergeJoin = this.mergeJoin;
        clone.noUnnest = this.noUnnest;
        clone.depJoin = this.depJoin;
        return clone;
    }
    
}