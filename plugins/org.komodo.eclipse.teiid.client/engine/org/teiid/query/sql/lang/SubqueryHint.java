/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
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