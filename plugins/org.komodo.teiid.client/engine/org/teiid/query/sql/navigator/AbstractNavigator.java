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

package org.teiid.query.sql.navigator;

import java.util.Collection;
import java.util.List;
import java.util.RandomAccess;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;



/** 
 * abstract parent class of navigators
 */
public class AbstractNavigator extends TCLanguageVisitorImpl {

    private TCLanguageVisitorImpl visitor;
    
    /**
     * @param visitor
     */
    public AbstractNavigator(TCLanguageVisitorImpl visitor) {
        super(visitor.getTeiidVersion());
        this.visitor = visitor;
    }
    
    /**
     * @return internal vistor
     */
    public TCLanguageVisitorImpl getVisitor() {
        return this.visitor;
    }

    protected void visitVisitor(BaseLanguageObject obj) {
    	if(this.visitor.shouldAbort()) {
            return;
        }
    	
        obj.acceptVisitor(this.visitor);
    }
    
    protected void visitNode(BaseLanguageObject obj) {
        if(this.visitor.shouldAbort()) {
            return;
        }
        
        if(obj != null) {
            obj.acceptVisitor(this);
        }
    }
    
    protected void visitNodes(Collection<? extends BaseLanguageObject> nodes) {
        if(this.visitor.shouldAbort() || nodes == null) {
            return;
        }
        int size = nodes.size();
        if (size > 0) {
        	if (nodes instanceof List<?> && nodes instanceof RandomAccess) {
        		List<? extends BaseLanguageObject> list = (List<? extends BaseLanguageObject>) nodes;
        		for (int i = 0; i < size; i++) {
        			visitNode(list.get(i));
        		}
        		return;
        	}
        	for (BaseLanguageObject languageObject : nodes) {
				visitNode(languageObject);
			}
        }
    }


}
