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

package org.teiid.query.sql.visitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;

import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.navigator.PreOrPostOrderNavigator;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.BaseWindowFunction;


public class AggregateSymbolCollectorVisitorImpl extends TCLanguageVisitorImpl {
    
    public static class AggregateStopNavigator extends PreOrPostOrderNavigator {
    	
    	private Collection<? extends BaseExpression> groupingCols;
    	private Collection<? super BaseExpression> groupingColsUsed;
    	
        public AggregateStopNavigator(TCLanguageVisitorImpl visitor, Collection<? super BaseExpression> groupingColsUsed, Collection<? extends BaseExpression> groupingCols) {
            super(visitor, PreOrPostOrderNavigator.PRE_ORDER, false);
            this.groupingCols = groupingCols;
            this.groupingColsUsed = groupingColsUsed;
        }
        
        public AggregateStopNavigator(TCLanguageVisitorImpl visitor) {
            super(visitor, PreOrPostOrderNavigator.PRE_ORDER, true);
        }
        
        public void visit(BaseAggregateSymbol obj) {
            // Visit aggregate symbol but do not dive into it's expression
            preVisitVisitor(obj);
            postVisitVisitor(obj);
        }
        
        @Override
        protected void visitNode(BaseLanguageObject obj) {
        	if (groupingCols != null && obj instanceof BaseExpression && groupingCols.contains(obj)) {
        		if (groupingColsUsed != null) {
        			groupingColsUsed.add((BaseExpression)obj);
        		}
        		return;
        	}
        	super.visitNode(obj);
        }
        
    }

    private Collection<? super BaseAggregateSymbol> aggregates;
    private Collection<? super ElementSymbolImpl> otherElements;
    private Collection<? super BaseWindowFunction> windowFunctions;
    
	public AggregateSymbolCollectorVisitorImpl(TeiidVersion teiidVersion, Collection<? super BaseAggregateSymbol> aggregates, Collection<? super ElementSymbolImpl> elements) {
	    super(teiidVersion);
        this.aggregates = aggregates;
        this.otherElements = elements;
	}
    
    public void visit(BaseAggregateSymbol obj) {
        if (aggregates != null && !obj.isWindowed()) {
            this.aggregates.add(obj);
        }
    }
    
    public void visit(BaseWindowFunction windowFunction) {
    	if (this.windowFunctions != null) {
    		this.windowFunctions.add(windowFunction);
    	}
    }
    
    public void visit(ElementSymbolImpl obj) {
        if (this.otherElements != null && !obj.isExternalReference()) {
            this.otherElements.add(obj);  
        }
    }

    public static final void getAggregates(BaseLanguageObject obj, 
    		Collection<? super BaseAggregateSymbol> aggregates, 
    		Collection<? super ElementSymbolImpl> otherElements, 
    		Collection<? super BaseExpression> groupingColsUsed, 
    		Collection<? super BaseWindowFunction> windowFunctions, 
    		Collection<? extends BaseExpression> groupingCols) {
        AggregateSymbolCollectorVisitorImpl visitor = new AggregateSymbolCollectorVisitorImpl(obj.getTeiidVersion(), aggregates, otherElements);
        visitor.windowFunctions = windowFunctions;
        AggregateStopNavigator asn = new AggregateStopNavigator(visitor, groupingColsUsed, groupingCols);
        asn.visitNode(obj);
    }

    public static final Collection<BaseAggregateSymbol> getAggregates(BaseLanguageObject obj, boolean removeDuplicates) {
    	if (obj == null) {
    		return Collections.emptyList();
    	}
        Collection<BaseAggregateSymbol> aggregates = null;
        if (removeDuplicates) {
            aggregates = new LinkedHashSet<BaseAggregateSymbol>();
        } else {
            aggregates = new ArrayList<BaseAggregateSymbol>();    
        }
        AggregateSymbolCollectorVisitorImpl visitor = new AggregateSymbolCollectorVisitorImpl(obj.getTeiidVersion(), aggregates, null);
        AggregateStopNavigator asn = new AggregateStopNavigator(visitor, null, null);
        obj.acceptVisitor(asn);
        return aggregates;
    }
        
}
