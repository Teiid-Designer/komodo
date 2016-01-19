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
import java.util.LinkedList;
import java.util.List;

import org.komodo.spi.query.sql.ValueIteratorProviderCollectorVisitor;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.navigator.PreOrderNavigator;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;


/**
 * <p>This visitor class will traverse a language object tree and collect all language
 * objects that implement {@link BaseSubqueryContainer}.  
 * By default it uses a java.util.ArrayList to collect the objects in the order 
 * they're found.</p>
 * 
 * <p>The easiest way to use this visitor is to call one of the static methods which create 
 * the visitor, run the visitor, and get the collection. 
 * The public visit() methods should NOT be called directly.</p>
 */
public class ValueIteratorProviderCollectorVisitorImpl<T extends CommandImpl> extends TCLanguageVisitorImpl
    implements ValueIteratorProviderCollectorVisitor<BaseLanguageObject, BaseSubqueryContainer<T>> {

    private List<BaseSubqueryContainer<T>> valueIteratorProviders;
    
    /**
     * Construct a new visitor with the default collection type, which is a 
     * {@link java.util.ArrayList}.  
     * @param teiidVersion
     */
    public ValueIteratorProviderCollectorVisitorImpl(TeiidVersion teiidVersion) {
        super(teiidVersion);
        this.valueIteratorProviders = new ArrayList<BaseSubqueryContainer<T>>();
    }   

	/**
	 * Construct a new visitor with the given Collection to accumulate
     * ValueIteratorProvider instances
     * @param teiidVersion
	 * @param valueIteratorProviders Collection to accumulate found 
	 */
	ValueIteratorProviderCollectorVisitorImpl(TeiidVersion teiidVersion, List<BaseSubqueryContainer<T>> valueIteratorProviders) {
	    super(teiidVersion);
		this.valueIteratorProviders = valueIteratorProviders;
	}   
    
    /**
     * Get the value iterator providers collected by the visitor.  This should best be called 
     * after the visitor has been run on the language object tree.
     * @return Collection of {@link BaseSubqueryContainer}
     * (by default, this is a java.util.ArrayList)
     */
    public List<BaseSubqueryContainer<T>> getValueIteratorProviders() { 
        return this.valueIteratorProviders;
    }
    
    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(SubquerySetCriteriaImpl obj) {
        this.valueIteratorProviders.add((BaseSubqueryContainer<T>) obj);
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
        this.valueIteratorProviders.add((BaseSubqueryContainer<T>) obj);
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(ExistsCriteriaImpl obj) {
        this.valueIteratorProviders.add((BaseSubqueryContainer<T>) obj);
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(ScalarSubqueryImpl obj) {
        this.valueIteratorProviders.add((BaseSubqueryContainer<T>) obj);
    }

    @Override
    public List<BaseSubqueryContainer<T>> findValueIteratorProviders(BaseLanguageObject obj) {
        PreOrderNavigator.doVisit(obj, this);
        return getValueIteratorProviders();
    }

    /**
     * Helper to quickly get the ValueIteratorProvider instances from obj
     * @param obj Language object
     * @return java.util.ArrayList of found ValueIteratorProvider
     */
    public static final <T extends CommandImpl> List<BaseSubqueryContainer<T>> getValueIteratorProviders(BaseLanguageObject obj) {
        ValueIteratorProviderCollectorVisitorImpl<T> visitor = new ValueIteratorProviderCollectorVisitorImpl<T>(obj.getTeiidVersion());
        return visitor.findValueIteratorProviders(obj);
    }

	/**
	 * @param obj
	 * @param valueIteratorProviders
	 */
	public static final <T extends CommandImpl> void getValueIteratorProviders(BaseLanguageObject obj, List<BaseSubqueryContainer<T>> valueIteratorProviders) {
		ValueIteratorProviderCollectorVisitorImpl<T> visitor = new ValueIteratorProviderCollectorVisitorImpl<T>(obj.getTeiidVersion(), valueIteratorProviders);
		visitor.findValueIteratorProviders(obj);
	}
          	
    /**
     * @param languageObjects
     * @return list of {@link BaseSubqueryContainer}s
     */
    public static final <T extends CommandImpl> List<BaseSubqueryContainer<T>> getValueIteratorProviders(Collection<? extends BaseLanguageObject> languageObjects) {
    	if (languageObjects == null || languageObjects.isEmpty()) {
    		return Collections.emptyList();
    	}
    	BaseLanguageObject languageObject = languageObjects.iterator().next();
    	List<BaseSubqueryContainer<T>> result = new LinkedList<BaseSubqueryContainer<T>>();
        ValueIteratorProviderCollectorVisitorImpl<T> visitor = new ValueIteratorProviderCollectorVisitorImpl<T>(languageObject.getTeiidVersion(), result);
        for (BaseLanguageObject obj : languageObjects) {
            visitor.findValueIteratorProviders(obj);
        }
        return result;
    }            
}
