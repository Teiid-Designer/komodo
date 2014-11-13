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
import java.util.HashSet;

import org.komodo.spi.query.sql.FunctionCollectorVisitor;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.metadata.FunctionMethod.Determinism;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.navigator.DeepPreOrderNavigator;
import org.teiid.query.sql.navigator.PreOrderNavigator;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.runtime.client.Messages;


/**
 * <p>This visitor class will traverse a language object tree and collect all Function
 * references it finds.  It uses a collection to collect the Functions in so
 * different collections will give you different collection properties - for instance,
 * using a Set will remove duplicates.</p>
 * 
 * <p>This visitor can optionally collect functions of only a specific name</p>
 *
 * <p>The easiest way to use this visitor is to call the static methods which create
 * the visitor (and possibly the collection), run the visitor, and return the collection.
 * The public visit() methods should NOT be called directly.</p>
 */
public class FunctionCollectorVisitorImpl extends TCLanguageVisitorImpl
    implements FunctionCollectorVisitor<BaseLanguageObject, FunctionImpl> {    

    private Collection<FunctionImpl> functions;
    
    private String functionName;

    /**
     * Construct a new visitor with a default returning collection
     *
     * @param teiidVersion
     * @param removeDuplicates 
     */
    public FunctionCollectorVisitorImpl(TeiidVersion teiidVersion, boolean removeDuplicates) {
        this(teiidVersion, removeDuplicates ? new HashSet<FunctionImpl>() : new ArrayList<FunctionImpl>());
    }
    
    /**
     * Construct a new visitor with the specified collection, which should
     * be non-null.
     *
     * @param teiidVersion
     * @param functions
     * @throws IllegalArgumentException If elements is null
     */
	public FunctionCollectorVisitorImpl(TeiidVersion teiidVersion, Collection<FunctionImpl> functions) {
        this(teiidVersion, functions, null);
	}

    /**
     * Construct a new visitor with the specified collection, which should
     * be non-null.
     *
     * @param teiidVersion
     * @param functions
     * @param functionName
     *
     * @throws IllegalArgumentException If elements is null
     */
    public FunctionCollectorVisitorImpl(TeiidVersion teiidVersion, Collection<FunctionImpl> functions, String functionName) {
        super(teiidVersion);
        if(functions == null) {
            throw new IllegalArgumentException(Messages.getString(Messages.ERR.ERR_015_010_0022));
        }
        this.functions = functions;
        this.functionName = functionName;
    }    
    
    /**
     * Get the elements collected by the visitor.  This should best be called
     * after the visitor has been run on the language object tree.
     * @return Collection of {@link ElementSymbolImpl}
     */
    public Collection<FunctionImpl> getFunctions() {
        return this.functions;
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(FunctionImpl obj) {
        if (this.functionName == null || obj.getName().equalsIgnoreCase(this.functionName)) {
            this.functions.add(obj);
        }
    }
    
    @Override
    public Collection<FunctionImpl> findFunctions(BaseLanguageObject obj, boolean deep) {
        if (!deep) {
            PreOrderNavigator.doVisit(obj, this);
        } else {
            DeepPreOrderNavigator.doVisit(obj, this);
        }
        
        return functions;
    }

    /**
     * Helper to quickly get the elements from obj in the elements collection
     * @param obj Language object
     * @param functions Collection to collect elements in
     */
    public static final void getFunctions(BaseLanguageObject obj, Collection<FunctionImpl> functions) {
        getFunctions(obj, functions, false);
    }
    
    /**
     * Helper to quickly get the elements from obj in the elements collection
     *
     * @param obj Language object
     * @param functions Collection to collect elements in
     * @param deep
     */
    public static final void getFunctions(BaseLanguageObject obj, Collection<FunctionImpl> functions, boolean deep) {
        FunctionCollectorVisitorImpl visitor = new FunctionCollectorVisitorImpl(obj.getTeiidVersion(), functions);
        visitor.findFunctions(obj, deep);
    }

    /**
     * Helper to quickly get the elements from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate elements will be
     * filtered out.
     *
     * @param obj Language object
     * @param removeDuplicates True to remove duplicates
     * @return Collection of {@link ElementSymbolImpl}
     */
    public static final Collection<FunctionImpl> getFunctions(BaseLanguageObject obj, boolean removeDuplicates) {
        return getFunctions(obj, removeDuplicates, false);
    }

    /**
     * Helper to quickly get the elements from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate elements will be
     * filtered out.
     *
     * @param obj
     * @param removeDuplicates
     * @param deep
     * @return Collection of {@link ElementSymbolImpl}
     */
    public static final Collection<FunctionImpl> getFunctions(BaseLanguageObject obj, boolean removeDuplicates, boolean deep) {
        Collection<FunctionImpl> functions = null;
        if(removeDuplicates) {
            functions = new HashSet<FunctionImpl>();
        } else {
            functions = new ArrayList<FunctionImpl>();
        }
        getFunctions(obj, functions, deep);
        return functions;
    }
    
	/**
	 * @param ex
	 * @return true if non deterministic
	 */
	public static boolean isNonDeterministic(BaseLanguageObject ex) {
		Collection<FunctionImpl> functions = FunctionCollectorVisitorImpl.getFunctions(ex, true, false);
		for (FunctionImpl function : functions) {
			if ( function.getFunctionDescriptor().getDeterministic() == Determinism.NONDETERMINISTIC) {
				return true;
			}
		}
		return false;
	}

}
