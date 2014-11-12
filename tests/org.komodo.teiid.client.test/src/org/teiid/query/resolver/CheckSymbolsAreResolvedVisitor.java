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

package org.teiid.query.resolver;

import java.util.ArrayList;
import java.util.Collection;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;


/**
 * Used to verify that all symbols in a LanguageObject were resolved
 * with respect to runtime metadata
 */
@SuppressWarnings( {"javadoc"} )
public class CheckSymbolsAreResolvedVisitor extends TCLanguageVisitorImpl {

    private Collection<BaseLanguageObject> unresolvedSymbols;
    
	/**
	 * @param teiidVersion
	 */
	public CheckSymbolsAreResolvedVisitor(TeiidVersion teiidVersion) {
	    super(teiidVersion);
        unresolvedSymbols = new ArrayList<BaseLanguageObject>();    
    }
    
    /**
     * Get the Collection of any unresolved symbols
     * @return Collection of any unresolved Symbols; may
     * be empty but never null
     */
    public Collection<BaseLanguageObject> getUnresolvedSymbols(){
        return this.unresolvedSymbols;
    }
    
    @Override
    public void visit(CaseExpressionImpl obj) {
        if (obj.getType() == null){
            this.unresolvedSymbols.add(obj);
        }
    }
    
    @Override
    public void visit(ElementSymbolImpl obj) {
        if (obj.getMetadataID() == null){
            this.unresolvedSymbols.add(obj);
        }
    }

    @Override
    public void visit(GroupSymbolImpl obj) {
        if (!obj.isResolved()){
            this.unresolvedSymbols.add(obj);
        }
    }

    @Override
    public void visit(SearchedCaseExpressionImpl obj) {
        if (obj.getType() == null){
            this.unresolvedSymbols.add(obj);
        }
    }
    
    @Override
    public void visit(ScalarSubqueryImpl obj) {
        if (obj.getType() == null){
            this.unresolvedSymbols.add(obj);
        }
    }
    
    @Override
    public void visit(FunctionImpl obj) {
        if (obj.getFunctionDescriptor() == null){
            this.unresolvedSymbols.add(obj);
        }
    }
    
    @Override
    public void visit(ReferenceImpl obj) {
        if (obj.getType() == null){
            this.unresolvedSymbols.add(obj);
        }
    }
}
