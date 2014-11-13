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

package org.teiid.query.validator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;

import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;


public class AbstractValidationVisitor extends TCLanguageVisitorImpl {
    
    // Exception handling
    private Exception exception;
    private BaseLanguageObject exceptionObject;
        
    // Validation error handling
    protected ValidatorReport report;
    
    private QueryMetadataInterface metadata;
    
    protected CommandImpl currentCommand;
    protected Stack<BaseLanguageObject> stack = new Stack<BaseLanguageObject>();
    
    /**
     * @param teiidVersion
     */
    public AbstractValidationVisitor(TeiidVersion teiidVersion) {
        super(teiidVersion);
        this.report = new ValidatorReport();
    }

    public void setMetadata(QueryMetadataInterface metadata) {
        this.metadata = metadata;
    }
    
    protected QueryMetadataInterface getMetadata() {
        return this.metadata;
    } 
    
    /**
     * Reset so visitor can be used on a different language object.  This does 
     * not wipe the report.
     */
    public void reset() {
        this.currentCommand = null;
        this.stack.clear();
    }
    
    // ######################### Store results info #########################

    protected void handleValidationError(String message) {
        this.report.addItem(new ValidatorFailure(message));
    }

    protected void handleValidationError(String message, BaseLanguageObject invalidObj) {
        this.report.addItem(new ValidatorFailure(message, invalidObj));
    }

    protected void handleValidationError(String message, Collection invalidObjs) {
        this.report.addItem(new ValidatorFailure(message, invalidObjs));
    }

    protected void handleException(Exception e) { 
        handleException(e, null);
    }

    protected void handleException(Exception e, BaseLanguageObject obj) { 
        // Store exception information
        this.exceptionObject = obj;
        this.exception = e;
        
        // Abort the validation process
        setAbort(true);
    }

    // ######################### Report results info #########################

    public Exception getException() { 
        return this.exception;
    }
    
    public BaseLanguageObject getExceptionObject() { 
        return this.exceptionObject;
    }
    
    public ValidatorReport getReport() { 
        return this.report;
    }
    
    // ######################### Helper methods for validation #########################
    /**
	 * Check to verify if the query would return XML results.
     * @param query the query to check
	 */
	protected boolean isXMLCommand(CommandImpl command) {
		if (command instanceof QueryImpl) {
		    return ((QueryImpl)command).getIsXML();
        }
        return false;
	}   
	
    protected Collection<ElementSymbolImpl> validateElementsSupport(Collection<ElementSymbolImpl> elements, int supportsFlag) {
	    // Collect any identifiers not supporting flag
	    List<ElementSymbolImpl> dontSupport = null;  
        ElementSymbolImpl symbol = null;              

        try {
	        Iterator<ElementSymbolImpl> elemIter = elements.iterator();
            while(elemIter.hasNext()) {
		    symbol = elemIter.next();
               if(! getMetadata().elementSupports(symbol.getMetadataID(), supportsFlag)) {
                    if(dontSupport == null) { 
                        dontSupport = new ArrayList<ElementSymbolImpl>();
                    } 
                    dontSupport.add(symbol);    
                }            
		    }
        } catch(Exception e) {
            handleException(e, symbol);
        } 

        return dontSupport;
    }

}

