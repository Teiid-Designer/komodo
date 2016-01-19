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

package org.teiid.query.metadata;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.teiid.query.sql.symbol.ElementSymbolImpl;


public class GroupInfo implements Serializable {
	
	private static final long serialVersionUID = 5724520038004637086L;

	public static final String CACHE_PREFIX = "groupinfo/"; //$NON-NLS-1$
	
	private Map<Object, ElementSymbolImpl> idToSymbolMap;
	private List<ElementSymbolImpl> symbolList;
	private Map<String, ElementSymbolImpl> shortNameToSymbolMap; 
	
	public GroupInfo(LinkedHashMap<Object, ElementSymbolImpl> symbols) {
		this.idToSymbolMap = symbols;
		this.symbolList = Collections.unmodifiableList(new ArrayList<ElementSymbolImpl>(symbols.values()));
		this.shortNameToSymbolMap = new TreeMap<String, ElementSymbolImpl>(String.CASE_INSENSITIVE_ORDER);
		for (ElementSymbolImpl symbol : symbolList) {
			shortNameToSymbolMap.put(symbol.getShortName(), symbol);
		}
	}
	
	public List<ElementSymbolImpl> getSymbolList() {
		return symbolList;
	}
	
	public ElementSymbolImpl getSymbol(Object metadataID) {
		return idToSymbolMap.get(metadataID);
	}
	
	public ElementSymbolImpl getSymbol(String shortCanonicalName) {
		return shortNameToSymbolMap.get(shortCanonicalName);
	}

}
