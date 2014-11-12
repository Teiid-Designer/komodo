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

package org.komodo.modeshape.teiid.sql.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.komodo.modeshape.teiid.sql.symbol.AliasSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.ExpressionSymbolImpl;
import org.komodo.utils.ArgCheck;


public class SymbolMap {

    private LinkedHashMap<ElementSymbolImpl, BaseExpression> map = new LinkedHashMap<ElementSymbolImpl, BaseExpression>();
    private Map<ElementSymbolImpl, BaseExpression> unmodifiableMap = Collections.unmodifiableMap(map);

    public SymbolMap() {
	}
    
    @Override
    public SymbolMap clone() {
    	SymbolMap clonedMap = new SymbolMap();
    	for (Map.Entry<ElementSymbolImpl, BaseExpression> entry : map.entrySet()) {
			clonedMap.addMapping(entry.getKey().clone(), entry.getValue().clone());
		}
    	return clonedMap;
    }
    
    public Map<BaseExpression, ElementSymbolImpl> inserseMapping() {
    	HashMap<BaseExpression, ElementSymbolImpl> inverseMap = new HashMap<BaseExpression, ElementSymbolImpl>();
		for (Map.Entry<ElementSymbolImpl, BaseExpression> entry : this.map.entrySet()) {
			inverseMap.put(entry.getValue(), entry.getKey());
		}
		return inverseMap;
    }
    
    /**
     * @return true if the map did not already contained the given symbol
     */
    public boolean addMapping(ElementSymbolImpl symbol,
                              BaseExpression expression) {
        return map.put(symbol, getExpression(expression)) == null;
    }

    public static final BaseExpression getExpression(BaseExpression symbol) {
        if (symbol instanceof AliasSymbolImpl) {
            symbol = ((AliasSymbolImpl)symbol).getSymbol();
        }

        if (symbol instanceof ExpressionSymbolImpl) {
            ExpressionSymbolImpl exprSymbol = (ExpressionSymbolImpl)symbol;
            return exprSymbol.getExpression();
        }

        return symbol;
    }

    public BaseExpression getMappedExpression(ElementSymbolImpl symbol) {
        return map.get(symbol);
    }
    
    public Map<ElementSymbolImpl, BaseExpression> asUpdatableMap() {
    	return this.map;
    }

    public Map<ElementSymbolImpl, BaseExpression> asMap() {
        return unmodifiableMap;
    }

    public List<ElementSymbolImpl> getKeys() {
        return new ArrayList<ElementSymbolImpl>(map.keySet());
    }
    
    public List<BaseExpression> getValues() {
        return new ArrayList<BaseExpression>(map.values());
    }

    public static final SymbolMap createSymbolMap(List<ElementSymbolImpl> virtualElements,
                                                  List<? extends BaseExpression> mappedCols) {
        ArgCheck.isTrue(virtualElements.size() == mappedCols.size(), "elements must match columns"); //$NON-NLS-1$
        SymbolMap symbolMap = new SymbolMap();
        Iterator<ElementSymbolImpl> keyIter = virtualElements.iterator();
        for (BaseExpression symbol : mappedCols) {
            symbolMap.addMapping(keyIter.next(), symbol);
        }

        return symbolMap;
    }
    
    /** 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return map.toString();
    }

}
