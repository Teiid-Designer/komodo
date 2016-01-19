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

package org.teiid.query.optimizer.relational;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.komodo.spi.query.sql.lang.SetQuery.Operation;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.QueryCommandImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.util.SymbolMap;

/**
 * TODO: support recursive detection of partitions
 * 
 * Extracts a map of partitioning information from a union
 */
public class PartitionAnalyzer {
	
	public static Map<ElementSymbolImpl, List<Set<ConstantImpl>>> extractPartionInfo(SetQueryImpl setQuery, List<ElementSymbolImpl> projectedSymbols) {
		List<QueryImpl> queries = new LinkedList<QueryImpl>();
    	if (!extractQueries(setQuery, queries)) {
    		return Collections.emptyMap();
    	}
		Map<ElementSymbolImpl, List<Set<ConstantImpl>>> partitions = new LinkedHashMap<ElementSymbolImpl, List<Set<ConstantImpl>>>();
		boolean first = true;
		for (QueryImpl query : queries) {
			Map<ElementSymbolImpl, Set<ConstantImpl>> info = extractPartitionInfo(query, projectedSymbols);
			
			partitions.keySet().retainAll(info.keySet());
			
			if (first) {
    			first = false;
    			for (Map.Entry<ElementSymbolImpl, Set<ConstantImpl>> entry : info.entrySet()) {
    				ArrayList<Set<ConstantImpl>> values = new ArrayList<Set<ConstantImpl>>(queries.size());
					partitions.put(entry.getKey(), values);
					values.add(entry.getValue());
    			}
    			continue;
			} 
			Set<ElementSymbolImpl> keys = partitions.keySet();
			
			for (Iterator<ElementSymbolImpl> iter = keys.iterator(); iter.hasNext();) {
				ElementSymbolImpl elementSymbol = iter.next();
				List<Set<ConstantImpl>> values = partitions.get(elementSymbol);
				Set<ConstantImpl> value = info.get(elementSymbol);
				for (Set<ConstantImpl> set : values) {
					if (!Collections.disjoint(set, value)) {
						iter.remove();
						continue;
					}
				}
				values.add(value);
			}
    	}
		return partitions;
	}
	
	public static boolean extractQueries(QueryCommandImpl queryCommand, List<QueryImpl> result) {
		if (queryCommand instanceof SetQueryImpl) {
			SetQueryImpl sq = (SetQueryImpl)queryCommand;
			if (sq.isAll() && sq.getOperation() == Operation.UNION && sq.getOrderBy() == null && sq.getLimit() == null && sq.getWith() == null) {
				if (!extractQueries(sq.getLeftQuery(), result)) {
					return false;
				}
				if (!extractQueries(sq.getRightQuery(), result)) {
					return false;
				}
				return true;
	    	}
			return false;
		}
		result.add((QueryImpl)queryCommand);
		return true;
	}
	
	private static Map<ElementSymbolImpl, Set<ConstantImpl>> extractPartitionInfo(QueryImpl query, List<ElementSymbolImpl> projectedSymbols) {
		List<BaseExpression> projected = query.getSelect().getProjectedSymbols();
		List<CriteriaImpl> crits = CriteriaImpl.separateCriteriaByAnd(query.getCriteria());
		Map<BaseExpression, Set<ConstantImpl>> inMap = new HashMap<BaseExpression, Set<ConstantImpl>>();
		for (CriteriaImpl criteria : crits) {
			if (criteria instanceof CompareCriteriaImpl) {
				CompareCriteriaImpl cc = (CompareCriteriaImpl)criteria;
				if (cc.getOperator() != CompareCriteriaImpl.EQ) {
					continue;
				}
				if (cc.getLeftExpression() instanceof ConstantImpl) {
					inMap.put(cc.getRightExpression(), new HashSet<ConstantImpl>(Arrays.asList((ConstantImpl)cc.getLeftExpression())));
				} else if (cc.getRightExpression() instanceof ConstantImpl) {
					inMap.put(cc.getLeftExpression(), new HashSet<ConstantImpl>(Arrays.asList((ConstantImpl)cc.getRightExpression())));
				}
				continue;
			}
			if (!(criteria instanceof SetCriteriaImpl)) {
				continue;
			}
			SetCriteriaImpl sc = (SetCriteriaImpl)criteria;
			HashSet<ConstantImpl> values = new HashSet<ConstantImpl>();
			boolean allConstants = true;
			for (BaseExpression exp : (Collection<BaseExpression>)sc.getValues()) {
				if (exp instanceof ConstantImpl) {
					values.add((ConstantImpl)exp);
				} else {
					allConstants = false;
					break;
				}
			}
			if (allConstants) {
				inMap.put(sc.getExpression(), values);
			}
		}
		Map<ElementSymbolImpl, Set<ConstantImpl>> result = new HashMap<ElementSymbolImpl, Set<ConstantImpl>>();
		for (int i = 0; i < projected.size(); i++) {
			BaseExpression ex = SymbolMap.getExpression(projected.get(i));
			if (DefaultDataTypeManager.getInstance(ex.getTeiidVersion()).isNonComparable(ex.getType())) {
				continue;
			}
			if (ex instanceof ConstantImpl) {
				result.put(projectedSymbols.get(i), Collections.singleton((ConstantImpl)ex));
			} else {
				Set<ConstantImpl> values = inMap.get(ex);
				if (values != null) {
					result.put(projectedSymbols.get(i), values);
				}
			}
		}
		return result;
	}
	
}
