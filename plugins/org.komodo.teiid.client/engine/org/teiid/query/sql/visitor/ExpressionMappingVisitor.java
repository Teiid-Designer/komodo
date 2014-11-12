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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.ExpressionCriteriaImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.LimitImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.ObjectColumnImpl;
import org.teiid.query.sql.lang.ObjectTableImpl;
import org.teiid.query.sql.lang.OrderByItemImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.XMLColumnImpl;
import org.teiid.query.sql.lang.XMLTableImpl;
import org.teiid.query.sql.navigator.PreOrPostOrderNavigator;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.ExceptionExpressionImpl;
import org.teiid.query.sql.proc.ReturnStatementImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.ArraySymbolImpl;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.QueryStringImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.symbol.WindowSpecificationImpl;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;


/**
 * It is important to use a Post Navigator with this class, 
 * otherwise a replacement containing itself will not work
 */
public class ExpressionMappingVisitor extends TCLanguageVisitorImpl {

    private Map symbolMap;
    private boolean clone = true;
    private boolean elementSymbolsOnly;

    /**
     * Constructor for ExpressionMappingVisitor.
     * @param teiidVersion
     * @param symbolMap Map of ElementSymbol to Expression
     */
    public ExpressionMappingVisitor(TeiidVersion teiidVersion, Map symbolMap) {
        super(teiidVersion);
        this.symbolMap = symbolMap;
    }

    /**
     * @param teiidVersion
     * @param symbolMap
     * @param clone
     */
    public ExpressionMappingVisitor(DefaultTeiidVersion teiidVersion, Map symbolMap, boolean clone) {
        super(teiidVersion);
        this.symbolMap = symbolMap;
        this.clone = clone;
    }
        
    protected boolean createAliases() {
    	return true;
    }
    
    @Override
    public void visit(SelectImpl obj) {
    	List<BaseExpression> symbols = obj.getSymbols();
    	for (int i = 0; i < symbols.size(); i++) {
            BaseExpression symbol = symbols.get(i);
            
            if (symbol instanceof MultipleElementSymbolImpl) {
            	continue;
            }
            
            BaseExpression replacmentSymbol = replaceSymbol(symbol, true);
            
            symbols.set(i, replacmentSymbol);
        }
    }
    
    /**
     * @return true if clone, false otherwise
     */
    public boolean isClone() {
		return clone;
	}
    
    /**
     * @param clone
     */
    public void setClone(boolean clone) {
		this.clone = clone;
	}
    
    @Override
    public void visit(DerivedColumnImpl obj) {
    	BaseExpression original = obj.getExpression();
    	obj.setExpression(replaceExpression(original));
    	if (obj.isPropagateName() && obj.getAlias() == null && !(obj.getExpression() instanceof ElementSymbolImpl) && original instanceof ElementSymbolImpl) {
    		obj.setAlias(((ElementSymbolImpl)original).getShortName());
    	}
    }
    
    @Override
    public void visit(XMLTableImpl obj) {
    	for (XMLColumnImpl col : obj.getColumns()) {
    		BaseExpression exp = col.getDefaultExpression();
    		if (exp != null) {
    			col.setDefaultExpression(replaceExpression(exp));
    		}
		}
    }
    
    @Override
    public void visit(ObjectTableImpl obj) {
    	for (ObjectColumnImpl col : obj.getColumns()) {
    		BaseExpression exp = col.getDefaultExpression();
    		if (exp != null) {
    			col.setDefaultExpression(replaceExpression(exp));
    		}
		}
    }
    
    @Override
    public void visit(XMLSerializeImpl obj) {
    	obj.setExpression(replaceExpression(obj.getExpression()));
    }
    
    @Override
    public void visit(XMLParseImpl obj) {
    	obj.setExpression(replaceExpression(obj.getExpression()));
    }
    
	private BaseExpression replaceSymbol(BaseExpression ses,
			boolean alias) {
		BaseExpression expr = ses;
		String name = SymbolImpl.getShortName(ses);
		if (ses instanceof ExpressionSymbolImpl) {
		    expr = ((ExpressionSymbolImpl)ses).getExpression();
		}
		
		BaseExpression replacementSymbol = replaceExpression(expr);
		
		if (!(replacementSymbol instanceof SymbolImpl)) {
		    replacementSymbol = getTeiidParser().createASTNode(ASTNodes.EXPRESSION_SYMBOL);
		    ((ExpressionSymbolImpl) replacementSymbol).setName(name);
		    ((ExpressionSymbolImpl) replacementSymbol).setExpression(replacementSymbol);
		} else if (alias && createAliases() && !SymbolImpl.getShortName(replacementSymbol).equals(name)) {
		    AliasSymbolImpl aliasSymbol = getTeiidParser().createASTNode(ASTNodes.ALIAS_SYMBOL);
		    aliasSymbol.setName(name);
            aliasSymbol.setSymbol(replacementSymbol);
            replacementSymbol = aliasSymbol;
		}
		return replacementSymbol;
	}
    
    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.symbol.AliasSymbolImpl)
     */
    @Override
    public void visit(AliasSymbolImpl obj) {
        BaseExpression replacement = replaceExpression(obj.getSymbol());
        obj.setSymbol(replacement);
    }
    
    @Override
    public void visit(ExpressionSymbolImpl expr) {
        expr.setExpression(replaceExpression(expr.getExpression()));
    }
    
    /**
     * @see TCLanguageVisitorImpl#visit(BetweenCriteriaImpl)
     */
    @Override
    public void visit(BetweenCriteriaImpl obj) {
        obj.setExpression( replaceExpression(obj.getExpression()) );
        obj.setLowerExpression( replaceExpression(obj.getLowerExpression()) );
        obj.setUpperExpression( replaceExpression(obj.getUpperExpression()) );
    }
    
    @Override
    public void visit(CaseExpressionImpl obj) {
        obj.setExpression(replaceExpression(obj.getExpression()));
        final int whenCount = obj.getWhenCount();
        ArrayList whens = new ArrayList(whenCount);
        ArrayList thens = new ArrayList(whenCount);
        for (int i = 0; i < whenCount; i++) {
            whens.add(replaceExpression(obj.getWhenExpression(i)));
            thens.add(replaceExpression(obj.getThenExpression(i)));
        }
        obj.setWhen(whens, thens);
        if (obj.getElseExpression() != null) {
            obj.setElseExpression(replaceExpression(obj.getElseExpression()));
        }
    }

    /**
     * @see TCLanguageVisitorImpl#visit(CompareCriteriaImpl)
     */
    @Override
    public void visit(CompareCriteriaImpl obj) {
        obj.setLeftExpression( replaceExpression(obj.getLeftExpression()) );
        obj.setRightExpression( replaceExpression(obj.getRightExpression()) );
    }

    /**
     * @see TCLanguageVisitorImpl#visit(FunctionImpl)
     */
    @Override
    public void visit(FunctionImpl obj) {
        BaseExpression[] args = obj.getArgs();
        if(args != null && args.length > 0) {
            for(int i=0; i<args.length; i++) {
                args[i] = replaceExpression(args[i]);
            }
        }
    }

    /**
     * @see TCLanguageVisitorImpl#visit(IsNullCriteriaImpl)
     */
    @Override
    public void visit(IsNullCriteriaImpl obj) {
        obj.setExpression( replaceExpression(obj.getExpression()) );
    }

    /**
     * @see TCLanguageVisitorImpl#visit(MatchCriteriaImpl)
     */
    @Override
    public void visit(MatchCriteriaImpl obj) {
        obj.setLeftExpression( replaceExpression(obj.getLeftExpression()) );
        obj.setRightExpression( replaceExpression(obj.getRightExpression()) );
    }

    @Override
    public void visit(SearchedCaseExpressionImpl obj) {
        int whenCount = obj.getWhenCount();
        ArrayList<BaseExpression> thens = new ArrayList<BaseExpression>(whenCount);
        for (int i = 0; i < whenCount; i++) {
            thens.add(replaceExpression(obj.getThenExpression(i)));
        }
        obj.setWhen(obj.getWhen(), thens);
        if (obj.getElseExpression() != null) {
            obj.setElseExpression(replaceExpression(obj.getElseExpression()));
        }
    }

    /**
     * @see TCLanguageVisitorImpl#visit(SetCriteriaImpl)
     */
    @Override
    public void visit(SetCriteriaImpl obj) {
        obj.setExpression( replaceExpression(obj.getExpression()) );
        
        if (obj.isAllConstants()) {
        	return;
        }
        
        Collection newValues = new ArrayList(obj.getValues().size());        
        Iterator valueIter = obj.getValues().iterator();
        while(valueIter.hasNext()) {
            newValues.add( replaceExpression( (BaseExpression) valueIter.next() ) );
        }
        
        obj.setValues(newValues);                    
    }

    /**
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl)
     */
    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
        obj.setLeftExpression( replaceExpression(obj.getLeftExpression()) );
    }
    
    /**
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.SubquerySetCriteriaImpl)
     */
    @Override
    public void visit(SubquerySetCriteriaImpl obj) {
        obj.setExpression( replaceExpression(obj.getExpression()) );
    }    
    
    /**
     * Find an expression in the symbol map that could replace
     * the given expression
     *
     * @param element
     * @return new expression
     */
    public BaseExpression replaceExpression(BaseExpression element) {
    	if (elementSymbolsOnly && !(element instanceof ElementSymbolImpl)) {
    		return element;
    	}
        BaseExpression mapped = (BaseExpression) this.symbolMap.get(element);
        if(mapped != null) {
        	if (clone) {
        		return mapped.clone();
        	}
        	return mapped;
        }
        return element;    
    }
    
    @Override
    public void visit(StoredProcedureImpl obj) {
    	for (Iterator<SPParameterImpl> paramIter = obj.getInputParameters().iterator(); paramIter.hasNext();) {
			SPParameterImpl param = paramIter.next();
            BaseExpression expr = param.getExpression();
            param.setExpression(replaceExpression(expr));
        }
    }
    
    @Override
    public void visit(BaseAggregateSymbol obj) {
    	visit((FunctionImpl)obj);
    	if (obj.getCondition() != null) { 
    		obj.setCondition(replaceExpression(obj.getCondition()));
    	}
    }
    
    /**
     * Swap each ElementSymbol in GroupBy (other symbols are ignored).
     * @param obj Object to remap
     */
    @Override
    public void visit(GroupByImpl obj) {        
    	List<BaseExpression> symbols = obj.getSymbols();
		for (int i = 0; i < symbols.size(); i++) {
            BaseExpression symbol = symbols.get(i);
            symbols.set(i, replaceExpression(symbol));
        }
    }
    
    @Override
    public void visit(OrderByItemImpl obj) {
    	obj.setSymbol(replaceSymbol(obj.getSymbol(), obj.getExpressionPosition() != -1));
    }
    
    @Override
    public void visit(LimitImpl obj) {
        if (obj.getOffset() != null) {
            obj.setOffset(replaceExpression(obj.getOffset()));
        }
        obj.setRowLimit(replaceExpression(obj.getRowLimit()));
    }
       
    @Override
    public void visit(DynamicCommandImpl obj) {
        obj.setSql(replaceExpression(obj.getSql()));
        if (obj.getUsing() != null) {
	        for (SetClauseImpl clause : obj.getUsing().getClauses()) {
				visit(clause);
			}
        }
    }
    
    @Override
    public void visit(SetClauseImpl obj) {
    	obj.setValue(replaceExpression(obj.getValue()));
    }
    
    @Override
    public void visit(QueryStringImpl obj) {
    	obj.setPath(replaceExpression(obj.getPath()));
    }
    
    @Override
    public void visit(ExpressionCriteriaImpl obj) {
    	obj.setExpression(replaceExpression(obj.getExpression()));
    }
    
    /**
     * The object is modified in place, so is not returned.
     * @param obj Language object
     * @param exprMap Expression map, Expression to Expression
     */
    public static void mapExpressions(BaseLanguageObject obj, Map<? extends BaseExpression, ? extends BaseExpression> exprMap) {
        mapExpressions(obj, exprMap, false);
    }
    
    /**
     * The object is modified in place, so is not returned.
     * @param obj Language object
     * @param exprMap Expression map, Expression to Expression
     */
    public static void mapExpressions(BaseLanguageObject obj, Map<? extends BaseExpression, ? extends BaseExpression> exprMap, boolean deep) {
        if(obj == null || exprMap == null || exprMap.isEmpty()) { 
            return;
        }
        TeiidVersion teiidVersion = obj.getTeiidVersion();
        final ExpressionMappingVisitor visitor = new ExpressionMappingVisitor(teiidVersion, exprMap);
        visitor.elementSymbolsOnly = true;
        boolean preOrder = true;
        boolean useReverseMapping = true;
        for (Map.Entry<? extends BaseExpression, ? extends BaseExpression> entry : exprMap.entrySet()) {
        	if (!(entry.getKey() instanceof ElementSymbolImpl)) {
        		visitor.elementSymbolsOnly = false;
        		break;
        	}
		}
        if (!visitor.elementSymbolsOnly) {
        	for (Map.Entry<? extends BaseExpression, ? extends BaseExpression> entry : exprMap.entrySet()) {
            	if (!(entry.getValue() instanceof ElementSymbolImpl)) {
            		useReverseMapping = !Collections.disjoint(GroupsUsedByElementsVisitorImpl.getGroups(exprMap.keySet()),
                    		GroupsUsedByElementsVisitorImpl.getGroups(exprMap.values()));
            		break;
            	}
    		}
        } else {
        	preOrder = false;
        	useReverseMapping = false;
        }
        
        if (useReverseMapping) {
	        final Set<BaseExpression> reverseSet = new HashSet<BaseExpression>(exprMap.values());
	        PreOrPostOrderNavigator pon = new PreOrPostOrderNavigator(visitor, PreOrPostOrderNavigator.PRE_ORDER, deep) {
	        	@Override
	        	protected void visitNode(BaseLanguageObject obj) {
	        		if (!(obj instanceof BaseExpression) || !reverseSet.contains(obj)) {
	            		super.visitNode(obj);
	        		}
	        	}
	        };
	        obj.acceptVisitor(pon);
        } else {
        	PreOrPostOrderNavigator.doVisit(obj, visitor, preOrder, deep);
        }
    }
    
    protected void setVariableValues(Map variableValues) {
        this.symbolMap = variableValues;
    }

    protected Map getVariableValues() {
        return symbolMap;
    }    
    
    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.proc.AssignmentStatementImpl)
     *
     */
    @Override
    public void visit(AssignmentStatementImpl obj) {
        obj.setExpression(replaceExpression(obj.getExpression()));
    }
    
    /** 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.InsertImpl)
     *
     */
    @Override
    public void visit(InsertImpl obj) {
        for (int i = 0; i < obj.getValues().size(); i++) {
            obj.getValues().set(i, replaceExpression(obj.getValues().get(i)));
        }
    }
    
    @Override
    public void visit(XMLElementImpl obj) {
    	for (int i = 0; i < obj.getContent().size(); i++) {
    		obj.getContent().set(i, replaceExpression(obj.getContent().get(i)));
    	}
    }
    
    @Override
    public void visit(WindowSpecificationImpl windowSpecification) {
    	if (windowSpecification.getPartition() == null) {
    		return;
    	}
    	List<BaseExpression> partition = windowSpecification.getPartition();
		for (int i = 0; i < partition.size(); i++) {
    		partition.set(i, replaceExpression(partition.get(i)));
    	}
    }

    @Override
    public void visit(ArraySymbolImpl array) {
        List<BaseExpression> exprs = array.getExpressions();
        for (int i = 0; i < exprs.size(); i++) {
            exprs.set(i, replaceExpression(exprs.get(i)));
        }
    }

    @Override
    public void visit(ExceptionExpressionImpl exceptionExpression) {
    	if (exceptionExpression.getMessage() != null) {
    		exceptionExpression.setMessage(replaceExpression(exceptionExpression.getMessage()));
    	}
    	if (exceptionExpression.getSqlState() != null) {
    		exceptionExpression.setSqlState(replaceExpression(exceptionExpression.getSqlState()));
    	}
    	if (exceptionExpression.getErrorCode() != null) {
    		exceptionExpression.setErrorCode(replaceExpression(exceptionExpression.getErrorCode()));
    	}
    	if (exceptionExpression.getParent() != null) {
    		exceptionExpression.setParent(replaceExpression(exceptionExpression.getParent()));
    	}
    }
    
    @Override
    public void visit(ReturnStatementImpl obj) {
    	if (obj.getExpression() != null) {
    		obj.setExpression(replaceExpression(obj.getExpression()));
    	}
    }
    
}
