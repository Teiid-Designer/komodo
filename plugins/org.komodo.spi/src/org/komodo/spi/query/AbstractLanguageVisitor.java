/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.query;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.IAlterProcedure;
import org.komodo.spi.query.sql.lang.IAlterTrigger;
import org.komodo.spi.query.sql.lang.IAlterView;
import org.komodo.spi.query.sql.lang.IArrayTable;
import org.komodo.spi.query.sql.lang.IBetweenCriteria;
import org.komodo.spi.query.sql.lang.ICompareCriteria;
import org.komodo.spi.query.sql.lang.ICompoundCriteria;
import org.komodo.spi.query.sql.lang.ICreate;
import org.komodo.spi.query.sql.lang.IDelete;
import org.komodo.spi.query.sql.lang.IDrop;
import org.komodo.spi.query.sql.lang.IDynamicCommand;
import org.komodo.spi.query.sql.lang.IExistsCriteria;
import org.komodo.spi.query.sql.lang.IExpressionCriteria;
import org.komodo.spi.query.sql.lang.IFrom;
import org.komodo.spi.query.sql.lang.IGroupBy;
import org.komodo.spi.query.sql.lang.IInsert;
import org.komodo.spi.query.sql.lang.IInto;
import org.komodo.spi.query.sql.lang.IIsNullCriteria;
import org.komodo.spi.query.sql.lang.IJoinPredicate;
import org.komodo.spi.query.sql.lang.IJoinType;
import org.komodo.spi.query.sql.lang.ILimit;
import org.komodo.spi.query.sql.lang.IMatchCriteria;
import org.komodo.spi.query.sql.lang.INotCriteria;
import org.komodo.spi.query.sql.lang.IObjectTable;
import org.komodo.spi.query.sql.lang.IOption;
import org.komodo.spi.query.sql.lang.IOrderBy;
import org.komodo.spi.query.sql.lang.IOrderByItem;
import org.komodo.spi.query.sql.lang.IProcedureContainer;
import org.komodo.spi.query.sql.lang.IQuery;
import org.komodo.spi.query.sql.lang.ISelect;
import org.komodo.spi.query.sql.lang.ISetClause;
import org.komodo.spi.query.sql.lang.ISetClauseList;
import org.komodo.spi.query.sql.lang.ISetCriteria;
import org.komodo.spi.query.sql.lang.ISetQuery;
import org.komodo.spi.query.sql.lang.IStoredProcedure;
import org.komodo.spi.query.sql.lang.ISubqueryCompareCriteria;
import org.komodo.spi.query.sql.lang.ISubqueryFromClause;
import org.komodo.spi.query.sql.lang.ISubquerySetCriteria;
import org.komodo.spi.query.sql.lang.ITextTable;
import org.komodo.spi.query.sql.lang.IUnaryFromClause;
import org.komodo.spi.query.sql.lang.IUpdate;
import org.komodo.spi.query.sql.lang.IWithQueryCommand;
import org.komodo.spi.query.sql.lang.IXMLTable;
import org.komodo.spi.query.sql.proc.IAssignmentStatement;
import org.komodo.spi.query.sql.proc.IBlock;
import org.komodo.spi.query.sql.proc.IBranchingStatement;
import org.komodo.spi.query.sql.proc.ICommandStatement;
import org.komodo.spi.query.sql.proc.ICreateProcedureCommand;
import org.komodo.spi.query.sql.proc.ICriteriaSelector;
import org.komodo.spi.query.sql.proc.IDeclareStatement;
import org.komodo.spi.query.sql.proc.IExceptionExpression;
import org.komodo.spi.query.sql.proc.IHasCriteria;
import org.komodo.spi.query.sql.proc.IIfStatement;
import org.komodo.spi.query.sql.proc.ILoopStatement;
import org.komodo.spi.query.sql.proc.IRaiseStatement;
import org.komodo.spi.query.sql.proc.IReturnStatement;
import org.komodo.spi.query.sql.proc.ITranslateCriteria;
import org.komodo.spi.query.sql.proc.ITriggerAction;
import org.komodo.spi.query.sql.proc.IWhileStatement;
import org.komodo.spi.query.sql.symbol.IAggregateSymbol;
import org.komodo.spi.query.sql.symbol.IAliasSymbol;
import org.komodo.spi.query.sql.symbol.IArray;
import org.komodo.spi.query.sql.symbol.ICaseExpression;
import org.komodo.spi.query.sql.symbol.IConstant;
import org.komodo.spi.query.sql.symbol.IDerivedColumn;
import org.komodo.spi.query.sql.symbol.IElementSymbol;
import org.komodo.spi.query.sql.symbol.IExpressionSymbol;
import org.komodo.spi.query.sql.symbol.IFunction;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;
import org.komodo.spi.query.sql.symbol.IMultipleElementSymbol;
import org.komodo.spi.query.sql.symbol.IQueryString;
import org.komodo.spi.query.sql.symbol.IReference;
import org.komodo.spi.query.sql.symbol.IScalarSubquery;
import org.komodo.spi.query.sql.symbol.ISearchedCaseExpression;
import org.komodo.spi.query.sql.symbol.ITextLine;
import org.komodo.spi.query.sql.symbol.IWindowFunction;
import org.komodo.spi.query.sql.symbol.IWindowSpecification;
import org.komodo.spi.query.sql.symbol.IXMLAttributes;
import org.komodo.spi.query.sql.symbol.IXMLElement;
import org.komodo.spi.query.sql.symbol.IXMLForest;
import org.komodo.spi.query.sql.symbol.IXMLNamespaces;
import org.komodo.spi.query.sql.symbol.IXMLParse;
import org.komodo.spi.query.sql.symbol.IXMLQuery;
import org.komodo.spi.query.sql.symbol.IXMLSerialize;

/**
 *
 */
public abstract class AbstractLanguageVisitor implements ILanguageVisitor {

    @Override
    public void visit(IBetweenCriteria obj) {
    }

    @Override
    public void visit(ICaseExpression obj) {
    }

    @Override
    public void visit(ICompareCriteria obj) {
    }

    @Override
    public void visit(ICompoundCriteria obj) {
    }

    @Override
    public void visit(ICreate obj) {
    }

    @Override
    public void visit(IDelete obj) {
    }

    @Override
    public void visit(IExistsCriteria obj) {
    }

    @Override
    public void visit(IFrom obj) {
    }

    @Override
    public void visit(IGroupBy obj) {
    }

    @Override
    public void visit(IInsert obj) {
    }

    @Override
    public void visit(IIsNullCriteria obj) {
    }

    @Override
    public void visit(IJoinPredicate obj) {
    }

    @Override
    public void visit(IJoinType obj) {
    }

    @Override
    public void visit(ILimit obj) {
    }

    @Override
    public void visit(IMatchCriteria obj) {
    }

    @Override
    public void visit(INotCriteria obj) {
    }

    @Override
    public void visit(IOption obj) {
    }

    @Override
    public void visit(IOrderBy obj) {
    }

    @Override
    public void visit(IQuery obj) {
    }

    @Override
    public void visit(ISearchedCaseExpression obj) {
    }

    @Override
    public void visit(ISelect obj) {
    }

    @Override
    public void visit(ISetCriteria obj) {
    }

    @Override
    public void visit(ISetQuery obj) {
    }

    @Override
    public void visit(IStoredProcedure obj) {
    }

    @Override
    public void visit(ISubqueryCompareCriteria obj) {
    }

    @Override
    public void visit(ISubqueryFromClause obj) {
    }

    @Override
    public void visit(ISubquerySetCriteria obj) {
    }

    @Override
    public void visit(IUnaryFromClause obj) {
    }

    @Override
    public void visit(IUpdate obj) {
    }

    @Override
    public void visit(IInto obj) {
    }

    @Override
    public void visit(IDrop obj) {
    }

    @Override
    public void visit(IAggregateSymbol obj) {
    }

    @Override
    public void visit(IAliasSymbol obj) {
    }

    @Override
    public void visit(IMultipleElementSymbol obj) {
    }

    @Override
    public void visit(IConstant obj) {
    }

    @Override
    public void visit(IElementSymbol obj) {
    }

    @Override
    public void visit(IExpressionSymbol obj) {
    }

    @Override
    public void visit(IFunction obj) {
    }

    @Override
    public void visit(IGroupSymbol obj) {
    }

    @Override
    public void visit(IReference obj) {
    }

    @Override
    public void visit(IScalarSubquery obj) {
    }

    @Override
    public void visit(IAssignmentStatement obj) {
    }

    @Override
    public void visit(IBlock obj) {
    }

    @Override
    public void visit(ICommandStatement obj) {
    }

    @Override
    public void visit(ICreateProcedureCommand obj) {
    }

    @Override
    public void visit(ICriteriaSelector obj) {
    }

    @Override
    public void visit(IDeclareStatement obj) {
    }

    @Override
    public void visit(IHasCriteria obj) {
    }

    @Override
    public void visit(IIfStatement obj) {
    }

    @Override
    public void visit(IRaiseStatement obj) {
    }

    @Override
    public void visit(ITranslateCriteria obj) {
    }

    @Override
    public void visit(IBranchingStatement obj) {
    }

    @Override
    public void visit(IWhileStatement obj) {
    }

    @Override
    public void visit(ILoopStatement obj) {
    }

    @Override
    public void visit(IDynamicCommand obj) {
    }

    @Override
    public void visit(IProcedureContainer obj) {
    }

    @Override
    public void visit(ISetClauseList obj) {
    }

    @Override
    public void visit(ISetClause obj) {
    }

    @Override
    public void visit(IOrderByItem obj) {
    }

    @Override
    public void visit(IXMLElement obj) {
    }

    @Override
    public void visit(IXMLAttributes obj) {
    }

    @Override
    public void visit(IXMLForest obj) {
    }

    @Override
    public void visit(IXMLNamespaces obj) {
    }

    @Override
    public void visit(ITextTable obj) {
    }

    @Override
    public void visit(ITextLine obj) {
    }

    @Override
    public void visit(IXMLTable obj) {
    }

    @Override
    public void visit(IDerivedColumn obj) {
    }

    @Override
    public void visit(IXMLSerialize obj) {
    }

    @Override
    public void visit(IXMLQuery obj) {
    }

    @Override
    public void visit(IQueryString obj) {
    }

    @Override
    public void visit(IXMLParse obj) {
    }

    @Override
    public void visit(IExpressionCriteria obj) {
    }

    @Override
    public void visit(IWithQueryCommand obj) {
    }

    @Override
    public void visit(ITriggerAction obj) {
    }

    @Override
    public void visit(IArrayTable obj) {
    }

    @Override
    public void visit(IObjectTable objectTable) {
    }

    @Override
    public void visit(IAlterView obj) {
    }

    @Override
    public void visit(IAlterProcedure obj) {
    }

    @Override
    public void visit(IAlterTrigger obj) {
    }

    @Override
    public void visit(IWindowFunction windowFunction) {
    }

    @Override
    public void visit(IArray array) {
    }

    @Override
    public void visit(IExceptionExpression exceptionExpression) {
    }

    @Override
    public void visit(IReturnStatement returnStatement) {
    }

    @Override
    public void visit(IWindowSpecification windowSpecification) {
    }
}
