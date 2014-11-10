/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.spi.query;

import org.komodo.spi.query.sql.LanguageVisitor;
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
 * An implementation of a {@link LanguageVisitor language object visitor} that does nothing.
 */
public abstract class AbstractLanguageVisitor implements LanguageVisitor {

    @Override
    public void visit(IBetweenCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(ICaseExpression obj) {
        // nothing to do
    }

    @Override
    public void visit(ICompareCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(ICompoundCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(ICreate obj) {
        // nothing to do
    }

    @Override
    public void visit(IDelete obj) {
        // nothing to do
    }

    @Override
    public void visit(IExistsCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IFrom obj) {
        // nothing to do
    }

    @Override
    public void visit(IGroupBy obj) {
        // nothing to do
    }

    @Override
    public void visit(IInsert obj) {
        // nothing to do
    }

    @Override
    public void visit(IIsNullCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IJoinPredicate obj) {
        // nothing to do
    }

    @Override
    public void visit(IJoinType obj) {
        // nothing to do
    }

    @Override
    public void visit(ILimit obj) {
        // nothing to do
    }

    @Override
    public void visit(IMatchCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(INotCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IOption obj) {
        // nothing to do
    }

    @Override
    public void visit(IOrderBy obj) {
        // nothing to do
    }

    @Override
    public void visit(IQuery obj) {
        // nothing to do
    }

    @Override
    public void visit(ISearchedCaseExpression obj) {
        // nothing to do
    }

    @Override
    public void visit(ISelect obj) {
        // nothing to do
    }

    @Override
    public void visit(ISetCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(ISetQuery obj) {
        // nothing to do
    }

    @Override
    public void visit(IStoredProcedure obj) {
        // nothing to do
    }

    @Override
    public void visit(ISubqueryCompareCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(ISubqueryFromClause obj) {
        // nothing to do
    }

    @Override
    public void visit(ISubquerySetCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IUnaryFromClause obj) {
        // nothing to do
    }

    @Override
    public void visit(IUpdate obj) {
        // nothing to do
    }

    @Override
    public void visit(IInto obj) {
        // nothing to do
    }

    @Override
    public void visit(IDrop obj) {
        // nothing to do
    }

    @Override
    public void visit(IAggregateSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(IAliasSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(IMultipleElementSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(IConstant obj) {
        // nothing to do
    }

    @Override
    public void visit(IElementSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(IExpressionSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(IFunction obj) {
        // nothing to do
    }

    @Override
    public void visit(IGroupSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(IReference obj) {
        // nothing to do
    }

    @Override
    public void visit(IScalarSubquery obj) {
        // nothing to do
    }

    @Override
    public void visit(IAssignmentStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(IBlock obj) {
        // nothing to do
    }

    @Override
    public void visit(ICommandStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(ICreateProcedureCommand obj) {
        // nothing to do
    }

    @Override
    public void visit(ICriteriaSelector obj) {
        // nothing to do
    }

    @Override
    public void visit(IDeclareStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(IHasCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IIfStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(IRaiseStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(ITranslateCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IBranchingStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(IWhileStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(ILoopStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(IDynamicCommand obj) {
        // nothing to do
    }

    @Override
    public void visit(IProcedureContainer obj) {
        // nothing to do
    }

    @Override
    public void visit(ISetClauseList obj) {
        // nothing to do
    }

    @Override
    public void visit(ISetClause obj) {
        // nothing to do
    }

    @Override
    public void visit(IOrderByItem obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLElement obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLAttributes obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLForest obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLNamespaces obj) {
        // nothing to do
    }

    @Override
    public void visit(ITextTable obj) {
        // nothing to do
    }

    @Override
    public void visit(ITextLine obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLTable obj) {
        // nothing to do
    }

    @Override
    public void visit(IDerivedColumn obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLSerialize obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLQuery obj) {
        // nothing to do
    }

    @Override
    public void visit(IQueryString obj) {
        // nothing to do
    }

    @Override
    public void visit(IXMLParse obj) {
        // nothing to do
    }

    @Override
    public void visit(IExpressionCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IWithQueryCommand obj) {
        // nothing to do
    }

    @Override
    public void visit(ITriggerAction obj) {
        // nothing to do
    }

    @Override
    public void visit(IArrayTable obj) {
        // nothing to do
    }

    @Override
    public void visit(IObjectTable objectTable) {
        // nothing to do
    }

    @Override
    public void visit(IAlterView obj) {
        // nothing to do
    }

    @Override
    public void visit(IAlterProcedure obj) {
        // nothing to do
    }

    @Override
    public void visit(IAlterTrigger obj) {
        // nothing to do
    }

    @Override
    public void visit(IWindowFunction windowFunction) {
        // nothing to do
    }

    @Override
    public void visit(IArray array) {
        // nothing to do
    }

    @Override
    public void visit(IExceptionExpression exceptionExpression) {
        // nothing to do
    }

    @Override
    public void visit(IReturnStatement returnStatement) {
        // nothing to do
    }

    @Override
    public void visit(IWindowSpecification windowSpecification) {
        // nothing to do
    }
}
