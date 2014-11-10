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
package org.komodo.spi.query.sql;

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
public interface LanguageVisitor {
     
    // Visitor methods for language objects
    void visit(IBetweenCriteria obj);
    void visit(ICaseExpression obj);
    void visit(ICompareCriteria obj);
    void visit(ICompoundCriteria obj);
    void visit(ICreate obj);
    void visit(IDelete obj);
    void visit(IExistsCriteria obj);
    void visit(IFrom obj);
    void visit(IGroupBy obj);
    void visit(IInsert obj);
    void visit(IIsNullCriteria obj);
    void visit(IJoinPredicate obj);
    void visit(IJoinType obj);
    void visit(ILimit obj);
    void visit(IMatchCriteria obj);
    void visit(INotCriteria obj);
    void visit(IOption obj);
    void visit(IOrderBy obj);
    void visit(IQuery obj);
    void visit(ISearchedCaseExpression obj);
    void visit(ISelect obj);
    void visit(ISetCriteria obj);
    void visit(ISetQuery obj);
    void visit(IStoredProcedure obj);
    void visit(ISubqueryCompareCriteria obj);
    void visit(ISubqueryFromClause obj);
    void visit(ISubquerySetCriteria obj);
    void visit(IUnaryFromClause obj);
    void visit(IUpdate obj);
    void visit(IInto obj);
    void visit(IDrop obj);

    // Visitor methods for symbol objects
    void visit(IAggregateSymbol obj);
    void visit(IAliasSymbol obj);
    void visit(IArray obj);
    void visit(IMultipleElementSymbol obj);
    void visit(IConstant obj);
    void visit(IElementSymbol obj);
    void visit(IExpressionSymbol obj);
    void visit(IFunction obj);
    void visit(IGroupSymbol obj);
    void visit(IReference obj);
    void visit(IScalarSubquery obj);
    
    // Visitor methods for procedure language objects    
    void visit(IAssignmentStatement obj);
    void visit(IBlock obj);
    void visit(ICommandStatement obj);
    void visit(ICreateProcedureCommand obj);
    void visit(ICriteriaSelector obj);
    void visit(IDeclareStatement obj);
    void visit(IHasCriteria obj);
    void visit(IIfStatement obj);
    void visit(IRaiseStatement obj);
    void visit(ITranslateCriteria obj);
    void visit(IBranchingStatement obj);
    void visit(IWhileStatement obj);
    void visit(ILoopStatement obj);
    void visit(IDynamicCommand obj);
    void visit(ISetClauseList obj);
    void visit(ISetClause obj);
    void visit(IOrderByItem obj);
    void visit(IXMLElement obj);
    void visit(IXMLAttributes obj);
    void visit(IXMLForest obj);
    void visit(IXMLNamespaces obj);
    void visit(ITextTable obj);
    void visit(ITextLine obj);
    void visit(IXMLTable obj);
    void visit(IDerivedColumn obj);
    void visit(IXMLSerialize obj);
    void visit(IXMLQuery obj);
    void visit(IQueryString obj);
    void visit(IXMLParse obj);
    void visit(IExpressionCriteria obj);
    void visit(IWithQueryCommand obj);
    void visit(ITriggerAction obj);
    void visit(IArrayTable obj);
    void visit(IProcedureContainer obj);

    void visit(IAlterView obj);
    void visit(IAlterProcedure obj);
    void visit(IAlterTrigger obj);

    void visit(IWindowFunction windowFunction);

    void visit(IObjectTable objectTable);

    void visit(IExceptionExpression obj);

    void visit(IReturnStatement obj);

    void visit(IWindowSpecification obj);

}
