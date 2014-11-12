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
import org.komodo.spi.query.sql.lang.AlterProcedure;
import org.komodo.spi.query.sql.lang.AlterTrigger;
import org.komodo.spi.query.sql.lang.AlterView;
import org.komodo.spi.query.sql.lang.ArrayTable;
import org.komodo.spi.query.sql.lang.BetweenCriteria;
import org.komodo.spi.query.sql.lang.CompareCriteria;
import org.komodo.spi.query.sql.lang.CompoundCriteria;
import org.komodo.spi.query.sql.lang.Create;
import org.komodo.spi.query.sql.lang.Delete;
import org.komodo.spi.query.sql.lang.Drop;
import org.komodo.spi.query.sql.lang.DynamicCommand;
import org.komodo.spi.query.sql.lang.ExistsCriteria;
import org.komodo.spi.query.sql.lang.ExpressionCriteria;
import org.komodo.spi.query.sql.lang.From;
import org.komodo.spi.query.sql.lang.GroupBy;
import org.komodo.spi.query.sql.lang.Insert;
import org.komodo.spi.query.sql.lang.Into;
import org.komodo.spi.query.sql.lang.IsNullCriteria;
import org.komodo.spi.query.sql.lang.JoinPredicate;
import org.komodo.spi.query.sql.lang.JoinType;
import org.komodo.spi.query.sql.lang.Limit;
import org.komodo.spi.query.sql.lang.MatchCriteria;
import org.komodo.spi.query.sql.lang.NotCriteria;
import org.komodo.spi.query.sql.lang.ObjectTable;
import org.komodo.spi.query.sql.lang.Option;
import org.komodo.spi.query.sql.lang.OrderBy;
import org.komodo.spi.query.sql.lang.OrderByItem;
import org.komodo.spi.query.sql.lang.ProcedureContainer;
import org.komodo.spi.query.sql.lang.Query;
import org.komodo.spi.query.sql.lang.Select;
import org.komodo.spi.query.sql.lang.SetClause;
import org.komodo.spi.query.sql.lang.SetClauseList;
import org.komodo.spi.query.sql.lang.SetCriteria;
import org.komodo.spi.query.sql.lang.SetQuery;
import org.komodo.spi.query.sql.lang.StoredProcedure;
import org.komodo.spi.query.sql.lang.SubqueryCompareCriteria;
import org.komodo.spi.query.sql.lang.SubqueryFromClause;
import org.komodo.spi.query.sql.lang.SubquerySetCriteria;
import org.komodo.spi.query.sql.lang.TextTable;
import org.komodo.spi.query.sql.lang.UnaryFromClause;
import org.komodo.spi.query.sql.lang.Update;
import org.komodo.spi.query.sql.lang.WithQueryCommand;
import org.komodo.spi.query.sql.lang.XMLTable;
import org.komodo.spi.query.sql.proc.AssignmentStatement;
import org.komodo.spi.query.sql.proc.Block;
import org.komodo.spi.query.sql.proc.BranchingStatement;
import org.komodo.spi.query.sql.proc.CommandStatement;
import org.komodo.spi.query.sql.proc.CreateProcedureCommand;
import org.komodo.spi.query.sql.proc.CriteriaSelector;
import org.komodo.spi.query.sql.proc.DeclareStatement;
import org.komodo.spi.query.sql.proc.ExceptionExpression;
import org.komodo.spi.query.sql.proc.HasCriteria;
import org.komodo.spi.query.sql.proc.IfStatement;
import org.komodo.spi.query.sql.proc.LoopStatement;
import org.komodo.spi.query.sql.proc.RaiseStatement;
import org.komodo.spi.query.sql.proc.ReturnStatement;
import org.komodo.spi.query.sql.proc.TranslateCriteria;
import org.komodo.spi.query.sql.proc.TriggerAction;
import org.komodo.spi.query.sql.proc.WhileStatement;
import org.komodo.spi.query.sql.symbol.AggregateSymbol;
import org.komodo.spi.query.sql.symbol.AliasSymbol;
import org.komodo.spi.query.sql.symbol.Array;
import org.komodo.spi.query.sql.symbol.CaseExpression;
import org.komodo.spi.query.sql.symbol.Constant;
import org.komodo.spi.query.sql.symbol.DerivedColumn;
import org.komodo.spi.query.sql.symbol.ElementSymbol;
import org.komodo.spi.query.sql.symbol.ExpressionSymbol;
import org.komodo.spi.query.sql.symbol.Function;
import org.komodo.spi.query.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.symbol.MultipleElementSymbol;
import org.komodo.spi.query.sql.symbol.QueryString;
import org.komodo.spi.query.sql.symbol.Reference;
import org.komodo.spi.query.sql.symbol.ScalarSubquery;
import org.komodo.spi.query.sql.symbol.SearchedCaseExpression;
import org.komodo.spi.query.sql.symbol.TextLine;
import org.komodo.spi.query.sql.symbol.WindowFunction;
import org.komodo.spi.query.sql.symbol.WindowSpecification;
import org.komodo.spi.query.sql.symbol.XMLAttributes;
import org.komodo.spi.query.sql.symbol.XMLElement;
import org.komodo.spi.query.sql.symbol.XMLForest;
import org.komodo.spi.query.sql.symbol.XMLNamespaces;
import org.komodo.spi.query.sql.symbol.XMLParse;
import org.komodo.spi.query.sql.symbol.XMLQuery;
import org.komodo.spi.query.sql.symbol.XMLSerialize;

/**
 * An implementation of a {@link LanguageVisitor language object visitor} that does nothing.
 */
public abstract class AbstractLanguageVisitor implements LanguageVisitor {

    @Override
    public void visit(BetweenCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(CaseExpression obj) {
        // nothing to do
    }

    @Override
    public void visit(CompareCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(CompoundCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(Create obj) {
        // nothing to do
    }

    @Override
    public void visit(Delete obj) {
        // nothing to do
    }

    @Override
    public void visit(ExistsCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(From obj) {
        // nothing to do
    }

    @Override
    public void visit(GroupBy obj) {
        // nothing to do
    }

    @Override
    public void visit(Insert obj) {
        // nothing to do
    }

    @Override
    public void visit(IsNullCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(JoinPredicate obj) {
        // nothing to do
    }

    @Override
    public void visit(JoinType obj) {
        // nothing to do
    }

    @Override
    public void visit(Limit obj) {
        // nothing to do
    }

    @Override
    public void visit(MatchCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(NotCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(Option obj) {
        // nothing to do
    }

    @Override
    public void visit(OrderBy obj) {
        // nothing to do
    }

    @Override
    public void visit(Query obj) {
        // nothing to do
    }

    @Override
    public void visit(SearchedCaseExpression obj) {
        // nothing to do
    }

    @Override
    public void visit(Select obj) {
        // nothing to do
    }

    @Override
    public void visit(SetCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(SetQuery obj) {
        // nothing to do
    }

    @Override
    public void visit(StoredProcedure obj) {
        // nothing to do
    }

    @Override
    public void visit(SubqueryCompareCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(SubqueryFromClause obj) {
        // nothing to do
    }

    @Override
    public void visit(SubquerySetCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(UnaryFromClause obj) {
        // nothing to do
    }

    @Override
    public void visit(Update obj) {
        // nothing to do
    }

    @Override
    public void visit(Into obj) {
        // nothing to do
    }

    @Override
    public void visit(Drop obj) {
        // nothing to do
    }

    @Override
    public void visit(AggregateSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(AliasSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(MultipleElementSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(Constant obj) {
        // nothing to do
    }

    @Override
    public void visit(ElementSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(ExpressionSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(Function obj) {
        // nothing to do
    }

    @Override
    public void visit(GroupSymbol obj) {
        // nothing to do
    }

    @Override
    public void visit(Reference obj) {
        // nothing to do
    }

    @Override
    public void visit(ScalarSubquery obj) {
        // nothing to do
    }

    @Override
    public void visit(AssignmentStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(Block obj) {
        // nothing to do
    }

    @Override
    public void visit(CommandStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(CreateProcedureCommand obj) {
        // nothing to do
    }

    @Override
    public void visit(CriteriaSelector obj) {
        // nothing to do
    }

    @Override
    public void visit(DeclareStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(HasCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(IfStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(RaiseStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(TranslateCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(BranchingStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(WhileStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(LoopStatement obj) {
        // nothing to do
    }

    @Override
    public void visit(DynamicCommand obj) {
        // nothing to do
    }

    @Override
    public void visit(ProcedureContainer obj) {
        // nothing to do
    }

    @Override
    public void visit(SetClauseList obj) {
        // nothing to do
    }

    @Override
    public void visit(SetClause obj) {
        // nothing to do
    }

    @Override
    public void visit(OrderByItem obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLElement obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLAttributes obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLForest obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLNamespaces obj) {
        // nothing to do
    }

    @Override
    public void visit(TextTable obj) {
        // nothing to do
    }

    @Override
    public void visit(TextLine obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLTable obj) {
        // nothing to do
    }

    @Override
    public void visit(DerivedColumn obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLSerialize obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLQuery obj) {
        // nothing to do
    }

    @Override
    public void visit(QueryString obj) {
        // nothing to do
    }

    @Override
    public void visit(XMLParse obj) {
        // nothing to do
    }

    @Override
    public void visit(ExpressionCriteria obj) {
        // nothing to do
    }

    @Override
    public void visit(WithQueryCommand obj) {
        // nothing to do
    }

    @Override
    public void visit(TriggerAction obj) {
        // nothing to do
    }

    @Override
    public void visit(ArrayTable obj) {
        // nothing to do
    }

    @Override
    public void visit(ObjectTable objectTable) {
        // nothing to do
    }

    @Override
    public void visit(AlterView obj) {
        // nothing to do
    }

    @Override
    public void visit(AlterProcedure obj) {
        // nothing to do
    }

    @Override
    public void visit(AlterTrigger obj) {
        // nothing to do
    }

    @Override
    public void visit(WindowFunction windowFunction) {
        // nothing to do
    }

    @Override
    public void visit(Array array) {
        // nothing to do
    }

    @Override
    public void visit(ExceptionExpression exceptionExpression) {
        // nothing to do
    }

    @Override
    public void visit(ReturnStatement returnStatement) {
        // nothing to do
    }

    @Override
    public void visit(WindowSpecification windowSpecification) {
        // nothing to do
    }
}
