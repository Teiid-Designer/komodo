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
 *
 */
public interface LanguageVisitor {
     
    // Visitor methods for language objects
    void visit(BetweenCriteria obj);
    void visit(CaseExpression obj);
    void visit(CompareCriteria obj);
    void visit(CompoundCriteria obj);
    void visit(Create obj);
    void visit(Delete obj);
    void visit(ExistsCriteria obj);
    void visit(From obj);
    void visit(GroupBy obj);
    void visit(Insert obj);
    void visit(IsNullCriteria obj);
    void visit(JoinPredicate obj);
    void visit(JoinType obj);
    void visit(Limit obj);
    void visit(MatchCriteria obj);
    void visit(NotCriteria obj);
    void visit(Option obj);
    void visit(OrderBy obj);
    void visit(Query obj);
    void visit(SearchedCaseExpression obj);
    void visit(Select obj);
    void visit(SetCriteria obj);
    void visit(SetQuery obj);
    void visit(StoredProcedure obj);
    void visit(SubqueryCompareCriteria obj);
    void visit(SubqueryFromClause obj);
    void visit(SubquerySetCriteria obj);
    void visit(UnaryFromClause obj);
    void visit(Update obj);
    void visit(Into obj);
    void visit(Drop obj);

    // Visitor methods for symbol objects
    void visit(AggregateSymbol obj);
    void visit(AliasSymbol obj);
    void visit(Array obj);
    void visit(MultipleElementSymbol obj);
    void visit(Constant obj);
    void visit(ElementSymbol obj);
    void visit(ExpressionSymbol obj);
    void visit(Function obj);
    void visit(GroupSymbol obj);
    void visit(Reference obj);
    void visit(ScalarSubquery obj);
    
    // Visitor methods for procedure language objects    
    void visit(AssignmentStatement obj);
    void visit(Block obj);
    void visit(CommandStatement obj);
    void visit(CreateProcedureCommand obj);
    void visit(CriteriaSelector obj);
    void visit(DeclareStatement obj);
    void visit(HasCriteria obj);
    void visit(IfStatement obj);
    void visit(RaiseStatement obj);
    void visit(TranslateCriteria obj);
    void visit(BranchingStatement obj);
    void visit(WhileStatement obj);
    void visit(LoopStatement obj);
    void visit(DynamicCommand obj);
    void visit(SetClauseList obj);
    void visit(SetClause obj);
    void visit(OrderByItem obj);
    void visit(XMLElement obj);
    void visit(XMLAttributes obj);
    void visit(XMLForest obj);
    void visit(XMLNamespaces obj);
    void visit(TextTable obj);
    void visit(TextLine obj);
    void visit(XMLTable obj);
    void visit(DerivedColumn obj);
    void visit(XMLSerialize obj);
    void visit(XMLQuery obj);
    void visit(QueryString obj);
    void visit(XMLParse obj);
    void visit(ExpressionCriteria obj);
    void visit(WithQueryCommand obj);
    void visit(TriggerAction obj);
    void visit(ArrayTable obj);
    void visit(ProcedureContainer obj);

    void visit(AlterView obj);
    void visit(AlterProcedure obj);
    void visit(AlterTrigger obj);

    void visit(WindowFunction windowFunction);

    void visit(ObjectTable objectTable);

    void visit(ExceptionExpression obj);

    void visit(ReturnStatement obj);

    void visit(WindowSpecification obj);

}
