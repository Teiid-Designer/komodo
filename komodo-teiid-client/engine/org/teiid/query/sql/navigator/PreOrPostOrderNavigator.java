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

package org.teiid.query.sql.navigator;

import java.util.Collection;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.AlterProcedureImpl;
import org.teiid.query.sql.lang.AlterTriggerImpl;
import org.teiid.query.sql.lang.AlterViewImpl;
import org.teiid.query.sql.lang.ArrayTableImpl;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CompoundCriteriaImpl;
import org.teiid.query.sql.lang.CreateImpl;
import org.teiid.query.sql.lang.CriteriaSelectorImpl;
import org.teiid.query.sql.lang.DeleteImpl;
import org.teiid.query.sql.lang.DropImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.ExpressionCriteriaImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.HasCriteriaImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IntoImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.JoinPredicateImpl;
import org.teiid.query.sql.lang.JoinTypeImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.LimitImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NotCriteriaImpl;
import org.teiid.query.sql.lang.ObjectColumnImpl;
import org.teiid.query.sql.lang.ObjectTableImpl;
import org.teiid.query.sql.lang.OptionImpl;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.OrderByItemImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.SetClauseListImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.TextTableImpl;
import org.teiid.query.sql.lang.TranslateCriteriaImpl;
import org.teiid.query.sql.lang.UnaryFromClauseImpl;
import org.teiid.query.sql.lang.UpdateImpl;
import org.teiid.query.sql.lang.WithQueryCommandImpl;
import org.teiid.query.sql.lang.XMLColumnImpl;
import org.teiid.query.sql.lang.XMLTableImpl;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateProcedureCommandImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.DeclareStatementImpl;
import org.teiid.query.sql.proc.ExceptionExpressionImpl;
import org.teiid.query.sql.proc.IfStatementImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.proc.RaiseErrorStatementImpl;
import org.teiid.query.sql.proc.RaiseStatementImpl;
import org.teiid.query.sql.proc.ReturnStatementImpl;
import org.teiid.query.sql.proc.TriggerActionImpl;
import org.teiid.query.sql.proc.WhileStatementImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.ArraySymbolImpl;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.JSONObjectImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.QueryStringImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.symbol.TextLineImpl;
import org.teiid.query.sql.symbol.BaseWindowFunction;
import org.teiid.query.sql.symbol.WindowSpecificationImpl;
import org.teiid.query.sql.symbol.XMLAttributesImpl;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLForestImpl;
import org.teiid.query.sql.symbol.XMLNamespacesImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;

/** 
 *
 */
public class PreOrPostOrderNavigator extends AbstractNavigator {

    /**
     * Pre order flag
     */
    public static final boolean PRE_ORDER = true;

    /**
     * Post order flag
     */
    public static final boolean POST_ORDER = false;

    private boolean order;
    private boolean deep;

    /**
     * @param visitor
     * @param order
     * @param deep
     */
    public PreOrPostOrderNavigator(TCLanguageVisitorImpl visitor, boolean order, boolean deep) {
        super(visitor);
        this.order = order;
        this.deep = deep;
    }

    protected void preVisitVisitor(BaseLanguageObject obj) {
        if (order == PRE_ORDER) {
            visitVisitor(obj);
        }
    }

    protected void postVisitVisitor(BaseLanguageObject obj) {
        if (order == POST_ORDER) {
            visitVisitor(obj);
        }
    }

    @Override
    public void visit(BaseAggregateSymbol obj) {
        preVisitVisitor(obj);

        if (getTeiidVersion().isLessThan(Version.TEIID_8_0.get()))
            visitNode(obj.getExpression());
        else {
            BaseExpression[] args = obj.getArgs();
            if (args != null) {
                for (int i = 0; i < args.length; i++) {
                    visitNode(args[i]);
                }
            }
        }

        visitNode(obj.getOrderBy());
        visitNode(obj.getCondition());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(AliasSymbolImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getSymbol());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(MultipleElementSymbolImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    public void visit(AssignmentStatementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getVariable());
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(BetweenCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        visitNode(obj.getLowerExpression());
        visitNode(obj.getUpperExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(BlockImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getStatements());
        visitNodes(obj.getExceptionStatements());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(BranchingStatementImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    public void visit(CaseExpressionImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        for (int i = 0; i < obj.getWhenCount(); i++) {
            visitNode(obj.getWhenExpression(i));
            visitNode(obj.getThenExpression(i));
        }
        visitNode(obj.getElseExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(CommandStatementImpl obj) {
        preVisitVisitor(obj);
        if (deep) {
            visitNode(obj.getCommand());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(CompareCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getLeftExpression());
        visitNode(obj.getRightExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(CompoundCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getCriteria());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ConstantImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(CreateUpdateProcedureCommandImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getBlock());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(CreateProcedureCommandImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getBlock());
        postVisitVisitor(obj);
    }

    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(CriteriaSelectorImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getElements());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(DeclareStatementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getVariable());
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(DeleteImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getGroup());
        visitNode(obj.getCriteria());
        visitNode(obj.getOption());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ElementSymbolImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ExistsCriteriaImpl obj) {
        preVisitVisitor(obj);
        if (deep) {
            visitNode(obj.getCommand());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ExpressionSymbolImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(FromImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getClauses());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(FunctionImpl obj) {
        preVisitVisitor(obj);
        BaseExpression[] args = obj.getArgs();
        if (args != null) {
            for (int i = 0; i < args.length; i++) {
                visitNode(args[i]);
            }
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(GroupByImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getSymbols());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(GroupSymbolImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(HasCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getSelector());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(IfStatementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getCondition());
        visitNode(obj.getIfBlock());
        visitNode(obj.getElseBlock());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(InsertImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getGroup());
        visitNodes(obj.getVariables());
        visitNodes(obj.getValues());
        if (deep && obj.getQueryExpression() != null) {
            visitNode(obj.getQueryExpression());
        }
        visitNode(obj.getOption());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(CreateImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getTable());
        visitNodes(obj.getColumnSymbols());
        visitNodes(obj.getPrimaryKey());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(DropImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getTable());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(IntoImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getGroup());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(IsNullCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(JoinPredicateImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getLeftClause());
        visitNode(obj.getJoinType());
        visitNode(obj.getRightClause());
        visitNodes(obj.getJoinCriteria());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(JoinTypeImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    public void visit(LimitImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getOffset());
        visitNode(obj.getRowLimit());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(LoopStatementImpl obj) {
        preVisitVisitor(obj);
        if (deep) {
            visitNode(obj.getCommand());
        }
        visitNode(obj.getBlock());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(MatchCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getLeftExpression());
        visitNode(obj.getRightExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(NotCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getCriteria());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(OptionImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    public void visit(OrderByImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getOrderByItems());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(OrderByItemImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getSymbol());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(QueryImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getWith());
        visitNode(obj.getSelect());
        visitNode(obj.getInto());
        visitNode(obj.getFrom());
        visitNode(obj.getCriteria());
        visitNode(obj.getGroupBy());
        visitNode(obj.getHaving());
        visitNode(obj.getOrderBy());
        visitNode(obj.getLimit());
        visitNode(obj.getOption());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(RaiseStatementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(RaiseErrorStatementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ReferenceImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ScalarSubqueryImpl obj) {
        preVisitVisitor(obj);
        if (deep) {
            visitNode(obj.getCommand());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SearchedCaseExpressionImpl obj) {
        preVisitVisitor(obj);
        for (int i = 0; i < obj.getWhenCount(); i++) {
            visitNode(obj.getWhenCriteria(i));
            visitNode(obj.getThenExpression(i));
        }
        visitNode(obj.getElseExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SelectImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getSymbols());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SetCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        visitNodes(obj.getValues());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SetQueryImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getWith());
        visitNodes(obj.getQueryCommands());
        visitNode(obj.getOrderBy());
        visitNode(obj.getLimit());
        visitNode(obj.getOption());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(StoredProcedureImpl obj) {
        preVisitVisitor(obj);

        Collection<SPParameterImpl> params = obj.getParameters();
        if (params != null && !params.isEmpty()) {
            for (SPParameterImpl parameter : params) {
                BaseExpression expression = parameter.getExpression();
                visitNode(expression);
            }
        }

        visitNode(obj.getOption());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getLeftExpression());
        if (deep) {
            visitNode(obj.getCommand());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SubqueryFromClauseImpl obj) {
        preVisitVisitor(obj);
        if (deep) {
            visitNode(obj.getCommand());
        }
        visitNode(obj.getGroupSymbol());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SubquerySetCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        if (deep) {
            visitNode(obj.getCommand());
        }
        postVisitVisitor(obj);
    }

    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(TranslateCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getSelector());
        visitNodes(obj.getTranslations());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(UnaryFromClauseImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getGroup());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(UpdateImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getGroup());
        visitNode(obj.getChangeList());
        visitNode(obj.getCriteria());
        visitNode(obj.getOption());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(WhileStatementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getCondition());
        visitNode(obj.getBlock());
        postVisitVisitor(obj);
    }

    /**
     * NOTE: we specifically don't need to visit the as columns or the using identifiers.
     * These will be resolved by the dynamic command resolver instead.
     * 
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.DynamicCommandImpl)
     */
    @Override
    public void visit(DynamicCommandImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getSql());
        visitNode(obj.getIntoGroup());
        if (obj.getUsing() != null) {
            for (SetClauseImpl setClause : obj.getUsing().getClauses()) {
                visitNode(setClause.getValue());
            }
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SetClauseListImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getClauses());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(SetClauseImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getSymbol());
        visitNode(obj.getValue());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(TextLineImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getExpressions());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLForestImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getNamespaces());
        visitNodes(obj.getArgs());
        postVisitVisitor(obj);
    }

    @Override
    @Since(Version.TEIID_8_0)
    public void visit(JSONObjectImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getArgs());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLAttributesImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getArgs());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLElementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getNamespaces());
        visitNode(obj.getAttributes());
        visitNodes(obj.getContent());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLNamespacesImpl obj) {
        preVisitVisitor(obj);
        postVisitVisitor(obj);
    }

    @Override
    public void visit(TextTableImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getFile());
        visitNode(obj.getGroupSymbol());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLTableImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getNamespaces());
        visitNodes(obj.getPassing());
        for (XMLColumnImpl column : obj.getColumns()) {
            visitNode(column.getDefaultExpression());
        }
        visitNode(obj.getGroupSymbol());
        postVisitVisitor(obj);
    }

    @Override
    @Since(Version.TEIID_8_0)
    public void visit(ObjectTableImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getPassing());
        for (ObjectColumnImpl column : obj.getColumns()) {
            visitNode(column.getDefaultExpression());
        }
        visitNode(obj.getGroupSymbol());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLQueryImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getNamespaces());
        visitNodes(obj.getPassing());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(DerivedColumnImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLSerializeImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(QueryStringImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getPath());
        visitNodes(obj.getArgs());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(XMLParseImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ExpressionCriteriaImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(WithQueryCommandImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getColumns());
        if (deep) {
            visitNode(obj.getCommand());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(TriggerActionImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getBlock());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(ArrayTableImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getArrayValue());
        visitNode(obj.getGroupSymbol());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(AlterProcedureImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getTarget());
        if (deep) {
            visitNode(obj.getDefinition());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(AlterTriggerImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getTarget());
        if (deep) {
            visitNode(obj.getDefinition());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(AlterViewImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getTarget());
        if (deep) {
            visitNode(obj.getDefinition());
        }
        postVisitVisitor(obj);
    }

    @Override
    public void visit(BaseWindowFunction obj) {
        preVisitVisitor(obj);
        visitNode(obj.getFunction());
        visitNode(obj.getWindowSpecification());
        postVisitVisitor(obj);
    }

    @Override
    public void visit(WindowSpecificationImpl obj) {
        preVisitVisitor(obj);
        visitNodes(obj.getPartition());
        visitNode(obj.getOrderBy());
        postVisitVisitor(obj);
    }

    @Override
    @Since(Version.TEIID_8_0)
    public void visit(ArraySymbolImpl array) {
        preVisitVisitor(array);
        visitNodes(array.getExpressions());
        postVisitVisitor(array);
    }

    @Override
    @Since(Version.TEIID_8_0)
    public void visit(ExceptionExpressionImpl exceptionExpression) {
        preVisitVisitor(exceptionExpression);
        visitNode(exceptionExpression.getMessage());
        visitNode(exceptionExpression.getSqlState());
        visitNode(exceptionExpression.getErrorCode());
        visitNode(exceptionExpression.getParent());
        postVisitVisitor(exceptionExpression);
    }

    @Override
    @Since(Version.TEIID_8_0)
    public void visit(ReturnStatementImpl obj) {
        preVisitVisitor(obj);
        visitNode(obj.getExpression());
        postVisitVisitor(obj);
    }

    /**
     * @param object
     * @param visitor
     * @param order
     */
    public static void doVisit(BaseLanguageObject object, TCLanguageVisitorImpl visitor, boolean order) {
        doVisit(object, visitor, order, false);
    }

    /**
     * @param object
     * @param visitor
     * @param order
     * @param deep
     */
    public static void doVisit(BaseLanguageObject object, TCLanguageVisitorImpl visitor, boolean order, boolean deep) {
        PreOrPostOrderNavigator nav = new PreOrPostOrderNavigator(visitor, order, deep);
        object.acceptVisitor(nav);
    }

}
