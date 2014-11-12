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
package org.teiid.runtime.client.query;

import java.util.ArrayList;
import java.util.List;
import org.komodo.spi.query.QueryFactory;
import org.komodo.spi.query.metadata.MetadataID;
import org.komodo.spi.query.metadata.QueryNode;
import org.komodo.spi.query.metadata.StoredProcedureInfo;
import org.komodo.spi.query.sql.lang.BetweenCriteria;
import org.komodo.spi.query.sql.lang.CompareCriteria;
import org.komodo.spi.query.sql.lang.CompoundCriteria;
import org.komodo.spi.query.sql.lang.Delete;
import org.komodo.spi.query.sql.lang.ExistsCriteria;
import org.komodo.spi.query.sql.lang.From;
import org.komodo.spi.query.sql.lang.GroupBy;
import org.komodo.spi.query.sql.lang.Insert;
import org.komodo.spi.query.sql.lang.IsNullCriteria;
import org.komodo.spi.query.sql.lang.JoinPredicate;
import org.komodo.spi.query.sql.lang.JoinType;
import org.komodo.spi.query.sql.lang.MatchCriteria;
import org.komodo.spi.query.sql.lang.NotCriteria;
import org.komodo.spi.query.sql.lang.Option;
import org.komodo.spi.query.sql.lang.OrderBy;
import org.komodo.spi.query.sql.lang.Query;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.query.sql.lang.SPParameter.ParameterInfo;
import org.komodo.spi.query.sql.lang.Select;
import org.komodo.spi.query.sql.lang.SetCriteria;
import org.komodo.spi.query.sql.lang.SetQuery;
import org.komodo.spi.query.sql.lang.SetQuery.Operation;
import org.komodo.spi.query.sql.lang.StoredProcedure;
import org.komodo.spi.query.sql.lang.SubqueryCompareCriteria;
import org.komodo.spi.query.sql.lang.SubqueryFromClause;
import org.komodo.spi.query.sql.lang.SubquerySetCriteria;
import org.komodo.spi.query.sql.lang.UnaryFromClause;
import org.komodo.spi.query.sql.lang.Update;
import org.komodo.spi.query.sql.proc.AssignmentStatement;
import org.komodo.spi.query.sql.proc.Block;
import org.komodo.spi.query.sql.proc.CommandStatement;
import org.komodo.spi.query.sql.proc.CreateProcedureCommand;
import org.komodo.spi.query.sql.proc.DeclareStatement;
import org.komodo.spi.query.sql.proc.RaiseStatement;
import org.komodo.spi.query.sql.symbol.AggregateSymbol;
import org.komodo.spi.query.sql.symbol.AliasSymbol;
import org.komodo.spi.query.sql.symbol.Constant;
import org.komodo.spi.query.sql.symbol.ElementSymbol;
import org.komodo.spi.query.sql.symbol.ElementSymbol.DisplayMode;
import org.komodo.spi.query.sql.symbol.ExpressionSymbol;
import org.komodo.spi.query.sql.symbol.Function;
import org.komodo.spi.query.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.symbol.MultipleElementSymbol;
import org.komodo.spi.query.sql.symbol.Reference;
import org.komodo.spi.query.sql.symbol.ScalarSubquery;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.mapping.relational.TCQueryNode;
import org.teiid.query.metadata.TCStoredProcedureInfo;
import org.teiid.query.metadata.TempMetadataID;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.parser.TeiidNodeFactory;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CompoundCriteriaImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.CriteriaOperator.Operator;
import org.teiid.query.sql.lang.DeleteImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.FromClauseImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.JoinPredicateImpl;
import org.teiid.query.sql.lang.JoinTypeImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NotCriteriaImpl;
import org.teiid.query.sql.lang.OptionImpl;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.QueryCommandImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl.PredicateQuantifier;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.UnaryFromClauseImpl;
import org.teiid.query.sql.lang.UpdateImpl;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateProcedureCommandImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.DeclareStatementImpl;
import org.teiid.query.sql.proc.RaiseErrorStatementImpl;
import org.teiid.query.sql.proc.RaiseStatementImpl;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;

/**
 *
 */
public class SyntaxFactory implements QueryFactory <BaseExpression, 
                                                                                                 BaseExpression,
                                                                                                 FromClauseImpl,
                                                                                                 ElementSymbolImpl,
                                                                                                 CommandImpl,
                                                                                                 QueryCommandImpl,
                                                                                                 CriteriaImpl,
                                                                                                 ConstantImpl,
                                                                                                 BlockImpl,
                                                                                                 BaseExpression,
                                                                                                 GroupSymbolImpl,
                                                                                                 JoinTypeImpl> {

    private TeiidClientParser teiidParser;

    private final TeiidNodeFactory nodeFactory = TeiidNodeFactory.getInstance();

    /**
     * @param teiidParser teiid parser
     */
    public SyntaxFactory(TeiidClientParser teiidParser) {
        this.teiidParser = teiidParser;
    }

    private boolean isGreaterThanOrEqualTo(TeiidVersion teiidVersion) {
        TeiidVersion minVersion = teiidParser.getVersion().getMinimumVersion();
        return minVersion.equals(teiidVersion) || minVersion.isGreaterThan(teiidVersion);
    }

    private <T extends BaseLanguageObject> T create(ASTNodes nodeType) {
        return nodeFactory.create(teiidParser, nodeType);
    }

    @Override
    public Function createFunction(String name, List<? extends BaseExpression> arguments) {
        if (arguments == null) {
            arguments = new ArrayList<BaseExpression>();
        }

        FunctionImpl function = create(ASTNodes.FUNCTION);
        function.setName(name);
        function.setArgs(arguments.toArray(new BaseExpression[0]));
        return function;
    }

    @Override
    public AggregateSymbol createAggregateSymbol(String functionName, BaseAggregateSymbol.Type functionType, boolean isDistinct, BaseExpression expression) {
        BaseAggregateSymbol aggregateSymbol = create(ASTNodes.AGGREGATE_SYMBOL);
        aggregateSymbol.setName(functionName);
        aggregateSymbol.setAggregateFunction(functionType);
        aggregateSymbol.setDistinct(isDistinct);

        if (expression != null) {
            if (isGreaterThanOrEqualTo(Version.TEIID_8_0.get()))
                aggregateSymbol.setArgs(new BaseExpression[] { expression });
            else
                aggregateSymbol.setExpression(expression);
        }

        return aggregateSymbol;
    }

    @Override
    public ElementSymbol createElementSymbol(String name) {
        ElementSymbolImpl elementSymbol = create(ASTNodes.ELEMENT_SYMBOL);
        elementSymbol.setName(name);
        return elementSymbol;
    }

    @Override
    public ElementSymbol createElementSymbol(String name, boolean displayFullyQualified) {
        ElementSymbolImpl elementSymbol = create(ASTNodes.ELEMENT_SYMBOL);
        elementSymbol.setName(name);
        if (displayFullyQualified)
            elementSymbol.setDisplayMode(DisplayMode.FULLY_QUALIFIED);
        else
            elementSymbol.setDisplayMode(DisplayMode.SHORT_OUTPUT_NAME);

        return elementSymbol;
    }

    @Override
    public AliasSymbol createAliasSymbol(String name, BaseExpression symbol) {
        AliasSymbolImpl aliasSymbol = create(ASTNodes.ALIAS_SYMBOL);
        aliasSymbol.setName(name);
        aliasSymbol.setSymbol(symbol);
        return aliasSymbol;
    }

    @Override
    public GroupSymbol createGroupSymbol(String name) {
        GroupSymbolImpl groupSymbol = create(ASTNodes.GROUP_SYMBOL);
        groupSymbol.setName(name);
        return groupSymbol;
    }

    @Override
    public GroupSymbol createGroupSymbol(String name, String definition) {
        GroupSymbolImpl groupSymbol = create(ASTNodes.GROUP_SYMBOL);
        groupSymbol.setName(name);
        groupSymbol.setDefinition(definition);
        return groupSymbol;
    }

    @Override
    public ExpressionSymbol createExpressionSymbol(String name, BaseExpression expression) {
        ExpressionSymbolImpl expressionSymbol = create(ASTNodes.EXPRESSION_SYMBOL);
        expressionSymbol.setName(name);
        expressionSymbol.setExpression(expression);
        return expressionSymbol;
    }

    @Override
    public MultipleElementSymbol createMultipleElementSymbol() {
        MultipleElementSymbolImpl multipleElementSymbol = create(ASTNodes.MULTIPLE_ELEMENT_SYMBOL);
        return multipleElementSymbol;
    }

    @Override
    public Constant createConstant(Object value) {
        ConstantImpl constant = create(ASTNodes.CONSTANT);
        constant.setValue(value);
        return constant;
    }

    @Override
    public DeclareStatement createDeclareStatement(ElementSymbolImpl variable, String valueType) {
        DeclareStatementImpl declareStatement = create(ASTNodes.DECLARE_STATEMENT);
        declareStatement.setVariable(variable);
        declareStatement.setVariableType(valueType);
        return declareStatement;
    }

    @Override
    public CommandStatement createCommandStatement(CommandImpl command) {
        CommandStatementImpl commandStatement = create(ASTNodes.COMMAND_STATEMENT);
        commandStatement.setCommand(command);
        return commandStatement;
    }

    @Override
    public RaiseStatement createRaiseStatement(BaseExpression expression) {
        if (isGreaterThanOrEqualTo(Version.TEIID_8_0.get())) {
            RaiseStatementImpl raiseStatement = create(ASTNodes.RAISE_STATEMENT);
            raiseStatement.setExpression(expression);
            return raiseStatement;
        } else {
            RaiseErrorStatementImpl raiseErrorStatement = create(ASTNodes.RAISE_ERROR_STATEMENT);
            raiseErrorStatement.setExpression(expression);
            return raiseErrorStatement;
        }
    }

    @Override
    public Query createQuery() {
        QueryImpl query = create(ASTNodes.QUERY);
        return query;
    }

    @Override
    public SetQuery createSetQuery(Operation operation, boolean all, QueryCommandImpl leftQuery, QueryCommandImpl rightQuery) {
        SetQueryImpl setQuery = create(ASTNodes.SET_QUERY);
        setQuery.setLeftQuery(leftQuery);
        setQuery.setAll(all);
        setQuery.setOperation(operation);
        setQuery.setRightQuery(rightQuery);
        return setQuery;
    }

    @Override
    public SetQuery createSetQuery(Operation operation) {
        SetQueryImpl setQuery = create(ASTNodes.SET_QUERY);
        setQuery.setOperation(operation);
        return setQuery;
    }

    @Override
    public CompareCriteria createCompareCriteria() {
        CompareCriteriaImpl compareCriteria = create(ASTNodes.COMPARE_CRITERIA);
        return compareCriteria;
    }

    @Override
    public CompareCriteria createCompareCriteria(BaseExpression expression1, int operator, BaseExpression expression2) {
        CompareCriteriaImpl compareCriteria = create(ASTNodes.COMPARE_CRITERIA);
        compareCriteria.setLeftExpression(expression1);
        compareCriteria.setOperator(Operator.findOperator(operator));
        compareCriteria.setRightExpression(expression2);
        return compareCriteria;
    }

    @Override
    public IsNullCriteria createIsNullCriteria() {
        IsNullCriteriaImpl isNullCriteria = create(ASTNodes.IS_NULL_CRITERIA);
        return isNullCriteria;
    }

    @Override
    public IsNullCriteria createIsNullCriteria(BaseExpression expression) {
        IsNullCriteriaImpl isNullCriteria = create(ASTNodes.IS_NULL_CRITERIA);
        isNullCriteria.setExpression(expression);
        return isNullCriteria;
    }

    @Override
    public NotCriteria createNotCriteria() {
        NotCriteriaImpl notCriteria = create(ASTNodes.NOT_CRITERIA);
        return notCriteria;
    }

    @Override
    public NotCriteria createNotCriteria(CriteriaImpl criteria) {
        NotCriteriaImpl notCriteria = create(ASTNodes.NOT_CRITERIA);
        notCriteria.setCriteria(criteria);
        return notCriteria;
    }

    @Override
    public MatchCriteria createMatchCriteria() {
        MatchCriteriaImpl matchCriteria = create(ASTNodes.MATCH_CRITERIA);
        return matchCriteria;
    }

    @Override
    public SetCriteria createSetCriteria() {
        SetCriteriaImpl setCriteria = create(ASTNodes.SET_CRITERIA);
        return setCriteria;
    }

    @Override
    public SubquerySetCriteria createSubquerySetCriteria() {
        SubquerySetCriteriaImpl subquerySetCriteria = create(ASTNodes.SUBQUERY_SET_CRITERIA);
        return subquerySetCriteria;
    }

    @Override
    public SubquerySetCriteria createSubquerySetCriteria(BaseExpression expression, QueryCommandImpl command) {
        SubquerySetCriteriaImpl subquerySetCriteria = create(ASTNodes.SUBQUERY_SET_CRITERIA);
        subquerySetCriteria.setExpression(expression);
        subquerySetCriteria.setCommand(command);
        return subquerySetCriteria;
    }

    @Override
    public SubqueryCompareCriteria createSubqueryCompareCriteria(BaseExpression leftExpression, QueryCommandImpl command, int operator, int predicateQuantifier) {
        SubqueryCompareCriteriaImpl subqueryCompareCriteria = create(ASTNodes.SUBQUERY_COMPARE_CRITERIA);
        subqueryCompareCriteria.setLeftExpression(leftExpression);
        subqueryCompareCriteria.setCommand(command);
        subqueryCompareCriteria.setOperator(Operator.findOperator(operator));
        subqueryCompareCriteria.setPredicateQuantifier(PredicateQuantifier.findQuantifier(predicateQuantifier));
        return subqueryCompareCriteria;
    }

    @Override
    public ScalarSubquery createScalarSubquery(QueryCommandImpl queryCommand) {
        ScalarSubqueryImpl scalarSubquery = create(ASTNodes.SCALAR_SUBQUERY);
        scalarSubquery.setCommand(queryCommand);
        return scalarSubquery;
    }

    @Override
    public BetweenCriteria createBetweenCriteria(ElementSymbolImpl elementSymbol, ConstantImpl constant1, ConstantImpl constant2) {
        BetweenCriteriaImpl betweenCriteria = create(ASTNodes.BETWEEN_CRITERIA);
        betweenCriteria.setExpression(elementSymbol);
        betweenCriteria.setLowerExpression(constant1);
        betweenCriteria.setUpperExpression(constant2);
        return betweenCriteria;
    }

    @Override
    public CompoundCriteria createCompoundCriteria(int operator, List<? extends CriteriaImpl> criteria) {
        CompoundCriteriaImpl compoundCriteria = create(ASTNodes.COMPOUND_CRITERIA);
        compoundCriteria.setOperator(operator);
        compoundCriteria.setCriteria(criteria);
        return compoundCriteria;
    }

    @Override
    public ExistsCriteria createExistsCriteria(QueryCommandImpl queryCommand) {
        ExistsCriteriaImpl existsCriteria = create(ASTNodes.EXISTS_CRITERIA);
        existsCriteria.setCommand(queryCommand);
        return existsCriteria;
    }

    @Override
    public Block createBlock() {
        BlockImpl block = create(ASTNodes.BLOCK);
        return block;
    }

    @Override
    public CreateProcedureCommand createCreateProcedureCommand(BlockImpl block) {
        if (isGreaterThanOrEqualTo(Version.TEIID_8_0.get())) {
            CreateProcedureCommandImpl command = create(ASTNodes.CREATE_PROCEDURE_COMMAND);
            command.setBlock(block);
            return command;
        } else {
            CreateUpdateProcedureCommandImpl command = create(ASTNodes.CREATE_UPDATE_PROCEDURE_COMMAND);
            command.setBlock(block);
            return command;
        }
    }

    @Override
    public AssignmentStatement createAssignmentStatement(ElementSymbolImpl elementSymbol, BaseExpression expression) {
        AssignmentStatementImpl assignmentStatement = create(ASTNodes.ASSIGNMENT_STATEMENT);
        assignmentStatement.setVariable(elementSymbol);
        assignmentStatement.setExpression(expression);
        return assignmentStatement;
    }

    @Override
    public AssignmentStatement createAssignmentStatement(ElementSymbolImpl elementSymbol, QueryCommandImpl queryCommand) {
        AssignmentStatementImpl assignmentStatement = create(ASTNodes.ASSIGNMENT_STATEMENT);
        assignmentStatement.setVariable(elementSymbol);
        assignmentStatement.setCommand(queryCommand);
        return assignmentStatement;
    }

    @Override
    public Select createSelect() {
        SelectImpl select = create(ASTNodes.SELECT);
        return select;
    }

    @Override
    public Select createSelect(List<? extends BaseExpression> symbols) {
        SelectImpl select = create(ASTNodes.SELECT);
        select.setSymbols(symbols);
        return select;
    }

    @Override
    public From createFrom() {
        FromImpl from = create(ASTNodes.FROM);
        return from;
    }

    @Override
    public From createFrom(List<? extends FromClauseImpl> fromClauses) {
        FromImpl from = create(ASTNodes.FROM);
        from.setClauses(fromClauses);
        return from;
    }

    @Override
    public UnaryFromClause createUnaryFromClause(GroupSymbolImpl symbol) {
        UnaryFromClauseImpl unaryFromClause = create(ASTNodes.UNARY_FROM_CLAUSE);
        unaryFromClause.setGroup(symbol);
        return unaryFromClause;
    }

    @Override
    public SubqueryFromClause createSubqueryFromClause(String name, QueryCommandImpl command) {
        SubqueryFromClauseImpl subqueryFromClause = create(ASTNodes.SUBQUERY_FROM_CLAUSE);
        subqueryFromClause.setName(name);
        subqueryFromClause.setCommand(command);
        return subqueryFromClause;
    }

    @Override
    public JoinType getJoinType(JoinType.Types joinType) {
        JoinTypeImpl join = create(ASTNodes.JOIN_TYPE);
        join.setKind(joinType);
        return join;
    }

    @Override
    public JoinPredicate createJoinPredicate(FromClauseImpl leftClause, FromClauseImpl rightClause, JoinTypeImpl joinType) {
        JoinPredicateImpl joinPredicate = create(ASTNodes.JOIN_PREDICATE);
        joinPredicate.setJoinType(joinType);
        joinPredicate.setLeftClause(leftClause);
        joinPredicate.setRightClause(rightClause);
        return joinPredicate;
    }

    @Override
    public JoinPredicate createJoinPredicate(FromClauseImpl leftClause, FromClauseImpl rightClause, JoinTypeImpl joinType, List<CriteriaImpl> criteria) {
        JoinPredicateImpl joinPredicate = create(ASTNodes.JOIN_PREDICATE);
        joinPredicate.setJoinType(joinType);
        joinPredicate.setLeftClause(leftClause);
        joinPredicate.setRightClause(rightClause);
        joinPredicate.setJoinCriteria(criteria);
        return joinPredicate;
    }

    @Override
    public GroupBy createGroupBy() {
        GroupByImpl groupBy = create(ASTNodes.GROUP_BY);
        return groupBy;
    }

    @Override
    public OrderBy createOrderBy() {
        OrderByImpl orderBy = create(ASTNodes.ORDER_BY);
        return orderBy;
    }

    @Override
    public Option createOption() {
        OptionImpl option = create(ASTNodes.OPTION);
        return option;
    }

    @Override
    public Update createUpdate() {
        UpdateImpl update = create(ASTNodes.UPDATE);
        return update;
    }

    @Override
    public Delete createDelete() {
        DeleteImpl delete = create(ASTNodes.DELETE);
        return delete;
    }

    @Override
    public Insert createInsert() {
        InsertImpl insert = create(ASTNodes.INSERT);
        return insert;
    }

    @Override
    public StoredProcedure createStoredProcedure() {
        StoredProcedureImpl storedProcedure = create(ASTNodes.STORED_PROCEDURE);
        return storedProcedure;
    }

    @Override
    public SPParameter createSPParameter(int index, BaseExpression expression) {
        return new SPParameterImpl(teiidParser, index, expression);
    }

    @Override
    public SPParameter createSPParameter(int index, ParameterInfo parameterType, String name) {
        return new SPParameterImpl(teiidParser, index, parameterType.index(), name);
    }

    @Override
    public Reference createReference(int index) {
        ReferenceImpl reference = create(ASTNodes.REFERENCE);
        reference.setIndex(index);
        return reference;
    }

    @Override
    public MetadataID createMetadataID(String id, Class clazz) {
        return new TempMetadataID(id, clazz);
    }

    @Override
    public StoredProcedureInfo createStoredProcedureInfo() {
        return new TCStoredProcedureInfo();
    }

    @Override
    public QueryNode createQueryNode(String queryPlan) {
        return new TCQueryNode(queryPlan);
    }
}
