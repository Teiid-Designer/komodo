/*
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
*/
package org.komodo.modeshape.teiid.sql;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.komodo.modeshape.teiid.parser.SQQueryParser;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sql.lang.ArrayTable;
import org.komodo.modeshape.teiid.sql.lang.BetweenCriteria;
import org.komodo.modeshape.teiid.sql.lang.Command;
import org.komodo.modeshape.teiid.sql.lang.CompareCriteria;
import org.komodo.modeshape.teiid.sql.lang.CompoundCriteria;
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator.Operator;
import org.komodo.modeshape.teiid.sql.lang.Delete;
import org.komodo.modeshape.teiid.sql.lang.DynamicCommand;
import org.komodo.modeshape.teiid.sql.lang.ExistsCriteria;
import org.komodo.modeshape.teiid.sql.lang.ExpressionCriteria;
import org.komodo.modeshape.teiid.sql.lang.From;
import org.komodo.modeshape.teiid.sql.lang.FromClause;
import org.komodo.modeshape.teiid.sql.lang.GroupBy;
import org.komodo.modeshape.teiid.sql.lang.Insert;
import org.komodo.modeshape.teiid.sql.lang.Into;
import org.komodo.modeshape.teiid.sql.lang.IsNullCriteria;
import org.komodo.modeshape.teiid.sql.lang.JoinPredicate;
import org.komodo.modeshape.teiid.sql.lang.JoinType;
import org.komodo.modeshape.teiid.sql.lang.LanguageObject;
import org.komodo.modeshape.teiid.sql.lang.MatchCriteria;
import org.komodo.modeshape.teiid.sql.lang.NamespaceItem;
import org.komodo.modeshape.teiid.sql.lang.NotCriteria;
import org.komodo.modeshape.teiid.sql.lang.OrderBy;
import org.komodo.modeshape.teiid.sql.lang.OrderByItem;
import org.komodo.modeshape.teiid.sql.lang.ProjectedColumn;
import org.komodo.modeshape.teiid.sql.lang.Query;
import org.komodo.modeshape.teiid.sql.lang.QueryCommand;
import org.komodo.modeshape.teiid.sql.lang.SPParameter;
import org.komodo.modeshape.teiid.sql.lang.Select;
import org.komodo.modeshape.teiid.sql.lang.SetClause;
import org.komodo.modeshape.teiid.sql.lang.SetClauseList;
import org.komodo.modeshape.teiid.sql.lang.SetCriteria;
import org.komodo.modeshape.teiid.sql.lang.SetQuery;
import org.komodo.modeshape.teiid.sql.lang.StoredProcedure;
import org.komodo.modeshape.teiid.sql.lang.SubqueryCompareCriteria;
import org.komodo.modeshape.teiid.sql.lang.SubqueryCompareCriteria.PredicateQuantifier;
import org.komodo.modeshape.teiid.sql.lang.SubqueryFromClause;
import org.komodo.modeshape.teiid.sql.lang.SubquerySetCriteria;
import org.komodo.modeshape.teiid.sql.lang.TextColumn;
import org.komodo.modeshape.teiid.sql.lang.TextTable;
import org.komodo.modeshape.teiid.sql.lang.UnaryFromClause;
import org.komodo.modeshape.teiid.sql.lang.Update;
import org.komodo.modeshape.teiid.sql.lang.WithQueryCommand;
import org.komodo.modeshape.teiid.sql.lang.XMLColumn;
import org.komodo.modeshape.teiid.sql.lang.XMLTable;
import org.komodo.modeshape.teiid.sql.proc.AssignmentStatement;
import org.komodo.modeshape.teiid.sql.proc.Block;
import org.komodo.modeshape.teiid.sql.proc.BranchingStatement;
import org.komodo.modeshape.teiid.sql.proc.BranchingStatement.BranchingMode;
import org.komodo.modeshape.teiid.sql.proc.CommandStatement;
import org.komodo.modeshape.teiid.sql.proc.CreateProcedureCommand;
import org.komodo.modeshape.teiid.sql.proc.DeclareStatement;
import org.komodo.modeshape.teiid.sql.proc.ExceptionExpression;
import org.komodo.modeshape.teiid.sql.proc.IfStatement;
import org.komodo.modeshape.teiid.sql.proc.LoopStatement;
import org.komodo.modeshape.teiid.sql.proc.Statement;
import org.komodo.modeshape.teiid.sql.proc.WhileStatement;
import org.komodo.modeshape.teiid.sql.symbol.AggregateSymbol;
import org.komodo.modeshape.teiid.sql.symbol.AliasSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Array;
import org.komodo.modeshape.teiid.sql.symbol.CaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.Constant;
import org.komodo.modeshape.teiid.sql.symbol.DerivedColumn;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.Function;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.modeshape.teiid.sql.symbol.JSONObject;
import org.komodo.modeshape.teiid.sql.symbol.MultipleElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Reference;
import org.komodo.modeshape.teiid.sql.symbol.ScalarSubquery;
import org.komodo.modeshape.teiid.sql.symbol.SearchedCaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.TextLine;
import org.komodo.modeshape.teiid.sql.symbol.WindowFunction;
import org.komodo.modeshape.teiid.sql.symbol.WindowSpecification;
import org.komodo.modeshape.teiid.sql.symbol.XMLAttributes;
import org.komodo.modeshape.teiid.sql.symbol.XMLElement;
import org.komodo.modeshape.teiid.sql.symbol.XMLForest;
import org.komodo.modeshape.teiid.sql.symbol.XMLNamespaces;
import org.komodo.modeshape.teiid.sql.symbol.XMLParse;
import org.komodo.modeshape.teiid.sql.symbol.XMLQuery;
import org.komodo.modeshape.teiid.sql.symbol.XMLSerialize;
import org.komodo.spi.query.sql.lang.IJoinType.Types;
import org.komodo.spi.query.sql.lang.ISPParameter;
import org.komodo.spi.query.sql.lang.ISetQuery.Operation;
import org.komodo.spi.type.IDataTypeManagerService;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;

/**
 *
 */
@SuppressWarnings( "javadoc" )
public abstract class AbstractTestFactory {

    private SQQueryParser parser;

    public AbstractTestFactory(SQQueryParser parser) {
        this.parser = parser;
    }

    public IDataTypeManagerService getDataTypeService() {
        return parser.getTeiidParser().getDataTypeService();
    }

    public <T> T newNode(ASTNodes nodeType) {
        return parser.getTeiidParser().createASTNode(nodeType);
    }

    public <T extends LanguageObject> T newObject(T object) {
        return (T) object.clone();
    }

    public GroupSymbol newGroupSymbol(String... groupSymbolProps) {
        String name = groupSymbolProps[0];
        String definition = null;
        if (groupSymbolProps.length > 1)
            definition = groupSymbolProps[1];

        GroupSymbol gs = newNode(ASTNodes.GROUP_SYMBOL);
        gs.setName(name);
        if (definition != null)
            gs.setDefinition(definition);

        return gs;
    }

    public UnaryFromClause newUnaryFromClause(GroupSymbol groupSymbol) {
        UnaryFromClause ufc = newNode(ASTNodes.UNARY_FROM_CLAUSE);
        ufc.setGroup(groupSymbol);
        return ufc;
    }

    public UnaryFromClause newUnaryFromClause(String... groupSymbolProps) {
        return newUnaryFromClause(newGroupSymbol(groupSymbolProps));
    }

    public JoinPredicate newJoinPredicate(FromClause leftClause, FromClause rightClause, JoinType.Types joinTypeTypes) {
        JoinType joinType = newNode(ASTNodes.JOIN_TYPE);
        joinType.setKind(joinTypeTypes);
        JoinPredicate jp = newNode(ASTNodes.JOIN_PREDICATE);
        jp.setLeftClause(leftClause);
        jp.setRightClause(rightClause);
        jp.setJoinType(joinType);

        return jp;
    }

    public JoinPredicate newJoinPredicate(FromClause leftClause, FromClause rightClause, JoinType.Types joinTypeTypes, List<? extends Criteria> crits) {
        JoinPredicate jp = newJoinPredicate(leftClause, rightClause, joinTypeTypes);
        jp.setJoinCriteria((List<Criteria>)crits);
        return jp;
    }

    public From newFrom() {
        return newNode(ASTNodes.FROM);
    }

    public From newFrom(List<? extends FromClause> clauses) {
        From from = newFrom();
        from.setClauses(clauses);
        return from;
    }

    public Select newSelect() {
        return newNode(ASTNodes.SELECT);
    }

    public Select newSelect(List<? extends Expression> symbols) {
        Select select = newSelect();
        for (Expression symbol : symbols) {
            select.addSymbol(symbol);
        }
        return select;
    }

    public MultipleElementSymbol newMultipleElementSymbol() {
        MultipleElementSymbol mes = newNode(ASTNodes.MULTIPLE_ELEMENT_SYMBOL);
        return mes;
    }

    public MultipleElementSymbol newMultipleElementSymbol(String name) {
        MultipleElementSymbol mes = newMultipleElementSymbol();
        mes.setName(name);
        return mes;
    }

    public BranchingStatement newBranchingStatement() {
        BranchingStatement stmt = newNode(ASTNodes.BRANCHING_STATEMENT);
        return stmt;
    }

    public BranchingStatement newBranchingStatement(BranchingMode mode) {
        BranchingStatement stmt = newBranchingStatement();
        stmt.setMode(mode);
        return stmt;
    }

    public DynamicCommand newDynamicCommand() {
        DynamicCommand dc = newNode(ASTNodes.DYNAMIC_COMMAND);
        return dc;
    }

    public Select newSelectWithMultileElementSymbol() {
        Select select = newSelect();
        MultipleElementSymbol all = newMultipleElementSymbol();
        select.addSymbol(all);
        return select;
    }

    public Query newQuery() {
        Query query = newNode(ASTNodes.QUERY);
        return query;
    }

    public Query newQuery(Select select, From from) {
        Query query = newQuery();
        query.setSelect(select);
        query.setFrom(from);
        return query;
    }

    public ExpressionCriteria newExpressionCriteria(ElementSymbol newElementSymbol) {
        ExpressionCriteria ec = newNode(ASTNodes.EXPRESSION_CRITERIA);
        ec.setExpression(newElementSymbol);
        return ec;
    }

    public ExceptionExpression newExceptionExpression() {
        ExceptionExpression ee = newNode(ASTNodes.EXCEPTION_EXPRESSION);
        return ee;
    }

    public OrderByItem newOrderByItem(Expression symbol, boolean ascending) {
        OrderByItem orderByItem = newNode(ASTNodes.ORDER_BY_ITEM);
        orderByItem.setSymbol(symbol);
        orderByItem.setAscending(ascending);
        return orderByItem;
    }

    public OrderBy newOrderBy() {
        OrderBy orderBy = newNode(ASTNodes.ORDER_BY);
        return orderBy;
    }

    public OrderBy newOrderBy(List<? extends Expression> parameters) {
        OrderBy orderBy = newOrderBy();
        List<OrderByItem> orderByItems = new ArrayList<OrderByItem>();
        for (Expression singleElementSymbol : parameters) {
            orderByItems.add(newOrderByItem(singleElementSymbol, true));
        }
        orderBy.setOrderByItems(orderByItems);

        return orderBy;
    }

    public OrderBy newOrderBy(List<? extends Expression> parameters, List<Boolean> orderTypes) {
        OrderBy orderBy = newOrderBy();
        List<OrderByItem> orderByItems = new ArrayList<OrderByItem>();
        Iterator<Boolean> typeIter = orderTypes.iterator();
        for (Expression singleElementSymbol : parameters) {
            orderByItems.add(newOrderByItem(singleElementSymbol, typeIter.next()));
        }
        orderBy.setOrderByItems(orderByItems);

        return orderBy;
    }

    public ElementSymbol newElementSymbol(String symbolName) {
        ElementSymbol elementSymbol = newNode(ASTNodes.ELEMENT_SYMBOL);
        elementSymbol.setName(symbolName);
        return elementSymbol;
    }

    public ElementSymbol newElementSymbol(String shortName, GroupSymbol gs) {
        return newElementSymbol(shortName, gs, getDataTypeService().getDefaultDataClass(DataTypeName.STRING));
    }

    public ElementSymbol newElementSymbol(String shortName, GroupSymbol gs, Class<?> typeClass) {
        ElementSymbol elementSymbol = newNode(ASTNodes.ELEMENT_SYMBOL);
        elementSymbol.setName(shortName);
        elementSymbol.setGroupSymbol(gs);
        elementSymbol.setType(typeClass);
        return elementSymbol;
    }

    public Constant newConstant(Object literal) {
        Constant constant = newNode(ASTNodes.CONSTANT);
        constant.setValue(literal);
        return constant;
    }

    public Constant newConstant(Object literal, Class<?> type) {
        Constant constant = newConstant(literal);
        constant.setType(type);
        return constant;
    }

    public AliasSymbol newAliasSymbol(String aliasName, Expression expression) {
        AliasSymbol aliasSymbol = newNode(ASTNodes.ALIAS_SYMBOL);
        aliasSymbol.setName(aliasName);
        aliasSymbol.setSymbol(expression);
        return aliasSymbol;
    }

    public AliasSymbol newAliasSymbolWithElementSymbol(String aliasName, String elementSymbolName) {
        return newAliasSymbol(aliasName, newElementSymbol(elementSymbolName));
    }

    public CompareCriteria newCompareCriteria(Expression leftExpr, Operator operator, Expression rightExpr) {
        CompareCriteria crit = newNode(ASTNodes.COMPARE_CRITERIA);
        crit.setLeftExpression(leftExpr);
        crit.setOperator(operator);
        crit.setRightExpression(rightExpr);
        return crit;
    }

    public CompareCriteria newCompareCriteria(String leftExprName, Operator operator, String rightExprName) {
        ElementSymbol left = newElementSymbol(leftExprName);
        ElementSymbol right = newElementSymbol(rightExprName);
        return newCompareCriteria(left, operator, right);
    }

    public CompoundCriteria newCompoundCriteria(int operator, Criteria left, Criteria right) {
        CompoundCriteria cc = newNode(ASTNodes.COMPOUND_CRITERIA);
        cc.setOperator(operator);
        cc.addCriteria(left);
        cc.addCriteria(right);
        return cc;
    }

    public TextColumn newTextColumn(String name, String type, int position) {
        TextColumn tc = newNode(ASTNodes.TEXT_COLUMN);
        tc.setName(name);
        tc.setType(type);
        tc.setWidth(position);
        return tc;
    }

    public TextTable newTextTable() {
        TextTable tt = newNode(ASTNodes.TEXT_TABLE);
        return tt;
    }

    public Reference newReference(int index) {
        Reference reference = newNode(ASTNodes.REFERENCE);
        reference.setIndex(index);
        reference.setPositional(true);
        return reference;
    }

    public ProjectedColumn newProjectedColumn(String name, String type) {
        ProjectedColumn pc = newNode(ASTNodes.PROJECTED_COLUMN);
        pc.setName(name);
        pc.setType(type);
        return pc;
    }

    public IsNullCriteria newIsNullCriteria(Expression expression) {
        IsNullCriteria isNullCriteria = newNode(ASTNodes.IS_NULL_CRITERIA);
        isNullCriteria.setExpression(expression);
        return isNullCriteria;
    }

    public NotCriteria newNotCriteria(Criteria criteria) {
        NotCriteria notCriteria = newNode(ASTNodes.NOT_CRITERIA);
        notCriteria.setCriteria(criteria);
        return notCriteria;
    }

    public WithQueryCommand newWithQueryCommand(GroupSymbol groupSymbol, QueryCommand queryExpression) {
        WithQueryCommand withQueryCommand = newNode(ASTNodes.WITH_QUERY_COMMAND);
        withQueryCommand.setGroupSymbol(groupSymbol);
        withQueryCommand.setQueryExpression(queryExpression);
        return withQueryCommand;
    }

    public Block newBlock() {
        Block block = newNode(ASTNodes.BLOCK);
        return block;
    }

    public Block newBlock(CommandStatement cmdStmt) {
        Block block = newBlock();
        block.addStatement(cmdStmt);
        return block;
    }

    public AssignmentStatement newAssignmentStatement(ElementSymbol var1, Command command) {
        AssignmentStatement as = newNode(ASTNodes.ASSIGNMENT_STATEMENT);
        as.setVariable(var1);
        as.setCommand(command);
        return as;
    }

    public AssignmentStatement newAssignmentStatement(ElementSymbol var1, Expression expression) {
        AssignmentStatement as = newNode(ASTNodes.ASSIGNMENT_STATEMENT);
        as.setVariable(var1);
        as.setExpression(expression);
        return as;
    }

    public Function newFunction(String name, Expression... args) {
        Function function = newNode(ASTNodes.FUNCTION);
        function.setName(name);
        function.setArgs(args);
        return function;
    }

    public DerivedColumn newDerivedColumn(String alias, Expression expression) {
        DerivedColumn dc = newNode(ASTNodes.DERIVED_COLUMN);
        dc.setAlias(alias);
        dc.setExpression(expression);
        return dc;
    }

    public JSONObject newJSONObject(List<DerivedColumn> args) {
        JSONObject json = newNode(ASTNodes.JSON_OBJECT);
        json.setArgs(args);
        return json;
    }

    public CommandStatement newCommandStatement(Command cmd) {
        CommandStatement cmdStmt = newNode(ASTNodes.COMMAND_STATEMENT);
        cmdStmt.setCommand(cmd);
        return cmdStmt;
    }

    public GroupBy newGroupBy(ElementSymbol... elementSymbols) {
        GroupBy groupBy = newNode(ASTNodes.GROUP_BY);

        if (elementSymbols != null) {
            for (ElementSymbol es : elementSymbols) {
                groupBy.addSymbol(es);
            }
        }

        return groupBy;
    }

    public MatchCriteria newMatchCriteria(Expression left, Expression right) {
        MatchCriteria crit = newNode(ASTNodes.MATCH_CRITERIA);
        crit.setLeftExpression(left);
        crit.setRightExpression(right);
        return crit;
    }

    public MatchCriteria newMatchCriteria(Expression left, Expression right, char escapeChar) {
        MatchCriteria crit = newMatchCriteria(left, right);
        crit.setEscapeChar(escapeChar);
        return crit;
    }

    public CreateProcedureCommand newCreateProcedureCommand() {
        CreateProcedureCommand cpc = newNode(ASTNodes.CREATE_PROCEDURE_COMMAND);
        return cpc;
    }

    public CreateProcedureCommand newCreateProcedureCommand(Block block) {
        CreateProcedureCommand cpc = newCreateProcedureCommand();
        cpc.setBlock(block);
        return cpc;
    }

    public IfStatement newIfStatement(Criteria criteria, Block ifBlock) {
        IfStatement ifStmt = newNode(ASTNodes.IF_STATEMENT);
        ifStmt.setIfBlock(ifBlock);
        ifStmt.setCondition(criteria);
        return ifStmt;
    }

    public LoopStatement newLoopStatement(Block block, Query query, String cursorName) {
        LoopStatement loopStmt = newNode(ASTNodes.LOOP_STATEMENT);
        loopStmt.setBlock(block);
        loopStmt.setCommand(query);
        loopStmt.setCursorName(cursorName);
        return loopStmt;
    }

    public DeclareStatement newDeclareStatement(ElementSymbol elementSymbol, String varType) {
        DeclareStatement ds = newNode(ASTNodes.DECLARE_STATEMENT);
        ds.setVariable(elementSymbol);
        ds.setVariableType(varType);
        return ds;
    }

    public DeclareStatement newDeclareStatement(ElementSymbol elementSymbol, String varType, Expression value) {
        DeclareStatement ds = newDeclareStatement(elementSymbol, varType);
        ds.setExpression(value);
        return ds;
    }

    public StoredProcedure newStoredProcedure() {
        StoredProcedure sp = newNode(ASTNodes.STORED_PROCEDURE);
        return sp;
    }

    public SPParameter newSPParameter(int index, Expression expression) {
        SPParameter parameter = newNode(ASTNodes.SP_PARAMETER);
        parameter.setIndex(index);
        parameter.setExpression(expression);
        return parameter;
    }

    public SPParameter newSPParameter(int index, ISPParameter.ParameterInfo paramType, String name) {
        SPParameter parameter = newNode(ASTNodes.SP_PARAMETER);
        parameter.setIndex(index);
        parameter.setParameterType(paramType.index());
        parameter.setName(name);
        return parameter;
    }

    public SubqueryFromClause newSubqueryFromClause(String name, Command command) {
        SubqueryFromClause sfc = newNode(ASTNodes.SUBQUERY_FROM_CLAUSE);
        sfc.setName(name);
        sfc.setCommand(command);
        return sfc;
    }

    public ArrayTable newArrayTable() {
        ArrayTable arrayTable = newNode(ASTNodes.ARRAY_TABLE);
        return arrayTable;
    }

    public BetweenCriteria newBetweenCriteria(Expression expression, Expression lowerExpression, Expression upperExpression) {
        BetweenCriteria betweenCriteria = newNode(ASTNodes.BETWEEN_CRITERIA);
        betweenCriteria.setLowerExpression(lowerExpression);
        betweenCriteria.setUpperExpression(upperExpression);
        betweenCriteria.setExpression(expression);
        return betweenCriteria;
    }

    public Delete newDelete(GroupSymbol group, Criteria criteria) {
        Delete delete = newNode(ASTNodes.DELETE);
        delete.setGroup(group);
        delete.setCriteria(criteria);
        return delete;
    }

    public Insert newInsert() {
        Insert insert = newNode(ASTNodes.INSERT);
        return insert;
    }

    public Update newUpdate() {
        Update update = newNode(ASTNodes.UPDATE);
        return update;
    }

    public WhileStatement newWhileStatement(Criteria criteria, Block block) {
        WhileStatement whileStatement = newNode(ASTNodes.WHILE_STATEMENT);
        whileStatement.setBlock(block);
        whileStatement.setCondition(criteria);
        return whileStatement;
    }

    public WindowSpecification newWindowSpecification() {
        WindowSpecification windowSpecification = newNode(ASTNodes.WINDOW_SPECIFICATION);
        return windowSpecification;
    }

    public Into newInto(GroupSymbol group) {
        Into into = newNode(ASTNodes.INTO);
        into.setGroup(group);
        return into;
    }

    public SearchedCaseExpression newSearchedCaseExpression(List<? extends Criteria> when, List<? extends Expression> then) {
        SearchedCaseExpression sce = newNode(ASTNodes.SEARCHED_CASE_EXPRESSION);
        sce.setWhen(when);
        sce.setThen(then);
        return sce;
    }

    public SetClause newSetClause(ElementSymbol symbol, Expression value) {
        SetClause setClause = newNode(ASTNodes.SET_CLAUSE);
        setClause.setSymbol(symbol);
        setClause.setValue(value);
        return setClause;
    }

    public SetClauseList newSetClauseList() {
        SetClauseList setClauseList = newNode(ASTNodes.SET_CLAUSE_LIST);
        return setClauseList;
    }

    public SetQuery newSetQuery(QueryCommand leftQuery, Operation operation, QueryCommand rightQuery, boolean all) {
        SetQuery setQuery = newNode(ASTNodes.SET_QUERY);
        setQuery.setAll(all);
        setQuery.setLeftQuery(leftQuery);
        setQuery.setOperation(operation);
        setQuery.setRightQuery(rightQuery);
        return setQuery;
    }

    public TextLine newTextLine() {
        TextLine textLine = newNode(ASTNodes.TEXT_LINE);
        return textLine;
    }

    public ExistsCriteria newExistsCriteria(QueryCommand queryCommand) {
        ExistsCriteria existsCriteria = newNode(ASTNodes.EXISTS_CRITERIA);
        existsCriteria.setCommand(queryCommand);
        return existsCriteria;
    }

    public XMLParse newXMLParse() {
        XMLParse xmlParse = newNode(ASTNodes.XML_PARSE);
        return xmlParse;
    }

    public XMLQuery newXMLQuery() {
        XMLQuery xmlQuery = newNode(ASTNodes.XML_QUERY);
        return xmlQuery;
    }

    public XMLTable newXMLTable() {
        XMLTable xmlTable = newNode(ASTNodes.XML_TABLE);
        return xmlTable;
    }

    public XMLAttributes newXMLAttributes(List<DerivedColumn> args) {
        XMLAttributes xmlAttributes = newNode(ASTNodes.XML_ATTRIBUTES);
        xmlAttributes.setArgs(args);
        return xmlAttributes;
    }

    public XMLColumn newXMLColumn(String name, boolean ordinal) {
        XMLColumn xmlColumn = newNode(ASTNodes.XML_COLUMN);
        xmlColumn.setName(name);
        xmlColumn.setOrdinal(ordinal);
        return xmlColumn;
    }

    public XMLSerialize newXMLSerialize() {
        XMLSerialize xmlSerialize = newNode(ASTNodes.XML_SERIALIZE);
        return xmlSerialize;
    }

    public XMLElement newXMLElement(String name, List<Expression> content) {
        XMLElement xmlElement = newNode(ASTNodes.XML_ELEMENT);
        xmlElement.setName(name);
        xmlElement.setContent(content);
        return xmlElement;
    }

    public XMLForest newXMLForest(List<DerivedColumn> derivedColumns) {
        XMLForest xmlForest = newNode(ASTNodes.XML_FOREST);
        xmlForest.setArguments(derivedColumns);
        return xmlForest;
    }

    public XMLNamespaces newXMLNamespaces(List<NamespaceItem> namespaceItems) {
        XMLNamespaces xmlNamespaces = newNode(ASTNodes.XML_NAMESPACES);
        xmlNamespaces.setNamespaceItems(namespaceItems);
        return xmlNamespaces;
    }

    public SubquerySetCriteria newSubquerySetCriteria(Expression expression, QueryCommand command) {
        SubquerySetCriteria ssc = newNode(ASTNodes.SUBQUERY_SET_CRITERIA);
        ssc.setExpression(expression);
        ssc.setCommand(command);
        return ssc;
    }

    public SubqueryCompareCriteria newSubqueryCompareCriteria(Expression expression, Query query, Operator operator, PredicateQuantifier quantifier) {
        SubqueryCompareCriteria scc = newNode(ASTNodes.SUBQUERY_COMPARE_CRITERIA);
        scc.setLeftExpression(expression);
        scc.setOperator(operator);
        scc.setPredicateQuantifier(quantifier);
        scc.setCommand(query);
        return scc;
    }

    public SetCriteria newSetCriteria(ElementSymbol symbol, List<Expression> values) {
        SetCriteria sc = newNode(ASTNodes.SET_CRITERIA);
        sc.setExpression(symbol);
        sc.setValues(values);
        return sc;
    }

    public ScalarSubquery newScalarSubquery(Query query) {
        ScalarSubquery scalarSubquery = newNode(ASTNodes.SCALAR_SUBQUERY);
        scalarSubquery.setCommand(query);
        return scalarSubquery;
    }

    public Array newArray(List<Expression> args) {
        Array array = newNode(ASTNodes.ARRAY);
        array.setExpressions(args);
        return array;
    }

    public JoinType newJoinType(Types joinKind) {
        JoinType joinType = newNode(ASTNodes.JOIN_TYPE);
        joinType.setKind(joinKind);
        return joinType;
    }

    public CaseExpression newCaseExpression(ElementSymbol es, List<Expression> whenExpressions, List<Expression> thenExpressions) {
        CaseExpression caseExpression = newNode(ASTNodes.CASE_EXPRESSION);
        caseExpression.setExpression(es);
        caseExpression.setWhen(whenExpressions);
        caseExpression.setThen(thenExpressions);
        return caseExpression;
    }

    public abstract AggregateSymbol newAggregateSymbol(String name, boolean isDistinct, Expression expression);

    public abstract WindowFunction newWindowFunction(String name);

    public abstract Expression wrapExpression(Expression expr, String... exprName);

    public abstract Statement newRaiseStatement(Expression expr);
}
