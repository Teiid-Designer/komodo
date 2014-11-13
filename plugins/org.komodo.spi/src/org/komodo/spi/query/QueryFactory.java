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

import java.util.List;

import org.komodo.spi.query.metadata.MetadataID;
import org.komodo.spi.query.metadata.QueryNode;
import org.komodo.spi.query.metadata.StoredProcedureInfo;
import org.komodo.spi.query.sql.lang.BetweenCriteria;
import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.query.sql.lang.CompareCriteria;
import org.komodo.spi.query.sql.lang.CompoundCriteria;
import org.komodo.spi.query.sql.lang.Criteria;
import org.komodo.spi.query.sql.lang.Delete;
import org.komodo.spi.query.sql.lang.ExistsCriteria;
import org.komodo.spi.query.sql.lang.Expression;
import org.komodo.spi.query.sql.lang.From;
import org.komodo.spi.query.sql.lang.FromClause;
import org.komodo.spi.query.sql.lang.GroupBy;
import org.komodo.spi.query.sql.lang.Insert;
import org.komodo.spi.query.sql.lang.IsNullCriteria;
import org.komodo.spi.query.sql.lang.JoinPredicate;
import org.komodo.spi.query.sql.lang.JoinType;
import org.komodo.spi.query.sql.lang.LanguageObject;
import org.komodo.spi.query.sql.lang.MatchCriteria;
import org.komodo.spi.query.sql.lang.NotCriteria;
import org.komodo.spi.query.sql.lang.Option;
import org.komodo.spi.query.sql.lang.OrderBy;
import org.komodo.spi.query.sql.lang.Query;
import org.komodo.spi.query.sql.lang.QueryCommand;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.query.sql.lang.Select;
import org.komodo.spi.query.sql.lang.SetCriteria;
import org.komodo.spi.query.sql.lang.SetQuery;
import org.komodo.spi.query.sql.lang.StoredProcedure;
import org.komodo.spi.query.sql.lang.SubqueryCompareCriteria;
import org.komodo.spi.query.sql.lang.SubqueryFromClause;
import org.komodo.spi.query.sql.lang.SubquerySetCriteria;
import org.komodo.spi.query.sql.lang.UnaryFromClause;
import org.komodo.spi.query.sql.lang.Update;
import org.komodo.spi.query.sql.lang.SPParameter.ParameterInfo;
import org.komodo.spi.query.sql.lang.SetQuery.Operation;
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
import org.komodo.spi.query.sql.symbol.ExpressionSymbol;
import org.komodo.spi.query.sql.symbol.Function;
import org.komodo.spi.query.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.symbol.MultipleElementSymbol;
import org.komodo.spi.query.sql.symbol.Reference;
import org.komodo.spi.query.sql.symbol.ScalarSubquery;
import org.komodo.spi.query.sql.symbol.AggregateSymbol.Type;

/**
 *
 */
public interface QueryFactory<E extends Expression, 
                                                      SES extends Expression /* SingleElementSymbol */,
                                                      F extends FromClause,
                                                      ES extends ElementSymbol,
                                                      C extends Command,
                                                      QC extends QueryCommand,
                                                      CR extends Criteria,
                                                      CO extends Constant,
                                                      B extends Block,
                                                      SS extends LanguageObject /* SelectSymbol */,
                                                      GS extends GroupSymbol,
                                                      JT extends JoinType> {
    
    /**
     * Create a new function
     * 
     * @param name
     * @param arguments
     * 
     * @return instance of {@link Function}
     */
    Function createFunction(String name, List<? extends E> arguments);
    
    /**
     * Create a new aggregate symbol
     * 
     * @param functionName 
     * @param functionType 
     * @param isDistinct 
     * @param expression
     * 
     * @return instance of {@link AggregateSymbol}
     */
    AggregateSymbol createAggregateSymbol(String functionName, Type functionType, boolean isDistinct, E expression);
    
    /**
     * Create a new element symbol
     * 
     * @param name
     * 
     * @return instance of {@link ElementSymbol}
     */
    ElementSymbol createElementSymbol(String name);

    /**
     * Create a new element symbol
     * 
     * @param name
     * @param displayFullyQualified True if should display fully qualified
     * 
     * @return instance of {@link ElementSymbol}
     */
    ElementSymbol createElementSymbol(String name, boolean displayFullyQualified);
    
    /**
     * Create an alias symbol
     * 
     * @param name
     * @param symbol
     * 
     * @return instance of {@link AliasSymbol}
     */
    AliasSymbol createAliasSymbol(String name, SES symbol);
    
    /**
     * Create a new group symbol
     * 
     * @param name
     * 
     * @return instance of {@link GroupSymbol}
     */
    GroupSymbol createGroupSymbol(String name);
    
    /**
     * Create a new group symbol
     * 
     * @param name
     * @param definition
     * 
     * @return instance of {@link GroupSymbol}
     */
    GroupSymbol createGroupSymbol(String name, String definition);
    
    /**
     * Create an expression symbol
     * 
     * @param name
     * @param expression
     * 
     * @return instance of {@link ExpressionSymbol}
     */
    ExpressionSymbol createExpressionSymbol(String name, E expression);
    
    /**
     * Create an multiple element symbol
     * 
     * @return instance of {@link MultipleElementSymbol}
     */
    MultipleElementSymbol createMultipleElementSymbol();
    
    /**
     * Create a new constant
     * 
     * @param value
     * 
     * @return instance of {@link Constant}
     */
    Constant createConstant(Object value);

    /**
     * Create a new declare statement.
     * 
     * @param variable The <code>ElementSymbol</code> object that is the variable
     * @param valueType The type of this variable
     * 
     * @return instance of  {@link DeclareStatement}
     */
    DeclareStatement createDeclareStatement(ES variable, String valueType);
    
    /**
     * Create a command statement
     * 
     * @param command
     * 
     * @return instance of {@link CommandStatement}
     */
    CommandStatement createCommandStatement(C command);
    
    /**
     * Create a raise statement
     * 
     * @param expression
     * 
     * @return instance of {@link RaiseStatement}
     */
    RaiseStatement createRaiseStatement(E expression);
    
    /**
     * Create a query
     * 
     * @return instance of {@link Query}
     */
    Query createQuery();

    /**
     * Create a set query
     * 
     * @param operation
     * @param all
     * @param leftQuery 
     * @param rightQuery 
     * 
     * @return instance of {@link SetQuery}
     */
    SetQuery createSetQuery(Operation operation, 
                                              boolean all, 
                                              QC leftQuery, 
                                              QC rightQuery);

    /**
     * Create a set query
     * 
     * @param operation
     * 
     * @return instance of {@link SetQuery}
     */
    SetQuery createSetQuery(Operation operation);
    
    /**
     * Create a compare criteria
     * 
     * @return instance of {@link CompareCriteria}
     */
    CompareCriteria createCompareCriteria();

    /**
     * Create a compare criteria
     * 
     * @param expression1
     * @param operator
     * @param expression2
     * 
     * @return instance of {@link CompareCriteria}
     */
    CompareCriteria createCompareCriteria(E expression1, 
                                                                   int operator, 
                                                                   E expression2);
    
    /**
     * Create an is null criteria
     * 
     * @return instance of {@link IsNullCriteria}
     */
    IsNullCriteria createIsNullCriteria();

    /**
     * Create an is null criteria
     * 
     * @param expression 
     * 
     * @return instance of {@link IsNullCriteria}
     */
    IsNullCriteria createIsNullCriteria(E expression);
    
    /**
     * Create a not criteria
     * 
     * @return instance of {@link NotCriteria}
     */
    NotCriteria createNotCriteria();
    
    /**
     * Create a not criteria
     * 
     * @param criteria 
     * 
     * @return instance of {@link NotCriteria}
     */
    NotCriteria createNotCriteria(CR criteria);
    
    /**
     * Create a match criteria
     * 
     * @return instance of {@link MatchCriteria}
     */
    MatchCriteria createMatchCriteria();

    /**
     * Create a set criteria
     * 
     * @return instance of {@link SetCriteria}
     */
    SetCriteria createSetCriteria();

    /**
     * Create a subquery set criteria
     * 
     * @return instance of {@link SubquerySetCriteria}
     */
    SubquerySetCriteria createSubquerySetCriteria();

    /**
     * Create a subquery set criteria
     * 
     * @param expression 
     * @param command 
     * 
     * @return instance of {@link SubquerySetCriteria}
     */
    SubquerySetCriteria createSubquerySetCriteria(E expression, QC command);
    
    /**
     * Create a subquery compare criteria
     * 
     * @param leftExpression
     * @param command
     * @param operator
     * @param predicateQuantifier
     * 
     * @return instance of {@link SubqueryCompareCriteria}
     */
    SubqueryCompareCriteria createSubqueryCompareCriteria(E leftExpression, QC command, int operator, int predicateQuantifier);
    
    /**
     * Create a scalar sub query
     * 
     * @param queryCommand
     * 
     * @return instance of {@link ScalarSubquery}
     */
    ScalarSubquery createScalarSubquery(QC queryCommand);
    
    /**
     * Create an in-between criteria
     * 
     * @param elementSymbol
     * @param constant1
     * @param constant2
     * 
     * @return instance of {@link BetweenCriteria}
     */
    BetweenCriteria createBetweenCriteria(ES elementSymbol,
                                                                   CO constant1,
                                                                   CO constant2);

    /**
     * Create a compound criteria
     * 
     * @param operator
     * @param criteria
     * 
     * @return instance of {@link CompoundCriteria}
     */
    CompoundCriteria createCompoundCriteria(int operator, List<? extends CR> criteria);

    /**
     * Create an exists criteria
     * 
     * @param queryCommand
     * 
     * @return instance of {@link ExistsCriteria}
     */
    ExistsCriteria createExistsCriteria(QC queryCommand);
    
    /**
     * Create a block
     * 
     * @return instance of {@link Block}
     */
    Block createBlock();

    /**
     * Create a create-procedure statement
     * 
     * @param block
     * 
     * @return instance of {@link CreateProcedureCommand}
     */
    CreateProcedureCommand createCreateProcedureCommand(B block);

    /**
     * Create an assignment statement
     * 
     * @param elementSymbol 
     * @param expression
     * 
     * @return instance of {@link AssignmentStatement}
     */
    AssignmentStatement createAssignmentStatement(ES elementSymbol, E expression);

    /**
     * Create an assignment statement
     * 
     * @param elementSymbol 
     * @param queryCommand
     * 
     * @return instance of {@link AssignmentStatement}
     */
    AssignmentStatement createAssignmentStatement(ES elementSymbol, QC queryCommand);

    /**
     * Create a select
     * 
     * @return instance of {@link Select}
     */
    Select createSelect();
    
    /**
     * Create a select
     * 
     * @param symbols
     * 
     * @return instance of {@link Select}
     */
    Select createSelect(List<? extends SS> symbols);

    /**
     * Create a from
     * 
     * @return instance of {@link From}
     */
    From createFrom();

    /**
     * Create a from
     * 
     * @param fromClauses
     * 
     * @return instance of {@link From}
     */
    From createFrom(List<? extends F> fromClauses);
    
    /**
     * Create a unary from clause
     * 
     * @param symbol
     * 
     * @return instance of {@link UnaryFromClause}
     */
    UnaryFromClause createUnaryFromClause(GS symbol);

    /**
     * Create a subquery from clause
     * 
     * @param name
     * @param command
     * 
     * @return instance of {@link SubqueryFromClause}
     */
    SubqueryFromClause createSubqueryFromClause(String name, QC command);


    /**
     * Create a join type
     * 
     * @param joinType 
     * 
     * @return instance of {@link JoinType}
     */
    JoinType getJoinType(JoinType.Types joinType);
    
    /**
     * Create a join predicate
     * 
     * @param leftClause 
     * @param rightClause 
     * @param joinType
     * 
     * @return instance of {@link JoinPredicate}
     */
    JoinPredicate createJoinPredicate(F leftClause,
                                                         F rightClause, 
                                                         JT joinType);
    /**
     * Create a join predicate
     * 
     * @param leftClause 
     * @param rightClause 
     * @param joinType
     * @param criteria 
     * 
     * @return instance of {@link JoinPredicate}
     */    
    JoinPredicate createJoinPredicate(F leftClause, 
                                                         F rightClause,
                                                         JT joinType,
                                                         List<CR> criteria);
    
    /**
     * Create a group by
     * 
     * @return instance of {@link GroupBy}
     */
    GroupBy createGroupBy();

    /**
     * Create an order by
     * 
     * @return instance of {@link OrderBy}
     */
    OrderBy createOrderBy();
    
    /**
     * Create an option
     * 
     * @return instance of {@link Option}
     */
    Option createOption();
    
    /**
     * Create an update
     * 
     * @return instance of {@link Update}
     */
    Update createUpdate();
    
    /**
     * Create a delete     
     * 
     * @return instance of {@link Delete}
     */
    Delete createDelete();
    
    /**
     * Create an insert     
     * 
     * @return instance of {@link Insert}
     */
    Insert createInsert();
    
    /**
     * Create a stored procedure
     * 
     * @return instance of {@link StoredProcedure}
     */
    StoredProcedure createStoredProcedure();

    /**
     * Create a stored procedure parameter
     * 
     * @param index
     * @param expression
     * 
     * @return instance of {@link SPParameter}
     */
    SPParameter createSPParameter(int index, E expression);

    /**
     * Create a stored procedure parameter
     * 
     * @param index 
     * @param parameterType 
     * @param name
     * 
     * @return instance of {@link SPParameter}
     */
    SPParameter createSPParameter(int index, ParameterInfo parameterType, String name);
    
    /**
     * Create a reference
     * 
     * @param index
     * 
     * @return instance of {@link Reference}
     */
    Reference createReference(int index);

    /**
     * Create a metadata id
     * 
     * @param upperCase
     * @param clazz
     * 
     * @return instance of {@link MetadataID}
     */
    MetadataID createMetadataID(String upperCase, Class clazz);

    /**
     * Create a stored procedure info
     * 
     * @return instance of {@link StoredProcedureInfo}
     */
    StoredProcedureInfo createStoredProcedureInfo();

    /**
     * Create a query node
     * 
     * @param queryPlan
     * 
     * @return instance of {@link QueryNode}
     */
    QueryNode createQueryNode(String queryPlan);

}
