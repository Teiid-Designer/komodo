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
import java.util.Set;

import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.query.sql.CommandCollectorVisitor;
import org.komodo.spi.query.sql.ElementCollectorVisitor;
import org.komodo.spi.query.sql.FunctionCollectorVisitor;
import org.komodo.spi.query.sql.GroupCollectorVisitor;
import org.komodo.spi.query.sql.GroupsUsedByElementsVisitor;
import org.komodo.spi.query.sql.PredicateCollectorVisitor;
import org.komodo.spi.query.sql.ReferenceCollectorVisitor;
import org.komodo.spi.query.sql.ResolverVisitor;
import org.komodo.spi.query.sql.SQLStringVisitor;
import org.komodo.spi.query.sql.SQLStringVisitorCallback;
import org.komodo.spi.query.sql.ValueIteratorProviderCollectorVisitor;
import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.query.sql.lang.CompareCriteria;
import org.komodo.spi.query.sql.lang.Expression;
import org.komodo.spi.query.sql.lang.IsNullCriteria;
import org.komodo.spi.query.sql.lang.MatchCriteria;
import org.komodo.spi.query.sql.lang.SetCriteria;
import org.komodo.spi.query.sql.lang.SubqueryContainer;
import org.komodo.spi.query.sql.lang.SubquerySetCriteria;
import org.komodo.spi.query.sql.symbol.GroupSymbol;
import org.komodo.spi.udf.FunctionMethodDescriptor;
import org.komodo.spi.udf.FunctionLibrary;
import org.komodo.spi.validator.UpdateValidator;
import org.komodo.spi.validator.Validator;
import org.komodo.spi.validator.UpdateValidator.TransformUpdateType;
import org.komodo.spi.xml.MappingDocumentFactory;

/**
 *
 */
public interface QueryService {

    /**
     * Get the query parser
     * 
     * @return implementation of {@link QueryParser}
     */
    QueryParser getQueryParser();

    /**
     * Is the given word a reserved part of the SQL syntax
     * 
     * @param word
     * 
     * @return true if the word is reserved.
     */
    boolean isReservedWord(final String word);

    /**
     * Is the given word a reserved part of the Procedure SQL syntax
     * 
     * @param word
     * 
     * @return true if the word is reserved.
     */
    boolean isProcedureReservedWord(final String word);

    /**
     * Get the SQL reserved words
     * 
     * @return set of reserved words
     */
    Set<String> getReservedWords();

    /**
     * Get the SQL non-reserved words
     * 
     * @return set of non-reserved words
     */
    Set<String> getNonReservedWords();

    /**
     * Get the name of the JDCB type that conforms to the
     * given index number
     * 
     * @param jdbcType
     * 
     * @return type name
     */
    String getJDBCSQLTypeName(int jdbcType);

    /**
     * Create a new default function library
     * 
     * @return instance of {@link FunctionLibrary}
     */
    FunctionLibrary createFunctionLibrary();

    /**
     * Create a new function library with custom functions
     * derived from the given list of descriptors
     * 
     * @param functionMethodDescriptors
     * 
     * @return instance of {@link FunctionLibrary}
     */
    FunctionLibrary createFunctionLibrary(List<FunctionMethodDescriptor> functionMethodDescriptors);

    /**
     * Create a query language factory
     *
     * @return factory
     */
    QueryFactory createQueryFactory();
    
    /**
     * Create an xml mapping document factory
     * 
     * @return factory
     */
    MappingDocumentFactory getMappingDocumentFactory();

    /**
     * Get the symbol name version of the
     * given expression
     * 
     * @param expression
     * 
     * @return name of given expression
     */
    String getSymbolName(Expression expression);

    /**
     * Get the symbol short name version of the
     * given name
     * 
     * @param name
     * 
     * @return short name of given name
     */
    String getSymbolShortName(String name);

    /**
     * Get the symbol short name version of the
     * given expression
     * 
     * @param expression
     * 
     * @return short name of given expression
     */
    String getSymbolShortName(Expression expression);

    /**
     * Get the visitor that converts SQL objects into their
     * SQL syntax
     * 
     * @return SQL string visitor
     */
    SQLStringVisitor getSQLStringVisitor();

    /**
     * An {@link SQLStringVisitor} that can be extended
     * using the given callback
     * 
     * @param visitorCallback
     * 
     * @return instance of {@link SQLStringVisitor}
     */
    SQLStringVisitor getCallbackSQLStringVisitor(SQLStringVisitorCallback visitorCallback);

    /**
     * This visitor class will traverse a language object tree and collect all group
     * symbol references it finds.  It uses a collection to collect the groups in so
     * different collections will give you different collection properties - for instance,
     * using a Set will remove duplicates.
     * 
     * @param removeDuplicates 
     * 
     * @return instance of {@link GroupCollectorVisitor}
     */
    GroupCollectorVisitor getGroupCollectorVisitor(boolean removeDuplicates);

    /**
     * Get the visitor the retrieves the groups used by elements
     * in a collection.
     * 
     * @return instance of {@link GroupsUsedByElementsVisitor}
     */
    GroupsUsedByElementsVisitor getGroupsUsedByElementsVisitor();

    /**
     * This visitor class will traverse a language object tree and collect all element
     * symbol references it finds.  It uses a collection to collect the elements in so
     * different collections will give you different collection properties - for instance,
     * using a Set will remove duplicates.
     * 
     * @param removeDuplicates 
     * 
     * @return instance of {@link ElementCollectorVisitor}
     */
    ElementCollectorVisitor getElementCollectorVisitor(boolean removeDuplicates);

    /**
    * This visitor class will traverse a language object tree and collect all sub-commands 
    * it finds.  It uses a List to collect the sub-commands in the order they're found.
    * 
    * @return instance of {@link CommandCollectorVisitor}
    */
    CommandCollectorVisitor getCommandCollectorVisitor();
    
    /**
     * <p>This visitor class will traverse a language object tree and collect all Function
     * references it finds.  It uses a collection to collect the Functions in so
     * different collections will give you different collection properties - for instance,
     * using a Set will remove duplicates.</p>
     * 
     * <p>This visitor can optionally collect functions of only a specific name</p>
     * 
     * @param removeDuplicates 
     *
     * @return instance of {@link FunctionCollectorVisitor}
     */
    FunctionCollectorVisitor getFunctionCollectorVisitor(boolean removeDuplicates);

    /**
     * <p>Walk a tree of language objects and collect any predicate criteria that are found.
     * A predicate criteria is of the following types: </p>
     *
     * <ul>
     * <li>{@link CompareCriteria} CompareCriteria</li>
     * <li>{@link MatchCriteria} MatchCriteria</li>
     * <li>{@link SetCriteria} SetCriteria</li>
     * <li>{@link SubquerySetCriteria} SubquerySetCriteria</li>
     * <li>{@link IsNullCriteria} IsNullCriteria</li>
     * </ul>
     * 
     * @return instance of {@link PredicateCollectorVisitor} 
     */
    PredicateCollectorVisitor getPredicateCollectorVisitor();
    
    /**
     * This visitor class will traverse a language object tree and collect all
     * references it finds.
     * 
     * @return instance of {@link ReferenceCollectorVisitor}
     */
    ReferenceCollectorVisitor getReferenceCollectorVisitor();
    
    /**
     * This visitor class will traverse a language object tree and collect all language
     * objects that implement {@link SubqueryContainer}.
     * 
     * @return instance of {@link ValueIteratorProviderCollectorVisitor}
     */
    ValueIteratorProviderCollectorVisitor getValueIteratorProviderCollectorVisitor();
    
    /**
     * This visitor class will traverse and resolve the given language object
     * 
     * @return instance of {@link ResolverVisitor}
     */
    ResolverVisitor getResolverVisitor();
    
    /**
     * Get the validator
     * 
     * @return instance of {@link Validator}
     */
    Validator getValidator();

    /**
     * Get the update validator
     * 
     * @param metadata
     * @param insertType
     * @param updateType 
     * @param deleteType
     * 
     * @return instance of {@link UpdateValidator}
     */
    UpdateValidator getUpdateValidator(QueryMetadataInterface metadata, TransformUpdateType insertType, TransformUpdateType updateType, TransformUpdateType deleteType);

    /**
     * Resolve the given group
     * 
     * @param groupSymbol
     * @param metadata
     * @throws Exception 
     */
    void resolveGroup(GroupSymbol groupSymbol, QueryMetadataInterface metadata) throws Exception;

    /**
     * Convert all elements in a command to their fully qualified names.
     * 
     * @param command Command to convert
     */
    void fullyQualifyElements(Command command);

    /**
     * Get the query resolver
     * 
     * @return instance of {@link QueryResolver}
     */
    QueryResolver getQueryResolver();
    
    /**
     * Get the procedure service
     * 
     * @return instance of {@link ProcedureService}
     */
    ProcedureService getProcedureService();

}
