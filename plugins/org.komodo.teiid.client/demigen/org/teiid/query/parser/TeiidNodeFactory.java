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
package org.teiid.query.parser;

import org.teiid.query.parser.v7.Teiid7Parser;
import org.teiid.query.parser.v8.Teiid8Parser;
import org.teiid.query.parser.TeiidParser;
import org.teiid.runtime.client.Messages;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.teiid.query.sql.lang.*;
import org.teiid.query.sql.proc.*;
import org.teiid.query.sql.symbol.*;
import org.teiid.query.sql.lang.v7.*;
import org.teiid.query.sql.lang.v8.*;
import org.teiid.query.sql.symbol.v7.*;
import org.teiid.query.sql.symbol.v8.*;

/**
 * Factory for creating parser nodes
 */
public class TeiidNodeFactory {

	private static TeiidNodeFactory instance;


	public static TeiidNodeFactory getInstance() {
		if (instance == null) instance = new TeiidNodeFactory();
		return instance;
	}


	private static boolean isTeiid7Parser(TeiidParser teiidParser) {
		return teiidParser instanceof Teiid7Parser;
	}


	private static boolean isTeiid8Parser(TeiidParser teiidParser) {
		return teiidParser instanceof Teiid8Parser;
	}

	/**
	 * Method used by the generated parsers for constructing nodes
	 *
	 * @param teiidParser
	 * @param nodeType
	 *
	 * @return created language object
	 */
	public static LanguageObject jjtCreate(TeiidParser teiidParser, int nodeType) {
		return getInstance().create(teiidParser, nodeType);
	}

	/**
	 * Create a parser node for the given node type
	 *
	 * @param teiidParser
	 * @param nodeType
	 *
	 * @return node applicable to the given parser
	 */
	public <T extends LanguageObject> T create(TeiidParser teiidParser, int nodeType) {
		if (isTeiid7Parser(teiidParser))
			return create((Teiid7Parser) teiidParser, nodeType);
		else if (isTeiid8Parser(teiidParser))
			return create((Teiid8Parser) teiidParser, nodeType);
		throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
	}

	/**
	 * Create a parser node for the node with the given common node name
	 *
	 * @param teiidParser
	 * @param nodeType
	 *
	 * @return node applicable to the given parser
	 */
	public <T extends LanguageObject> T create(TeiidParser teiidParser, ASTNodes nodeType) {

		if (isTeiid7Parser(teiidParser)) {
			for (int i = 0; i < Teiid7ParserTreeConstants.jjtNodeName.length; ++i) {
				String constantName = Teiid7ParserTreeConstants.jjtNodeName[i];
					if (! constantName.equalsIgnoreCase(nodeType.getName()))
						continue;

				return create(teiidParser, i);
			}
		}

		else if (isTeiid8Parser(teiidParser)) {
			for (int i = 0; i < Teiid8ParserTreeConstants.jjtNodeName.length; ++i) {
				String constantName = Teiid8ParserTreeConstants.jjtNodeName[i];
					if (! constantName.equalsIgnoreCase(nodeType.getName()))
						continue;

				return create(teiidParser, i);
			}
		}

		throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType.getName(), teiidParser.getVersion()));
	}

	/**
	 * Names of AST nodes to allow creation outside of the parsers
	 *
	 * @generated
	 */
	public enum ASTNodes {
		/**
		 * TriggerAction
		 * @generated
		 */
		TRIGGER_ACTION("TriggerAction"), //$NON-NLS-1$

		/**
		 * Drop
		 * @generated
		 */
		DROP("Drop"), //$NON-NLS-1$

		/**
		 * Create
		 * @generated
		 */
		CREATE("Create"), //$NON-NLS-1$

		/**
		 * RaiseErrorStatement
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		RAISE_ERROR_STATEMENT("RaiseErrorStatement"), //$NON-NLS-1$

		/**
		 * BranchingStatement
		 * @generated
		 */
		BRANCHING_STATEMENT("BranchingStatement"), //$NON-NLS-1$

		/**
		 * WhileStatement
		 * @generated
		 */
		WHILE_STATEMENT("WhileStatement"), //$NON-NLS-1$

		/**
		 * LoopStatement
		 * @generated
		 */
		LOOP_STATEMENT("LoopStatement"), //$NON-NLS-1$

		/**
		 * IfStatement
		 * @generated
		 */
		IF_STATEMENT("IfStatement"), //$NON-NLS-1$

		/**
		 * CriteriaSelector
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		CRITERIA_SELECTOR("CriteriaSelector"), //$NON-NLS-1$

		/**
		 * HasCriteria
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		HAS_CRITERIA("HasCriteria"), //$NON-NLS-1$

		/**
		 * DeclareStatement
		 * @generated
		 */
		DECLARE_STATEMENT("DeclareStatement"), //$NON-NLS-1$

		/**
		 * CommandStatement
		 * @generated
		 */
		COMMAND_STATEMENT("CommandStatement"), //$NON-NLS-1$

		/**
		 * TranslateCriteria
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		TRANSLATE_CRITERIA("TranslateCriteria"), //$NON-NLS-1$

		/**
		 * CreateUpdateProcedureCommand
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		CREATE_UPDATE_PROCEDURE_COMMAND("CreateUpdateProcedureCommand"), //$NON-NLS-1$

		/**
		 * DynamicCommand
		 * @generated
		 */
		DYNAMIC_COMMAND("DynamicCommand"), //$NON-NLS-1$

		/**
		 * SetClauseList
		 * @generated
		 */
		SET_CLAUSE_LIST("SetClauseList"), //$NON-NLS-1$

		/**
		 * SetClause
		 * @generated
		 */
		SET_CLAUSE("SetClause"), //$NON-NLS-1$

		/**
		 * ProjectedColumn
		 * @generated
		 */
		PROJECTED_COLUMN("ProjectedColumn"), //$NON-NLS-1$

		/**
		 * StoredProcedure
		 * @generated
		 */
		STORED_PROCEDURE("StoredProcedure"), //$NON-NLS-1$

		/**
		 * Insert
		 * @generated
		 */
		INSERT("Insert"), //$NON-NLS-1$

		/**
		 * Update
		 * @generated
		 */
		UPDATE("Update"), //$NON-NLS-1$

		/**
		 * Delete
		 * @generated
		 */
		DELETE("Delete"), //$NON-NLS-1$

		/**
		 * WithQueryCommand
		 * @generated
		 */
		WITH_QUERY_COMMAND("WithQueryCommand"), //$NON-NLS-1$

		/**
		 * SetQuery
		 * @generated
		 */
		SET_QUERY("SetQuery"), //$NON-NLS-1$

		/**
		 * Query
		 * @generated
		 */
		QUERY("Query"), //$NON-NLS-1$

		/**
		 * Into
		 * @generated
		 */
		INTO("Into"), //$NON-NLS-1$

		/**
		 * Select
		 * @generated
		 */
		SELECT("Select"), //$NON-NLS-1$

		/**
		 * DerivedColumn
		 * @generated
		 */
		DERIVED_COLUMN("DerivedColumn"), //$NON-NLS-1$

		/**
		 * MultipleElementSymbol
		 * @generated
		 */
		MULTIPLE_ELEMENT_SYMBOL("MultipleElementSymbol"), //$NON-NLS-1$

		/**
		 * From
		 * @generated
		 */
		FROM("From"), //$NON-NLS-1$

		/**
		 * JoinPredicate
		 * @generated
		 */
		JOIN_PREDICATE("JoinPredicate"), //$NON-NLS-1$

		/**
		 * JoinType
		 * @generated
		 */
		JOIN_TYPE("JoinType"), //$NON-NLS-1$

		/**
		 * XMLSerialize
		 * @generated
		 */
		XML_SERIALIZE("XMLSerialize"), //$NON-NLS-1$

		/**
		 * ArrayTable
		 * @generated
		 */
		ARRAY_TABLE("ArrayTable"), //$NON-NLS-1$

		/**
		 * TextTable
		 * @generated
		 */
		TEXT_TABLE("TextTable"), //$NON-NLS-1$

		/**
		 * TextColumn
		 * @generated
		 */
		TEXT_COLUMN("TextColumn"), //$NON-NLS-1$

		/**
		 * XMLQuery
		 * @generated
		 */
		XML_QUERY("XMLQuery"), //$NON-NLS-1$

		/**
		 * XMLTable
		 * @generated
		 */
		XML_TABLE("XMLTable"), //$NON-NLS-1$

		/**
		 * XMLColumn
		 * @generated
		 */
		XML_COLUMN("XMLColumn"), //$NON-NLS-1$

		/**
		 * SubqueryFromClause
		 * @generated
		 */
		SUBQUERY_FROM_CLAUSE("SubqueryFromClause"), //$NON-NLS-1$

		/**
		 * UnaryFromClause
		 * @generated
		 */
		UNARY_FROM_CLAUSE("UnaryFromClause"), //$NON-NLS-1$

		/**
		 * Criteria
		 * @generated
		 */
		CRITERIA("Criteria"), //$NON-NLS-1$

		/**
		 * CompoundCriteria
		 * @generated
		 */
		COMPOUND_CRITERIA("CompoundCriteria"), //$NON-NLS-1$

		/**
		 * NotCriteria
		 * @generated
		 */
		NOT_CRITERIA("NotCriteria"), //$NON-NLS-1$

		/**
		 * CompareCriteria
		 * @generated
		 */
		COMPARE_CRITERIA("CompareCriteria"), //$NON-NLS-1$

		/**
		 * SubqueryCompareCriteria
		 * @generated
		 */
		SUBQUERY_COMPARE_CRITERIA("SubqueryCompareCriteria"), //$NON-NLS-1$

		/**
		 * MatchCriteria
		 * @generated
		 */
		MATCH_CRITERIA("MatchCriteria"), //$NON-NLS-1$

		/**
		 * BetweenCriteria
		 * @generated
		 */
		BETWEEN_CRITERIA("BetweenCriteria"), //$NON-NLS-1$

		/**
		 * IsNullCriteria
		 * @generated
		 */
		IS_NULL_CRITERIA("IsNullCriteria"), //$NON-NLS-1$

		/**
		 * SubquerySetCriteria
		 * @generated
		 */
		SUBQUERY_SET_CRITERIA("SubquerySetCriteria"), //$NON-NLS-1$

		/**
		 * SetCriteria
		 * @generated
		 */
		SET_CRITERIA("SetCriteria"), //$NON-NLS-1$

		/**
		 * ExistsCriteria
		 * @generated
		 */
		EXISTS_CRITERIA("ExistsCriteria"), //$NON-NLS-1$

		/**
		 * GroupBy
		 * @generated
		 */
		GROUP_BY("GroupBy"), //$NON-NLS-1$

		/**
		 * OrderBy
		 * @generated
		 */
		ORDER_BY("OrderBy"), //$NON-NLS-1$

		/**
		 * OrderByItem
		 * @generated
		 */
		ORDER_BY_ITEM("OrderByItem"), //$NON-NLS-1$

		/**
		 * ExpressionSymbol
		 * @generated
		 */
		EXPRESSION_SYMBOL("ExpressionSymbol"), //$NON-NLS-1$

		/**
		 * Limit
		 * @generated
		 */
		LIMIT("Limit"), //$NON-NLS-1$

		/**
		 * Option
		 * @generated
		 */
		OPTION("Option"), //$NON-NLS-1$

		/**
		 * Reference
		 * @generated
		 */
		REFERENCE("Reference"), //$NON-NLS-1$

		/**
		 * CaseExpression
		 * @generated
		 */
		CASE_EXPRESSION("CaseExpression"), //$NON-NLS-1$

		/**
		 * SearchedCaseExpression
		 * @generated
		 */
		SEARCHED_CASE_EXPRESSION("SearchedCaseExpression"), //$NON-NLS-1$

		/**
		 * Function
		 * @generated
		 */
		FUNCTION("Function"), //$NON-NLS-1$

		/**
		 * XMLParse
		 * @generated
		 */
		XML_PARSE("XMLParse"), //$NON-NLS-1$

		/**
		 * QueryString
		 * @generated
		 */
		QUERY_STRING("QueryString"), //$NON-NLS-1$

		/**
		 * XMLElement
		 * @generated
		 */
		XML_ELEMENT("XMLElement"), //$NON-NLS-1$

		/**
		 * XMLAttributes
		 * @generated
		 */
		XML_ATTRIBUTES("XMLAttributes"), //$NON-NLS-1$

		/**
		 * XMLForest
		 * @generated
		 */
		XML_FOREST("XMLForest"), //$NON-NLS-1$

		/**
		 * XMLNamespaces
		 * @generated
		 */
		XML_NAMESPACES("XMLNamespaces"), //$NON-NLS-1$

		/**
		 * AssignmentStatement
		 * @generated
		 */
		ASSIGNMENT_STATEMENT("AssignmentStatement"), //$NON-NLS-1$

		/**
		 * ScalarSubquery
		 * @generated
		 */
		SCALAR_SUBQUERY("ScalarSubquery"), //$NON-NLS-1$

		/**
		 * GroupSymbol
		 * @generated
		 */
		GROUP_SYMBOL("GroupSymbol"), //$NON-NLS-1$

		/**
		 * Constant
		 * @generated
		 */
		CONSTANT("Constant"), //$NON-NLS-1$

		/**
		 * ElementSymbol
		 * @generated
		 */
		ELEMENT_SYMBOL("ElementSymbol"), //$NON-NLS-1$

		/**
		 * Block
		 * @generated
		 */
		BLOCK("Block"), //$NON-NLS-1$

		/**
		 * ExpressionCriteria
		 * @generated
		 */
		EXPRESSION_CRITERIA("ExpressionCriteria"), //$NON-NLS-1$

		/**
		 * AliasSymbol
		 * @generated
		 */
		ALIAS_SYMBOL("AliasSymbol"), //$NON-NLS-1$

		/**
		 * AggregateSymbol
		 * @generated
		 */
		AGGREGATE_SYMBOL("AggregateSymbol"), //$NON-NLS-1$

		/**
		 * WindowFunction
		 * @generated
		 */
		WINDOW_FUNCTION("WindowFunction"), //$NON-NLS-1$

		/**
		 * WindowSpecification
		 * @generated
		 */
		WINDOW_SPECIFICATION("WindowSpecification"), //$NON-NLS-1$

		/**
		 * TextLine
		 * @generated
		 */
		TEXT_LINE("TextLine"), //$NON-NLS-1$

		/**
		 * AlterTrigger
		 * @generated
		 */
		ALTER_TRIGGER("AlterTrigger"), //$NON-NLS-1$

		/**
		 * AlterProcedure
		 * @generated
		 */
		ALTER_PROCEDURE("AlterProcedure"), //$NON-NLS-1$

		/**
		 * AlterView
		 * @generated
		 */
		ALTER_VIEW("AlterView"), //$NON-NLS-1$

		/**
		 * RaiseStatement
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		RAISE_STATEMENT("RaiseStatement"), //$NON-NLS-1$

		/**
		 * ExceptionExpression
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		EXCEPTION_EXPRESSION("ExceptionExpression"), //$NON-NLS-1$

		/**
		 * ReturnStatement
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		RETURN_STATEMENT("ReturnStatement"), //$NON-NLS-1$

		/**
		 * CreateProcedureCommand
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		CREATE_PROCEDURE_COMMAND("CreateProcedureCommand"), //$NON-NLS-1$

		/**
		 * ObjectTable
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		OBJECT_TABLE("ObjectTable"), //$NON-NLS-1$

		/**
		 * ObjectColumn
		 * @generated
		 */
		OBJECT_COLUMN("ObjectColumn"), //$NON-NLS-1$

		/**
		 * JSONObject
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		JSON_OBJECT("JSONObject"), //$NON-NLS-1$

		/**
		 * Array
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		ARRAY("Array"); //$NON-NLS-1$

		private String name;

		ASTNodes(String name) {
			this.name = name;
		}

		/**
		 * @return Name of this common node
		 */
		public String getName() {
			return name;
		}
	}
	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TriggerAction createTriggerAction(TeiidParser teiidParser, int nodeType) {
			return new TriggerAction(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Drop createDrop(TeiidParser teiidParser, int nodeType) {
			return new Drop(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Create createCreate(TeiidParser teiidParser, int nodeType) {
			return new Create(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private RaiseErrorStatement createRaiseErrorStatement(TeiidParser teiidParser, int nodeType) {
			return new RaiseErrorStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private BranchingStatement createBranchingStatement(TeiidParser teiidParser, int nodeType) {
			return new BranchingStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WhileStatement createWhileStatement(TeiidParser teiidParser, int nodeType) {
			return new WhileStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private LoopStatement createLoopStatement(TeiidParser teiidParser, int nodeType) {
			return new LoopStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private IfStatement createIfStatement(TeiidParser teiidParser, int nodeType) {
			return new IfStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CriteriaSelector createCriteriaSelector(TeiidParser teiidParser, int nodeType) {
			return new CriteriaSelector(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private HasCriteria createHasCriteria(TeiidParser teiidParser, int nodeType) {
			return new HasCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DeclareStatement createDeclareStatement(TeiidParser teiidParser, int nodeType) {
			return new DeclareStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CommandStatement createCommandStatement(TeiidParser teiidParser, int nodeType) {
			return new CommandStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TranslateCriteria createTranslateCriteria(TeiidParser teiidParser, int nodeType) {
			return new TranslateCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CreateUpdateProcedureCommand createCreateUpdateProcedureCommand(TeiidParser teiidParser, int nodeType) {
			return new CreateUpdateProcedureCommand(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DynamicCommand createDynamicCommand(TeiidParser teiidParser, int nodeType) {
			return new DynamicCommand(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetClauseList createSetClauseList(TeiidParser teiidParser, int nodeType) {
			return new SetClauseList(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetClause createSetClause(TeiidParser teiidParser, int nodeType) {
			return new SetClause(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ProjectedColumn createProjectedColumn(TeiidParser teiidParser, int nodeType) {
			return new ProjectedColumn(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private StoredProcedure createStoredProcedure(TeiidParser teiidParser, int nodeType) {
			return new StoredProcedure(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Insert createInsert(TeiidParser teiidParser, int nodeType) {
			return new Insert(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Update createUpdate(TeiidParser teiidParser, int nodeType) {
			return new Update(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Delete createDelete(TeiidParser teiidParser, int nodeType) {
			return new Delete(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WithQueryCommand createWithQueryCommand(TeiidParser teiidParser, int nodeType) {
			return new WithQueryCommand(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetQuery createSetQuery(TeiidParser teiidParser, int nodeType) {
			return new SetQuery(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Query createQuery(TeiidParser teiidParser, int nodeType) {
			return new Query(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Into createInto(TeiidParser teiidParser, int nodeType) {
			return new Into(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Select createSelect(TeiidParser teiidParser, int nodeType) {
			return new Select(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DerivedColumn createDerivedColumn(TeiidParser teiidParser, int nodeType) {
			return new DerivedColumn(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private MultipleElementSymbol createMultipleElementSymbol(TeiidParser teiidParser, int nodeType) {
			return new MultipleElementSymbol(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private From createFrom(TeiidParser teiidParser, int nodeType) {
			return new From(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private JoinPredicate createJoinPredicate(TeiidParser teiidParser, int nodeType) {
			return new JoinPredicate(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private JoinType createJoinType(TeiidParser teiidParser, int nodeType) {
			return new JoinType(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLSerialize createXMLSerialize(TeiidParser teiidParser, int nodeType) {
			return new XMLSerialize(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ArrayTable createArrayTable(TeiidParser teiidParser, int nodeType) {
			return new ArrayTable(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TextTable createTextTable(TeiidParser teiidParser, int nodeType) {
			return new TextTable(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TextColumn createTextColumn(TeiidParser teiidParser, int nodeType) {
			return new TextColumn(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLQuery createXMLQuery(TeiidParser teiidParser, int nodeType) {
			return new XMLQuery(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLTable createXMLTable(TeiidParser teiidParser, int nodeType) {
			return new XMLTable(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLColumn createXMLColumn(TeiidParser teiidParser, int nodeType) {
			return new XMLColumn(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SubqueryFromClause createSubqueryFromClause(TeiidParser teiidParser, int nodeType) {
			return new SubqueryFromClause(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private UnaryFromClause createUnaryFromClause(TeiidParser teiidParser, int nodeType) {
			return new UnaryFromClause(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Criteria createCriteria(TeiidParser teiidParser, int nodeType) {
			return new Criteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CompoundCriteria createCompoundCriteria(TeiidParser teiidParser, int nodeType) {
			return new CompoundCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private NotCriteria createNotCriteria(TeiidParser teiidParser, int nodeType) {
			return new NotCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CompareCriteria createCompareCriteria(TeiidParser teiidParser, int nodeType) {
			return new CompareCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SubqueryCompareCriteria createSubqueryCompareCriteria(TeiidParser teiidParser, int nodeType) {
			return new SubqueryCompareCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private MatchCriteria createMatchCriteria(TeiidParser teiidParser, int nodeType) {
			return new MatchCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private BetweenCriteria createBetweenCriteria(TeiidParser teiidParser, int nodeType) {
			return new BetweenCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private IsNullCriteria createIsNullCriteria(TeiidParser teiidParser, int nodeType) {
			return new IsNullCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SubquerySetCriteria createSubquerySetCriteria(TeiidParser teiidParser, int nodeType) {
			return new SubquerySetCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetCriteria createSetCriteria(TeiidParser teiidParser, int nodeType) {
			return new SetCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExistsCriteria createExistsCriteria(TeiidParser teiidParser, int nodeType) {
			return new ExistsCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private GroupBy createGroupBy(TeiidParser teiidParser, int nodeType) {
			return new GroupBy(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private OrderBy createOrderBy(TeiidParser teiidParser, int nodeType) {
			return new OrderBy(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private OrderByItem createOrderByItem(TeiidParser teiidParser, int nodeType) {
			return new OrderByItem(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExpressionSymbol createExpressionSymbol(TeiidParser teiidParser, int nodeType) {
			return new ExpressionSymbol(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Limit createLimit(TeiidParser teiidParser, int nodeType) {
			return new Limit(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Option createOption(TeiidParser teiidParser, int nodeType) {
			return new Option(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Reference createReference(TeiidParser teiidParser, int nodeType) {
			return new Reference(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CaseExpression createCaseExpression(TeiidParser teiidParser, int nodeType) {
			return new CaseExpression(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SearchedCaseExpression createSearchedCaseExpression(TeiidParser teiidParser, int nodeType) {
			return new SearchedCaseExpression(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Function createFunction(TeiidParser teiidParser, int nodeType) {
			return new Function(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLParse createXMLParse(TeiidParser teiidParser, int nodeType) {
			return new XMLParse(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private QueryString createQueryString(TeiidParser teiidParser, int nodeType) {
			return new QueryString(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLElement createXMLElement(TeiidParser teiidParser, int nodeType) {
			return new XMLElement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLAttributes createXMLAttributes(TeiidParser teiidParser, int nodeType) {
			return new XMLAttributes(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLForest createXMLForest(TeiidParser teiidParser, int nodeType) {
			return new XMLForest(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLNamespaces createXMLNamespaces(TeiidParser teiidParser, int nodeType) {
			return new XMLNamespaces(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AssignmentStatement createAssignmentStatement(TeiidParser teiidParser, int nodeType) {
			return new AssignmentStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ScalarSubquery createScalarSubquery(TeiidParser teiidParser, int nodeType) {
			return new ScalarSubquery(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private GroupSymbol createGroupSymbol(TeiidParser teiidParser, int nodeType) {
			return new GroupSymbol(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Constant createConstant(TeiidParser teiidParser, int nodeType) {
			return new Constant(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ElementSymbol createElementSymbol(TeiidParser teiidParser, int nodeType) {
			return new ElementSymbol(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Block createBlock(TeiidParser teiidParser, int nodeType) {
			return new Block(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExpressionCriteria createExpressionCriteria(TeiidParser teiidParser, int nodeType) {
			return new ExpressionCriteria(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AliasSymbol createAliasSymbol(TeiidParser teiidParser, int nodeType) {
			return new AliasSymbol(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AggregateSymbol createAggregateSymbol(TeiidParser teiidParser, int nodeType) {
		if (isTeiid7Parser(teiidParser))
			return new Aggregate7Symbol((Teiid7Parser) teiidParser, nodeType);
		else if (isTeiid8Parser(teiidParser))
			return new Aggregate8Symbol((Teiid8Parser) teiidParser, nodeType);
		throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));

	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WindowFunction createWindowFunction(TeiidParser teiidParser, int nodeType) {
		if (isTeiid7Parser(teiidParser))
			return new Window7Function((Teiid7Parser) teiidParser, nodeType);
		else if (isTeiid8Parser(teiidParser))
			return new Window8Function((Teiid8Parser) teiidParser, nodeType);
		throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));

	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WindowSpecification createWindowSpecification(TeiidParser teiidParser, int nodeType) {
			return new WindowSpecification(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TextLine createTextLine(TeiidParser teiidParser, int nodeType) {
			return new TextLine(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AlterTrigger createAlterTrigger(TeiidParser teiidParser, int nodeType) {
			return new AlterTrigger(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AlterProcedure createAlterProcedure(TeiidParser teiidParser, int nodeType) {
		if (isTeiid7Parser(teiidParser))
			return new Alter7Procedure((Teiid7Parser) teiidParser, nodeType);
		else if (isTeiid8Parser(teiidParser))
			return new Alter8Procedure((Teiid8Parser) teiidParser, nodeType);
		throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));

	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AlterView createAlterView(TeiidParser teiidParser, int nodeType) {
			return new AlterView(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private RaiseStatement createRaiseStatement(TeiidParser teiidParser, int nodeType) {
			return new RaiseStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExceptionExpression createExceptionExpression(TeiidParser teiidParser, int nodeType) {
			return new ExceptionExpression(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ReturnStatement createReturnStatement(TeiidParser teiidParser, int nodeType) {
			return new ReturnStatement(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CreateProcedureCommand createCreateProcedureCommand(TeiidParser teiidParser, int nodeType) {
			return new CreateProcedureCommand(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ObjectTable createObjectTable(TeiidParser teiidParser, int nodeType) {
			return new ObjectTable(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ObjectColumn createObjectColumn(TeiidParser teiidParser, int nodeType) {
			return new ObjectColumn(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private JSONObject createJSONObject(TeiidParser teiidParser, int nodeType) {
			return new JSONObject(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private Array createArray(TeiidParser teiidParser, int nodeType) {
			return new Array(teiidParser, nodeType);
	}

/**
 * Create a version 7 teiid parser node for the given node type.
 *
 * @generated
 *
 * @param teiidParser
 * @param nodeType
 * @return version 7 teiid parser node
 */
private <T extends LanguageObject> T create(Teiid7Parser teiidParser, int nodeType) {
		switch (nodeType) {
			case Teiid7ParserTreeConstants.JJTTRIGGERACTION:
				return (T) createTriggerAction(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTDROP:
				return (T) createDrop(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCREATE:
				return (T) createCreate(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTRAISEERRORSTATEMENT:
				return (T) createRaiseErrorStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTBRANCHINGSTATEMENT:
				return (T) createBranchingStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTWHILESTATEMENT:
				return (T) createWhileStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTLOOPSTATEMENT:
				return (T) createLoopStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTIFSTATEMENT:
				return (T) createIfStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCRITERIASELECTOR:
				return (T) createCriteriaSelector(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTHASCRITERIA:
				return (T) createHasCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTDECLARESTATEMENT:
				return (T) createDeclareStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCOMMANDSTATEMENT:
				return (T) createCommandStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTTRANSLATECRITERIA:
				return (T) createTranslateCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCREATEUPDATEPROCEDURECOMMAND:
				return (T) createCreateUpdateProcedureCommand(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTDYNAMICCOMMAND:
				return (T) createDynamicCommand(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSETCLAUSELIST:
				return (T) createSetClauseList(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSETCLAUSE:
				return (T) createSetClause(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTPROJECTEDCOLUMN:
				return (T) createProjectedColumn(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSTOREDPROCEDURE:
				return (T) createStoredProcedure(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTINSERT:
				return (T) createInsert(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTUPDATE:
				return (T) createUpdate(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTDELETE:
				return (T) createDelete(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTWITHQUERYCOMMAND:
				return (T) createWithQueryCommand(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSETQUERY:
				return (T) createSetQuery(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTQUERY:
				return (T) createQuery(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTINTO:
				return (T) createInto(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSELECT:
				return (T) createSelect(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTDERIVEDCOLUMN:
				return (T) createDerivedColumn(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTMULTIPLEELEMENTSYMBOL:
				return (T) createMultipleElementSymbol(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTFROM:
				return (T) createFrom(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTJOINPREDICATE:
				return (T) createJoinPredicate(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTJOINTYPE:
				return (T) createJoinType(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLSERIALIZE:
				return (T) createXMLSerialize(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTARRAYTABLE:
				return (T) createArrayTable(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTTEXTTABLE:
				return (T) createTextTable(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTTEXTCOLUMN:
				return (T) createTextColumn(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLQUERY:
				return (T) createXMLQuery(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLTABLE:
				return (T) createXMLTable(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLCOLUMN:
				return (T) createXMLColumn(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSUBQUERYFROMCLAUSE:
				return (T) createSubqueryFromClause(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTUNARYFROMCLAUSE:
				return (T) createUnaryFromClause(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCRITERIA:
				return (T) createCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCOMPOUNDCRITERIA:
				return (T) createCompoundCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTNOTCRITERIA:
				return (T) createNotCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCOMPARECRITERIA:
				return (T) createCompareCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSUBQUERYCOMPARECRITERIA:
				return (T) createSubqueryCompareCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTMATCHCRITERIA:
				return (T) createMatchCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTBETWEENCRITERIA:
				return (T) createBetweenCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTISNULLCRITERIA:
				return (T) createIsNullCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSUBQUERYSETCRITERIA:
				return (T) createSubquerySetCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSETCRITERIA:
				return (T) createSetCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTEXISTSCRITERIA:
				return (T) createExistsCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTGROUPBY:
				return (T) createGroupBy(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTORDERBY:
				return (T) createOrderBy(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTORDERBYITEM:
				return (T) createOrderByItem(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTEXPRESSIONSYMBOL:
				return (T) createExpressionSymbol(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTLIMIT:
				return (T) createLimit(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTOPTION:
				return (T) createOption(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTREFERENCE:
				return (T) createReference(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCASEEXPRESSION:
				return (T) createCaseExpression(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSEARCHEDCASEEXPRESSION:
				return (T) createSearchedCaseExpression(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTFUNCTION:
				return (T) createFunction(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLPARSE:
				return (T) createXMLParse(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTQUERYSTRING:
				return (T) createQueryString(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLELEMENT:
				return (T) createXMLElement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLATTRIBUTES:
				return (T) createXMLAttributes(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLFOREST:
				return (T) createXMLForest(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTXMLNAMESPACES:
				return (T) createXMLNamespaces(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTASSIGNMENTSTATEMENT:
				return (T) createAssignmentStatement(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTSCALARSUBQUERY:
				return (T) createScalarSubquery(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTGROUPSYMBOL:
				return (T) createGroupSymbol(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTCONSTANT:
				return (T) createConstant(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTELEMENTSYMBOL:
				return (T) createElementSymbol(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTBLOCK:
				return (T) createBlock(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTEXPRESSIONCRITERIA:
				return (T) createExpressionCriteria(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTALIASSYMBOL:
				return (T) createAliasSymbol(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTAGGREGATESYMBOL:
				return (T) createAggregateSymbol(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTWINDOWFUNCTION:
				return (T) createWindowFunction(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTWINDOWSPECIFICATION:
				return (T) createWindowSpecification(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTTEXTLINE:
				return (T) createTextLine(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTALTERTRIGGER:
				return (T) createAlterTrigger(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTALTERPROCEDURE:
				return (T) createAlterProcedure(teiidParser, nodeType);
			case Teiid7ParserTreeConstants.JJTALTERVIEW:
				return (T) createAlterView(teiidParser, nodeType);
		default:
			throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
		}
	}

/**
 * Create a version 8 teiid parser node for the given node type.
 *
 * @generated
 *
 * @param teiidParser
 * @param nodeType
 * @return version 8 teiid parser node
 */
private <T extends LanguageObject> T create(Teiid8Parser teiidParser, int nodeType) {
		switch (nodeType) {
			case Teiid8ParserTreeConstants.JJTTRIGGERACTION:
				return (T) createTriggerAction(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTDROP:
				return (T) createDrop(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCREATE:
				return (T) createCreate(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTRAISESTATEMENT:
				return (T) createRaiseStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTEXCEPTIONEXPRESSION:
				return (T) createExceptionExpression(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTBRANCHINGSTATEMENT:
				return (T) createBranchingStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTRETURNSTATEMENT:
				return (T) createReturnStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTWHILESTATEMENT:
				return (T) createWhileStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTLOOPSTATEMENT:
				return (T) createLoopStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTIFSTATEMENT:
				return (T) createIfStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTDECLARESTATEMENT:
				return (T) createDeclareStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCOMMANDSTATEMENT:
				return (T) createCommandStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCREATEPROCEDURECOMMAND:
				return (T) createCreateProcedureCommand(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTDYNAMICCOMMAND:
				return (T) createDynamicCommand(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSETCLAUSELIST:
				return (T) createSetClauseList(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSETCLAUSE:
				return (T) createSetClause(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTPROJECTEDCOLUMN:
				return (T) createProjectedColumn(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSTOREDPROCEDURE:
				return (T) createStoredProcedure(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTINSERT:
				return (T) createInsert(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTUPDATE:
				return (T) createUpdate(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTDELETE:
				return (T) createDelete(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTWITHQUERYCOMMAND:
				return (T) createWithQueryCommand(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSETQUERY:
				return (T) createSetQuery(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTQUERY:
				return (T) createQuery(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTINTO:
				return (T) createInto(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSELECT:
				return (T) createSelect(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTEXPRESSIONSYMBOL:
				return (T) createExpressionSymbol(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTDERIVEDCOLUMN:
				return (T) createDerivedColumn(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTMULTIPLEELEMENTSYMBOL:
				return (T) createMultipleElementSymbol(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTFROM:
				return (T) createFrom(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTJOINPREDICATE:
				return (T) createJoinPredicate(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTJOINTYPE:
				return (T) createJoinType(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLSERIALIZE:
				return (T) createXMLSerialize(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTARRAYTABLE:
				return (T) createArrayTable(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTTEXTTABLE:
				return (T) createTextTable(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTTEXTCOLUMN:
				return (T) createTextColumn(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLQUERY:
				return (T) createXMLQuery(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTOBJECTTABLE:
				return (T) createObjectTable(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTOBJECTCOLUMN:
				return (T) createObjectColumn(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLTABLE:
				return (T) createXMLTable(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLCOLUMN:
				return (T) createXMLColumn(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSUBQUERYFROMCLAUSE:
				return (T) createSubqueryFromClause(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTUNARYFROMCLAUSE:
				return (T) createUnaryFromClause(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCRITERIA:
				return (T) createCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCOMPOUNDCRITERIA:
				return (T) createCompoundCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTNOTCRITERIA:
				return (T) createNotCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCOMPARECRITERIA:
				return (T) createCompareCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSUBQUERYCOMPARECRITERIA:
				return (T) createSubqueryCompareCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTMATCHCRITERIA:
				return (T) createMatchCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTBETWEENCRITERIA:
				return (T) createBetweenCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTISNULLCRITERIA:
				return (T) createIsNullCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSUBQUERYSETCRITERIA:
				return (T) createSubquerySetCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSETCRITERIA:
				return (T) createSetCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTEXISTSCRITERIA:
				return (T) createExistsCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTGROUPBY:
				return (T) createGroupBy(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTORDERBY:
				return (T) createOrderBy(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTORDERBYITEM:
				return (T) createOrderByItem(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTLIMIT:
				return (T) createLimit(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTOPTION:
				return (T) createOption(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTREFERENCE:
				return (T) createReference(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCASEEXPRESSION:
				return (T) createCaseExpression(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSEARCHEDCASEEXPRESSION:
				return (T) createSearchedCaseExpression(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTFUNCTION:
				return (T) createFunction(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLPARSE:
				return (T) createXMLParse(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTQUERYSTRING:
				return (T) createQueryString(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLELEMENT:
				return (T) createXMLElement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLATTRIBUTES:
				return (T) createXMLAttributes(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTJSONOBJECT:
				return (T) createJSONObject(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLFOREST:
				return (T) createXMLForest(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTXMLNAMESPACES:
				return (T) createXMLNamespaces(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTASSIGNMENTSTATEMENT:
				return (T) createAssignmentStatement(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTSCALARSUBQUERY:
				return (T) createScalarSubquery(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTGROUPSYMBOL:
				return (T) createGroupSymbol(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTCONSTANT:
				return (T) createConstant(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTELEMENTSYMBOL:
				return (T) createElementSymbol(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTBLOCK:
				return (T) createBlock(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTEXPRESSIONCRITERIA:
				return (T) createExpressionCriteria(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTALIASSYMBOL:
				return (T) createAliasSymbol(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTAGGREGATESYMBOL:
				return (T) createAggregateSymbol(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTWINDOWFUNCTION:
				return (T) createWindowFunction(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTWINDOWSPECIFICATION:
				return (T) createWindowSpecification(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTTEXTLINE:
				return (T) createTextLine(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTALTERTRIGGER:
				return (T) createAlterTrigger(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTALTERPROCEDURE:
				return (T) createAlterProcedure(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTALTERVIEW:
				return (T) createAlterView(teiidParser, nodeType);
			case Teiid8ParserTreeConstants.JJTARRAY:
				return (T) createArray(teiidParser, nodeType);
		default:
			throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
		}
	}


}