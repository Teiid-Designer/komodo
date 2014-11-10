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

import org.teiid.query.parser.v7.Teiid7ClientParser;
import org.teiid.query.parser.v8.Teiid8ClientParser;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.runtime.client.Messages;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
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


	/**
	 * @return singleton instance
	 */
	public static TeiidNodeFactory getInstance() {
		if (instance == null) instance = new TeiidNodeFactory();
		return instance;
	}


	private static boolean isTeiid7ClientParser(TeiidClientParser teiidParser) {
		return teiidParser instanceof Teiid7ClientParser;
	}


	private static boolean isTeiid8ClientParser(TeiidClientParser teiidParser) {
		return teiidParser instanceof Teiid8ClientParser;
	}

	/**
	 * Method used by the generated parsers for constructing nodes
	 *
	 * @param teiidParser
	 * @param nodeType
	 *
	 * @return created language object
	 */
	public static LanguageObject jjtCreate(TeiidClientParser teiidParser, int nodeType) {
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
	public <T extends LanguageObject> T create(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return create((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return create((Teiid8ClientParser) teiidParser, nodeType);
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
	public <T extends LanguageObject> T create(TeiidClientParser teiidParser, ASTNodes nodeType) {

		if (isTeiid7ClientParser(teiidParser)) {
			for (int i = 0; i < Teiid7ClientParserTreeConstants.jjtNodeName.length; ++i) {
				String constantName = Teiid7ClientParserTreeConstants.jjtNodeName[i];
					if (! constantName.equalsIgnoreCase(nodeType.getName()))
						continue;

				return create(teiidParser, i);
			}
		}

		else if (isTeiid8ClientParser(teiidParser)) {
			for (int i = 0; i < Teiid8ClientParserTreeConstants.jjtNodeName.length; ++i) {
				String constantName = Teiid8ClientParserTreeConstants.jjtNodeName[i];
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
	private TriggerAction createTriggerAction(TeiidClientParser teiidParser, int nodeType) {
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
	private Drop createDrop(TeiidClientParser teiidParser, int nodeType) {
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
	private Create createCreate(TeiidClientParser teiidParser, int nodeType) {
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
	private RaiseErrorStatement createRaiseErrorStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private BranchingStatement createBranchingStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private WhileStatement createWhileStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private LoopStatement createLoopStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private IfStatement createIfStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private CriteriaSelector createCriteriaSelector(TeiidClientParser teiidParser, int nodeType) {
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
	private HasCriteria createHasCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private DeclareStatement createDeclareStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private CommandStatement createCommandStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private TranslateCriteria createTranslateCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private CreateUpdateProcedureCommand createCreateUpdateProcedureCommand(TeiidClientParser teiidParser, int nodeType) {
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
	private DynamicCommand createDynamicCommand(TeiidClientParser teiidParser, int nodeType) {
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
	private SetClauseList createSetClauseList(TeiidClientParser teiidParser, int nodeType) {
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
	private SetClause createSetClause(TeiidClientParser teiidParser, int nodeType) {
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
	private ProjectedColumn createProjectedColumn(TeiidClientParser teiidParser, int nodeType) {
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
	private StoredProcedure createStoredProcedure(TeiidClientParser teiidParser, int nodeType) {
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
	private Insert createInsert(TeiidClientParser teiidParser, int nodeType) {
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
	private Update createUpdate(TeiidClientParser teiidParser, int nodeType) {
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
	private Delete createDelete(TeiidClientParser teiidParser, int nodeType) {
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
	private WithQueryCommand createWithQueryCommand(TeiidClientParser teiidParser, int nodeType) {
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
	private SetQuery createSetQuery(TeiidClientParser teiidParser, int nodeType) {
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
	private Query createQuery(TeiidClientParser teiidParser, int nodeType) {
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
	private Into createInto(TeiidClientParser teiidParser, int nodeType) {
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
	private Select createSelect(TeiidClientParser teiidParser, int nodeType) {
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
	private DerivedColumn createDerivedColumn(TeiidClientParser teiidParser, int nodeType) {
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
	private MultipleElementSymbol createMultipleElementSymbol(TeiidClientParser teiidParser, int nodeType) {
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
	private From createFrom(TeiidClientParser teiidParser, int nodeType) {
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
	private JoinPredicate createJoinPredicate(TeiidClientParser teiidParser, int nodeType) {
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
	private JoinType createJoinType(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLSerialize createXMLSerialize(TeiidClientParser teiidParser, int nodeType) {
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
	private ArrayTable createArrayTable(TeiidClientParser teiidParser, int nodeType) {
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
	private TextTable createTextTable(TeiidClientParser teiidParser, int nodeType) {
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
	private TextColumn createTextColumn(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLQuery createXMLQuery(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLTable createXMLTable(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLColumn createXMLColumn(TeiidClientParser teiidParser, int nodeType) {
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
	private SubqueryFromClause createSubqueryFromClause(TeiidClientParser teiidParser, int nodeType) {
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
	private UnaryFromClause createUnaryFromClause(TeiidClientParser teiidParser, int nodeType) {
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
	private Criteria createCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private CompoundCriteria createCompoundCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private NotCriteria createNotCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private CompareCriteria createCompareCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private SubqueryCompareCriteria createSubqueryCompareCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private MatchCriteria createMatchCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private BetweenCriteria createBetweenCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private IsNullCriteria createIsNullCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private SubquerySetCriteria createSubquerySetCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private SetCriteria createSetCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private ExistsCriteria createExistsCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private GroupBy createGroupBy(TeiidClientParser teiidParser, int nodeType) {
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
	private OrderBy createOrderBy(TeiidClientParser teiidParser, int nodeType) {
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
	private OrderByItem createOrderByItem(TeiidClientParser teiidParser, int nodeType) {
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
	private ExpressionSymbol createExpressionSymbol(TeiidClientParser teiidParser, int nodeType) {
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
	private Limit createLimit(TeiidClientParser teiidParser, int nodeType) {
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
	private Option createOption(TeiidClientParser teiidParser, int nodeType) {
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
	private Reference createReference(TeiidClientParser teiidParser, int nodeType) {
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
	private CaseExpression createCaseExpression(TeiidClientParser teiidParser, int nodeType) {
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
	private SearchedCaseExpression createSearchedCaseExpression(TeiidClientParser teiidParser, int nodeType) {
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
	private Function createFunction(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLParse createXMLParse(TeiidClientParser teiidParser, int nodeType) {
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
	private QueryString createQueryString(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLElement createXMLElement(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLAttributes createXMLAttributes(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLForest createXMLForest(TeiidClientParser teiidParser, int nodeType) {
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
	private XMLNamespaces createXMLNamespaces(TeiidClientParser teiidParser, int nodeType) {
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
	private AssignmentStatement createAssignmentStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private ScalarSubquery createScalarSubquery(TeiidClientParser teiidParser, int nodeType) {
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
	private GroupSymbol createGroupSymbol(TeiidClientParser teiidParser, int nodeType) {
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
	private Constant createConstant(TeiidClientParser teiidParser, int nodeType) {
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
	private ElementSymbol createElementSymbol(TeiidClientParser teiidParser, int nodeType) {
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
	private Block createBlock(TeiidClientParser teiidParser, int nodeType) {
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
	private ExpressionCriteria createExpressionCriteria(TeiidClientParser teiidParser, int nodeType) {
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
	private AliasSymbol createAliasSymbol(TeiidClientParser teiidParser, int nodeType) {
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
	private AggregateSymbol createAggregateSymbol(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return new Aggregate7Symbol((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return new Aggregate8Symbol((Teiid8ClientParser) teiidParser, nodeType);
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
	private WindowFunction createWindowFunction(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return new Window7Function((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return new Window8Function((Teiid8ClientParser) teiidParser, nodeType);
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
	private WindowSpecification createWindowSpecification(TeiidClientParser teiidParser, int nodeType) {
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
	private TextLine createTextLine(TeiidClientParser teiidParser, int nodeType) {
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
	private AlterTrigger createAlterTrigger(TeiidClientParser teiidParser, int nodeType) {
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
	private AlterProcedure createAlterProcedure(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return new Alter7Procedure((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return new Alter8Procedure((Teiid8ClientParser) teiidParser, nodeType);
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
	private AlterView createAlterView(TeiidClientParser teiidParser, int nodeType) {
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
	private RaiseStatement createRaiseStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private ExceptionExpression createExceptionExpression(TeiidClientParser teiidParser, int nodeType) {
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
	private ReturnStatement createReturnStatement(TeiidClientParser teiidParser, int nodeType) {
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
	private CreateProcedureCommand createCreateProcedureCommand(TeiidClientParser teiidParser, int nodeType) {
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
	private ObjectTable createObjectTable(TeiidClientParser teiidParser, int nodeType) {
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
	private ObjectColumn createObjectColumn(TeiidClientParser teiidParser, int nodeType) {
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
	private JSONObject createJSONObject(TeiidClientParser teiidParser, int nodeType) {
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
	private Array createArray(TeiidClientParser teiidParser, int nodeType) {
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
private <T extends LanguageObject> T create(Teiid7ClientParser teiidParser, int nodeType) {
		switch (nodeType) {
			case Teiid7ClientParserTreeConstants.JJTTRIGGERACTION:
				return (T) createTriggerAction(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDROP:
				return (T) createDrop(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCREATE:
				return (T) createCreate(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTRAISEERRORSTATEMENT:
				return (T) createRaiseErrorStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBRANCHINGSTATEMENT:
				return (T) createBranchingStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTWHILESTATEMENT:
				return (T) createWhileStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTLOOPSTATEMENT:
				return (T) createLoopStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTIFSTATEMENT:
				return (T) createIfStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCRITERIASELECTOR:
				return (T) createCriteriaSelector(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTHASCRITERIA:
				return (T) createHasCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDECLARESTATEMENT:
				return (T) createDeclareStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCOMMANDSTATEMENT:
				return (T) createCommandStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTRANSLATECRITERIA:
				return (T) createTranslateCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCREATEUPDATEPROCEDURECOMMAND:
				return (T) createCreateUpdateProcedureCommand(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDYNAMICCOMMAND:
				return (T) createDynamicCommand(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETCLAUSELIST:
				return (T) createSetClauseList(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETCLAUSE:
				return (T) createSetClause(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTPROJECTEDCOLUMN:
				return (T) createProjectedColumn(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSTOREDPROCEDURE:
				return (T) createStoredProcedure(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTINSERT:
				return (T) createInsert(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTUPDATE:
				return (T) createUpdate(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDELETE:
				return (T) createDelete(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTWITHQUERYCOMMAND:
				return (T) createWithQueryCommand(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETQUERY:
				return (T) createSetQuery(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTQUERY:
				return (T) createQuery(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTINTO:
				return (T) createInto(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSELECT:
				return (T) createSelect(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDERIVEDCOLUMN:
				return (T) createDerivedColumn(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTMULTIPLEELEMENTSYMBOL:
				return (T) createMultipleElementSymbol(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTFROM:
				return (T) createFrom(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTJOINPREDICATE:
				return (T) createJoinPredicate(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTJOINTYPE:
				return (T) createJoinType(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLSERIALIZE:
				return (T) createXMLSerialize(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTARRAYTABLE:
				return (T) createArrayTable(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTEXTTABLE:
				return (T) createTextTable(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTEXTCOLUMN:
				return (T) createTextColumn(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLQUERY:
				return (T) createXMLQuery(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLTABLE:
				return (T) createXMLTable(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLCOLUMN:
				return (T) createXMLColumn(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSUBQUERYFROMCLAUSE:
				return (T) createSubqueryFromClause(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTUNARYFROMCLAUSE:
				return (T) createUnaryFromClause(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCRITERIA:
				return (T) createCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCOMPOUNDCRITERIA:
				return (T) createCompoundCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTNOTCRITERIA:
				return (T) createNotCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCOMPARECRITERIA:
				return (T) createCompareCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSUBQUERYCOMPARECRITERIA:
				return (T) createSubqueryCompareCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTMATCHCRITERIA:
				return (T) createMatchCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBETWEENCRITERIA:
				return (T) createBetweenCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTISNULLCRITERIA:
				return (T) createIsNullCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSUBQUERYSETCRITERIA:
				return (T) createSubquerySetCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETCRITERIA:
				return (T) createSetCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTEXISTSCRITERIA:
				return (T) createExistsCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTGROUPBY:
				return (T) createGroupBy(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTORDERBY:
				return (T) createOrderBy(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTORDERBYITEM:
				return (T) createOrderByItem(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTEXPRESSIONSYMBOL:
				return (T) createExpressionSymbol(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTLIMIT:
				return (T) createLimit(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTOPTION:
				return (T) createOption(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTREFERENCE:
				return (T) createReference(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCASEEXPRESSION:
				return (T) createCaseExpression(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSEARCHEDCASEEXPRESSION:
				return (T) createSearchedCaseExpression(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTFUNCTION:
				return (T) createFunction(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLPARSE:
				return (T) createXMLParse(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTQUERYSTRING:
				return (T) createQueryString(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLELEMENT:
				return (T) createXMLElement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLATTRIBUTES:
				return (T) createXMLAttributes(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLFOREST:
				return (T) createXMLForest(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLNAMESPACES:
				return (T) createXMLNamespaces(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTASSIGNMENTSTATEMENT:
				return (T) createAssignmentStatement(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSCALARSUBQUERY:
				return (T) createScalarSubquery(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTGROUPSYMBOL:
				return (T) createGroupSymbol(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCONSTANT:
				return (T) createConstant(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTELEMENTSYMBOL:
				return (T) createElementSymbol(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBLOCK:
				return (T) createBlock(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTEXPRESSIONCRITERIA:
				return (T) createExpressionCriteria(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALIASSYMBOL:
				return (T) createAliasSymbol(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTAGGREGATESYMBOL:
				return (T) createAggregateSymbol(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTWINDOWFUNCTION:
				return (T) createWindowFunction(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTWINDOWSPECIFICATION:
				return (T) createWindowSpecification(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTEXTLINE:
				return (T) createTextLine(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALTERTRIGGER:
				return (T) createAlterTrigger(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALTERPROCEDURE:
				return (T) createAlterProcedure(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALTERVIEW:
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
private <T extends LanguageObject> T create(Teiid8ClientParser teiidParser, int nodeType) {
		switch (nodeType) {
			case Teiid8ClientParserTreeConstants.JJTTRIGGERACTION:
				return (T) createTriggerAction(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDROP:
				return (T) createDrop(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCREATE:
				return (T) createCreate(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTRAISESTATEMENT:
				return (T) createRaiseStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXCEPTIONEXPRESSION:
				return (T) createExceptionExpression(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBRANCHINGSTATEMENT:
				return (T) createBranchingStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTRETURNSTATEMENT:
				return (T) createReturnStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTWHILESTATEMENT:
				return (T) createWhileStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTLOOPSTATEMENT:
				return (T) createLoopStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTIFSTATEMENT:
				return (T) createIfStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDECLARESTATEMENT:
				return (T) createDeclareStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCOMMANDSTATEMENT:
				return (T) createCommandStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCREATEPROCEDURECOMMAND:
				return (T) createCreateProcedureCommand(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDYNAMICCOMMAND:
				return (T) createDynamicCommand(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETCLAUSELIST:
				return (T) createSetClauseList(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETCLAUSE:
				return (T) createSetClause(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTPROJECTEDCOLUMN:
				return (T) createProjectedColumn(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSTOREDPROCEDURE:
				return (T) createStoredProcedure(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTINSERT:
				return (T) createInsert(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTUPDATE:
				return (T) createUpdate(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDELETE:
				return (T) createDelete(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTWITHQUERYCOMMAND:
				return (T) createWithQueryCommand(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETQUERY:
				return (T) createSetQuery(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTQUERY:
				return (T) createQuery(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTINTO:
				return (T) createInto(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSELECT:
				return (T) createSelect(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXPRESSIONSYMBOL:
				return (T) createExpressionSymbol(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDERIVEDCOLUMN:
				return (T) createDerivedColumn(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTMULTIPLEELEMENTSYMBOL:
				return (T) createMultipleElementSymbol(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTFROM:
				return (T) createFrom(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTJOINPREDICATE:
				return (T) createJoinPredicate(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTJOINTYPE:
				return (T) createJoinType(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLSERIALIZE:
				return (T) createXMLSerialize(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTARRAYTABLE:
				return (T) createArrayTable(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTTEXTTABLE:
				return (T) createTextTable(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTTEXTCOLUMN:
				return (T) createTextColumn(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLQUERY:
				return (T) createXMLQuery(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTOBJECTTABLE:
				return (T) createObjectTable(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTOBJECTCOLUMN:
				return (T) createObjectColumn(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLTABLE:
				return (T) createXMLTable(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLCOLUMN:
				return (T) createXMLColumn(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSUBQUERYFROMCLAUSE:
				return (T) createSubqueryFromClause(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTUNARYFROMCLAUSE:
				return (T) createUnaryFromClause(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCRITERIA:
				return (T) createCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCOMPOUNDCRITERIA:
				return (T) createCompoundCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTNOTCRITERIA:
				return (T) createNotCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCOMPARECRITERIA:
				return (T) createCompareCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSUBQUERYCOMPARECRITERIA:
				return (T) createSubqueryCompareCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTMATCHCRITERIA:
				return (T) createMatchCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBETWEENCRITERIA:
				return (T) createBetweenCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTISNULLCRITERIA:
				return (T) createIsNullCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSUBQUERYSETCRITERIA:
				return (T) createSubquerySetCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETCRITERIA:
				return (T) createSetCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXISTSCRITERIA:
				return (T) createExistsCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTGROUPBY:
				return (T) createGroupBy(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTORDERBY:
				return (T) createOrderBy(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTORDERBYITEM:
				return (T) createOrderByItem(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTLIMIT:
				return (T) createLimit(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTOPTION:
				return (T) createOption(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTREFERENCE:
				return (T) createReference(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCASEEXPRESSION:
				return (T) createCaseExpression(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSEARCHEDCASEEXPRESSION:
				return (T) createSearchedCaseExpression(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTFUNCTION:
				return (T) createFunction(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLPARSE:
				return (T) createXMLParse(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTQUERYSTRING:
				return (T) createQueryString(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLELEMENT:
				return (T) createXMLElement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLATTRIBUTES:
				return (T) createXMLAttributes(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTJSONOBJECT:
				return (T) createJSONObject(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLFOREST:
				return (T) createXMLForest(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLNAMESPACES:
				return (T) createXMLNamespaces(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTASSIGNMENTSTATEMENT:
				return (T) createAssignmentStatement(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSCALARSUBQUERY:
				return (T) createScalarSubquery(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTGROUPSYMBOL:
				return (T) createGroupSymbol(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCONSTANT:
				return (T) createConstant(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTELEMENTSYMBOL:
				return (T) createElementSymbol(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBLOCK:
				return (T) createBlock(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXPRESSIONCRITERIA:
				return (T) createExpressionCriteria(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALIASSYMBOL:
				return (T) createAliasSymbol(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTAGGREGATESYMBOL:
				return (T) createAggregateSymbol(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTWINDOWFUNCTION:
				return (T) createWindowFunction(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTWINDOWSPECIFICATION:
				return (T) createWindowSpecification(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTTEXTLINE:
				return (T) createTextLine(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALTERTRIGGER:
				return (T) createAlterTrigger(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALTERPROCEDURE:
				return (T) createAlterProcedure(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALTERVIEW:
				return (T) createAlterView(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTARRAY:
				return (T) createArray(teiidParser, nodeType);
		default:
			throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
		}
	}


}