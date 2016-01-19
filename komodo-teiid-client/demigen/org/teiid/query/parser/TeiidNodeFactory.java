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
	 * @param teiidParser teiid parser
	 * @param nodeType node type
	 *
	 * @return created language object
	 */
	public static BaseLanguageObject jjtCreate(TeiidClientParser teiidParser, int nodeType) {
		return getInstance().create(teiidParser, nodeType);
	}

	/**
	 * Create a parser node for the given node type
	 *
	 * @param teiidParser teiid parser
	 * @param nodeType node type
	 *
	 * @return node applicable to the given parser
	 */
	public <T extends BaseLanguageObject> T create(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return create((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return create((Teiid8ClientParser) teiidParser, nodeType);
		throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
	}

	/**
	 * Create a parser node for the node with the given common node name
	 *
	 * @param teiidParser teiid parser
	 * @param nodeType node type
	 *
	 * @return node applicable to the given parser
	 */
	public <T extends BaseLanguageObject> T create(TeiidClientParser teiidParser, ASTNodes nodeType) {

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
		TRIGGER_ACTION("TriggerActionImpl"), //$NON-NLS-1$

		/**
		 * Drop
		 * @generated
		 */
		DROP("DropImpl"), //$NON-NLS-1$

		/**
		 * Create
		 * @generated
		 */
		CREATE("CreateImpl"), //$NON-NLS-1$

		/**
		 * RaiseErrorStatement
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		RAISE_ERROR_STATEMENT("RaiseErrorStatementImpl"), //$NON-NLS-1$

		/**
		 * BranchingStatement
		 * @generated
		 */
		BRANCHING_STATEMENT("BranchingStatementImpl"), //$NON-NLS-1$

		/**
		 * WhileStatement
		 * @generated
		 */
		WHILE_STATEMENT("WhileStatementImpl"), //$NON-NLS-1$

		/**
		 * LoopStatement
		 * @generated
		 */
		LOOP_STATEMENT("LoopStatementImpl"), //$NON-NLS-1$

		/**
		 * IfStatement
		 * @generated
		 */
		IF_STATEMENT("IfStatementImpl"), //$NON-NLS-1$

		/**
		 * CriteriaSelector
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		CRITERIA_SELECTOR("CriteriaSelectorImpl"), //$NON-NLS-1$

		/**
		 * HasCriteria
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		HAS_CRITERIA("HasCriteriaImpl"), //$NON-NLS-1$

		/**
		 * DeclareStatement
		 * @generated
		 */
		DECLARE_STATEMENT("DeclareStatementImpl"), //$NON-NLS-1$

		/**
		 * CommandStatement
		 * @generated
		 */
		COMMAND_STATEMENT("CommandStatementImpl"), //$NON-NLS-1$

		/**
		 * TranslateCriteria
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		TRANSLATE_CRITERIA("TranslateCriteriaImpl"), //$NON-NLS-1$

		/**
		 * CreateUpdateProcedureCommand
		 * @generated
		 */
		@Removed(Version.TEIID_8_0)
		CREATE_UPDATE_PROCEDURE_COMMAND("CreateUpdateProcedureCommandImpl"), //$NON-NLS-1$

		/**
		 * DynamicCommand
		 * @generated
		 */
		DYNAMIC_COMMAND("DynamicCommandImpl"), //$NON-NLS-1$

		/**
		 * SetClauseList
		 * @generated
		 */
		SET_CLAUSE_LIST("SetClauseListImpl"), //$NON-NLS-1$

		/**
		 * SetClause
		 * @generated
		 */
		SET_CLAUSE("SetClauseImpl"), //$NON-NLS-1$

		/**
		 * ProjectedColumn
		 * @generated
		 */
		PROJECTED_COLUMN("ProjectedColumnImpl"), //$NON-NLS-1$

		/**
		 * StoredProcedure
		 * @generated
		 */
		STORED_PROCEDURE("StoredProcedureImpl"), //$NON-NLS-1$

		/**
		 * Insert
		 * @generated
		 */
		INSERT("InsertImpl"), //$NON-NLS-1$

		/**
		 * Update
		 * @generated
		 */
		UPDATE("UpdateImpl"), //$NON-NLS-1$

		/**
		 * Delete
		 * @generated
		 */
		DELETE("DeleteImpl"), //$NON-NLS-1$

		/**
		 * WithQueryCommand
		 * @generated
		 */
		WITH_QUERY_COMMAND("WithQueryCommandImpl"), //$NON-NLS-1$

		/**
		 * SetQuery
		 * @generated
		 */
		SET_QUERY("SetQueryImpl"), //$NON-NLS-1$

		/**
		 * Query
		 * @generated
		 */
		QUERY("QueryImpl"), //$NON-NLS-1$

		/**
		 * Into
		 * @generated
		 */
		INTO("IntoImpl"), //$NON-NLS-1$

		/**
		 * Select
		 * @generated
		 */
		SELECT("SelectImpl"), //$NON-NLS-1$

		/**
		 * DerivedColumn
		 * @generated
		 */
		DERIVED_COLUMN("DerivedColumnImpl"), //$NON-NLS-1$

		/**
		 * MultipleElementSymbol
		 * @generated
		 */
		MULTIPLE_ELEMENT_SYMBOL("MultipleElementSymbolImpl"), //$NON-NLS-1$

		/**
		 * From
		 * @generated
		 */
		FROM("FromImpl"), //$NON-NLS-1$

		/**
		 * JoinPredicate
		 * @generated
		 */
		JOIN_PREDICATE("JoinPredicateImpl"), //$NON-NLS-1$

		/**
		 * JoinType
		 * @generated
		 */
		JOIN_TYPE("JoinTypeImpl"), //$NON-NLS-1$

		/**
		 * XMLSerialize
		 * @generated
		 */
		XML_SERIALIZE("XMLSerializeImpl"), //$NON-NLS-1$

		/**
		 * ArrayTable
		 * @generated
		 */
		ARRAY_TABLE("ArrayTableImpl"), //$NON-NLS-1$

		/**
		 * TextTable
		 * @generated
		 */
		TEXT_TABLE("TextTableImpl"), //$NON-NLS-1$

		/**
		 * TextColumn
		 * @generated
		 */
		TEXT_COLUMN("TextColumnImpl"), //$NON-NLS-1$

		/**
		 * XMLQuery
		 * @generated
		 */
		XML_QUERY("XMLQueryImpl"), //$NON-NLS-1$

		/**
		 * XMLTable
		 * @generated
		 */
		XML_TABLE("XMLTableImpl"), //$NON-NLS-1$

		/**
		 * XMLColumn
		 * @generated
		 */
		XML_COLUMN("XMLColumnImpl"), //$NON-NLS-1$

		/**
		 * SubqueryFromClause
		 * @generated
		 */
		SUBQUERY_FROM_CLAUSE("SubqueryFromClauseImpl"), //$NON-NLS-1$

		/**
		 * UnaryFromClause
		 * @generated
		 */
		UNARY_FROM_CLAUSE("UnaryFromClauseImpl"), //$NON-NLS-1$

		/**
		 * Criteria
		 * @generated
		 */
		CRITERIA("CriteriaImpl"), //$NON-NLS-1$

		/**
		 * CompoundCriteria
		 * @generated
		 */
		COMPOUND_CRITERIA("CompoundCriteriaImpl"), //$NON-NLS-1$

		/**
		 * NotCriteria
		 * @generated
		 */
		NOT_CRITERIA("NotCriteriaImpl"), //$NON-NLS-1$

		/**
		 * CompareCriteria
		 * @generated
		 */
		COMPARE_CRITERIA("CompareCriteriaImpl"), //$NON-NLS-1$

		/**
		 * SubqueryCompareCriteria
		 * @generated
		 */
		SUBQUERY_COMPARE_CRITERIA("SubqueryCompareCriteriaImpl"), //$NON-NLS-1$

		/**
		 * MatchCriteria
		 * @generated
		 */
		MATCH_CRITERIA("MatchCriteriaImpl"), //$NON-NLS-1$

		/**
		 * BetweenCriteria
		 * @generated
		 */
		BETWEEN_CRITERIA("BetweenCriteriaImpl"), //$NON-NLS-1$

		/**
		 * IsNullCriteria
		 * @generated
		 */
		IS_NULL_CRITERIA("IsNullCriteriaImpl"), //$NON-NLS-1$

		/**
		 * SubquerySetCriteria
		 * @generated
		 */
		SUBQUERY_SET_CRITERIA("SubquerySetCriteriaImpl"), //$NON-NLS-1$

		/**
		 * SetCriteria
		 * @generated
		 */
		SET_CRITERIA("SetCriteriaImpl"), //$NON-NLS-1$

		/**
		 * ExistsCriteria
		 * @generated
		 */
		EXISTS_CRITERIA("ExistsCriteriaImpl"), //$NON-NLS-1$

		/**
		 * GroupBy
		 * @generated
		 */
		GROUP_BY("GroupByImpl"), //$NON-NLS-1$

		/**
		 * OrderBy
		 * @generated
		 */
		ORDER_BY("OrderByImpl"), //$NON-NLS-1$

		/**
		 * OrderByItem
		 * @generated
		 */
		ORDER_BY_ITEM("OrderByItemImpl"), //$NON-NLS-1$

		/**
		 * ExpressionSymbol
		 * @generated
		 */
		EXPRESSION_SYMBOL("ExpressionSymbolImpl"), //$NON-NLS-1$

		/**
		 * Limit
		 * @generated
		 */
		LIMIT("LimitImpl"), //$NON-NLS-1$

		/**
		 * Option
		 * @generated
		 */
		OPTION("OptionImpl"), //$NON-NLS-1$

		/**
		 * Reference
		 * @generated
		 */
		REFERENCE("ReferenceImpl"), //$NON-NLS-1$

		/**
		 * CaseExpression
		 * @generated
		 */
		CASE_EXPRESSION("CaseExpressionImpl"), //$NON-NLS-1$

		/**
		 * SearchedCaseExpression
		 * @generated
		 */
		SEARCHED_CASE_EXPRESSION("SearchedCaseExpressionImpl"), //$NON-NLS-1$

		/**
		 * Function
		 * @generated
		 */
		FUNCTION("FunctionImpl"), //$NON-NLS-1$

		/**
		 * XMLParse
		 * @generated
		 */
		XML_PARSE("XMLParseImpl"), //$NON-NLS-1$

		/**
		 * QueryString
		 * @generated
		 */
		QUERY_STRING("QueryStringImpl"), //$NON-NLS-1$

		/**
		 * XMLElement
		 * @generated
		 */
		XML_ELEMENT("XMLElementImpl"), //$NON-NLS-1$

		/**
		 * XMLAttributes
		 * @generated
		 */
		XML_ATTRIBUTES("XMLAttributesImpl"), //$NON-NLS-1$

		/**
		 * XMLForest
		 * @generated
		 */
		XML_FOREST("XMLForestImpl"), //$NON-NLS-1$

		/**
		 * XMLNamespaces
		 * @generated
		 */
		XML_NAMESPACES("XMLNamespacesImpl"), //$NON-NLS-1$

		/**
		 * AssignmentStatement
		 * @generated
		 */
		ASSIGNMENT_STATEMENT("AssignmentStatementImpl"), //$NON-NLS-1$

		/**
		 * ScalarSubquery
		 * @generated
		 */
		SCALAR_SUBQUERY("ScalarSubqueryImpl"), //$NON-NLS-1$

		/**
		 * GroupSymbol
		 * @generated
		 */
		GROUP_SYMBOL("GroupSymbolImpl"), //$NON-NLS-1$

		/**
		 * Constant
		 * @generated
		 */
		CONSTANT("ConstantImpl"), //$NON-NLS-1$

		/**
		 * ElementSymbol
		 * @generated
		 */
		ELEMENT_SYMBOL("ElementSymbolImpl"), //$NON-NLS-1$

		/**
		 * Block
		 * @generated
		 */
		BLOCK("BlockImpl"), //$NON-NLS-1$

		/**
		 * ExpressionCriteria
		 * @generated
		 */
		EXPRESSION_CRITERIA("ExpressionCriteriaImpl"), //$NON-NLS-1$

		/**
		 * AliasSymbol
		 * @generated
		 */
		ALIAS_SYMBOL("AliasSymbolImpl"), //$NON-NLS-1$

		/**
		 * AggregateSymbol
		 * @generated
		 */
		AGGREGATE_SYMBOL("BaseAggregateSymbol"), //$NON-NLS-1$

		/**
		 * WindowFunction
		 * @generated
		 */
		WINDOW_FUNCTION("BaseWindowFunction"), //$NON-NLS-1$

		/**
		 * WindowSpecification
		 * @generated
		 */
		WINDOW_SPECIFICATION("WindowSpecificationImpl"), //$NON-NLS-1$

		/**
		 * TextLine
		 * @generated
		 */
		TEXT_LINE("TextLineImpl"), //$NON-NLS-1$

		/**
		 * AlterTrigger
		 * @generated
		 */
		ALTER_TRIGGER("AlterTriggerImpl"), //$NON-NLS-1$

		/**
		 * AlterProcedure
		 * @generated
		 */
		ALTER_PROCEDURE("AlterProcedureImpl"), //$NON-NLS-1$

		/**
		 * AlterView
		 * @generated
		 */
		ALTER_VIEW("AlterViewImpl"), //$NON-NLS-1$

		/**
		 * RaiseStatement
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		RAISE_STATEMENT("RaiseStatementImpl"), //$NON-NLS-1$

		/**
		 * ExceptionExpression
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		EXCEPTION_EXPRESSION("ExceptionExpressionImpl"), //$NON-NLS-1$

		/**
		 * ReturnStatement
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		RETURN_STATEMENT("ReturnStatementImpl"), //$NON-NLS-1$

		/**
		 * CreateProcedureCommand
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		CREATE_PROCEDURE_COMMAND("CreateProcedureCommandImpl"), //$NON-NLS-1$

		/**
		 * ObjectTable
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		OBJECT_TABLE("ObjectTableImpl"), //$NON-NLS-1$

		/**
		 * ObjectColumn
		 * @generated
		 */
		OBJECT_COLUMN("ObjectColumnImpl"), //$NON-NLS-1$

		/**
		 * JSONObject
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		JSON_OBJECT("JSONObjectImpl"), //$NON-NLS-1$

		/**
		 * ArraySymbol
		 * @generated
		 */
		@Since(Version.TEIID_8_0)
		ARRAY_SYMBOL("ArraySymbolImpl"); //$NON-NLS-1$

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
	private TriggerActionImpl createTriggerActionImpl(TeiidClientParser teiidParser, int nodeType) {
			return new TriggerActionImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DropImpl createDropImpl(TeiidClientParser teiidParser, int nodeType) {
			return new DropImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CreateImpl createCreateImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CreateImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private RaiseErrorStatementImpl createRaiseErrorStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new RaiseErrorStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private BranchingStatementImpl createBranchingStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new BranchingStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WhileStatementImpl createWhileStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new WhileStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private LoopStatementImpl createLoopStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new LoopStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private IfStatementImpl createIfStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new IfStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CriteriaSelectorImpl createCriteriaSelectorImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CriteriaSelectorImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private HasCriteriaImpl createHasCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new HasCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DeclareStatementImpl createDeclareStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new DeclareStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CommandStatementImpl createCommandStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CommandStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TranslateCriteriaImpl createTranslateCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new TranslateCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CreateUpdateProcedureCommandImpl createCreateUpdateProcedureCommandImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CreateUpdateProcedureCommandImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DynamicCommandImpl createDynamicCommandImpl(TeiidClientParser teiidParser, int nodeType) {
			return new DynamicCommandImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetClauseListImpl createSetClauseListImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SetClauseListImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetClauseImpl createSetClauseImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SetClauseImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ProjectedColumnImpl createProjectedColumnImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ProjectedColumnImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private StoredProcedureImpl createStoredProcedureImpl(TeiidClientParser teiidParser, int nodeType) {
			return new StoredProcedureImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private InsertImpl createInsertImpl(TeiidClientParser teiidParser, int nodeType) {
			return new InsertImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private UpdateImpl createUpdateImpl(TeiidClientParser teiidParser, int nodeType) {
			return new UpdateImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DeleteImpl createDeleteImpl(TeiidClientParser teiidParser, int nodeType) {
			return new DeleteImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WithQueryCommandImpl createWithQueryCommandImpl(TeiidClientParser teiidParser, int nodeType) {
			return new WithQueryCommandImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetQueryImpl createSetQueryImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SetQueryImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private QueryImpl createQueryImpl(TeiidClientParser teiidParser, int nodeType) {
			return new QueryImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private IntoImpl createIntoImpl(TeiidClientParser teiidParser, int nodeType) {
			return new IntoImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SelectImpl createSelectImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SelectImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private DerivedColumnImpl createDerivedColumnImpl(TeiidClientParser teiidParser, int nodeType) {
			return new DerivedColumnImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private MultipleElementSymbolImpl createMultipleElementSymbolImpl(TeiidClientParser teiidParser, int nodeType) {
			return new MultipleElementSymbolImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private FromImpl createFromImpl(TeiidClientParser teiidParser, int nodeType) {
			return new FromImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private JoinPredicateImpl createJoinPredicateImpl(TeiidClientParser teiidParser, int nodeType) {
			return new JoinPredicateImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private JoinTypeImpl createJoinTypeImpl(TeiidClientParser teiidParser, int nodeType) {
			return new JoinTypeImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLSerializeImpl createXMLSerializeImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLSerializeImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ArrayTableImpl createArrayTableImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ArrayTableImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TextTableImpl createTextTableImpl(TeiidClientParser teiidParser, int nodeType) {
			return new TextTableImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TextColumnImpl createTextColumnImpl(TeiidClientParser teiidParser, int nodeType) {
			return new TextColumnImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLQueryImpl createXMLQueryImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLQueryImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLTableImpl createXMLTableImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLTableImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLColumnImpl createXMLColumnImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLColumnImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SubqueryFromClauseImpl createSubqueryFromClauseImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SubqueryFromClauseImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private UnaryFromClauseImpl createUnaryFromClauseImpl(TeiidClientParser teiidParser, int nodeType) {
			return new UnaryFromClauseImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CriteriaImpl createCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CompoundCriteriaImpl createCompoundCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CompoundCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private NotCriteriaImpl createNotCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new NotCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CompareCriteriaImpl createCompareCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CompareCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SubqueryCompareCriteriaImpl createSubqueryCompareCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SubqueryCompareCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private MatchCriteriaImpl createMatchCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new MatchCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private BetweenCriteriaImpl createBetweenCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new BetweenCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private IsNullCriteriaImpl createIsNullCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new IsNullCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SubquerySetCriteriaImpl createSubquerySetCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SubquerySetCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SetCriteriaImpl createSetCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SetCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExistsCriteriaImpl createExistsCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ExistsCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private GroupByImpl createGroupByImpl(TeiidClientParser teiidParser, int nodeType) {
			return new GroupByImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private OrderByImpl createOrderByImpl(TeiidClientParser teiidParser, int nodeType) {
			return new OrderByImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private OrderByItemImpl createOrderByItemImpl(TeiidClientParser teiidParser, int nodeType) {
			return new OrderByItemImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExpressionSymbolImpl createExpressionSymbolImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ExpressionSymbolImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private LimitImpl createLimitImpl(TeiidClientParser teiidParser, int nodeType) {
			return new LimitImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private OptionImpl createOptionImpl(TeiidClientParser teiidParser, int nodeType) {
			return new OptionImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ReferenceImpl createReferenceImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ReferenceImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CaseExpressionImpl createCaseExpressionImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CaseExpressionImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SearchedCaseExpressionImpl createSearchedCaseExpressionImpl(TeiidClientParser teiidParser, int nodeType) {
			return new SearchedCaseExpressionImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private FunctionImpl createFunctionImpl(TeiidClientParser teiidParser, int nodeType) {
			return new FunctionImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLParseImpl createXMLParseImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLParseImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private QueryStringImpl createQueryStringImpl(TeiidClientParser teiidParser, int nodeType) {
			return new QueryStringImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLElementImpl createXMLElementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLElementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLAttributesImpl createXMLAttributesImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLAttributesImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLForestImpl createXMLForestImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLForestImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private XMLNamespacesImpl createXMLNamespacesImpl(TeiidClientParser teiidParser, int nodeType) {
			return new XMLNamespacesImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AssignmentStatementImpl createAssignmentStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new AssignmentStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ScalarSubqueryImpl createScalarSubqueryImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ScalarSubqueryImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private GroupSymbolImpl createGroupSymbolImpl(TeiidClientParser teiidParser, int nodeType) {
			return new GroupSymbolImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ConstantImpl createConstantImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ConstantImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ElementSymbolImpl createElementSymbolImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ElementSymbolImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private BlockImpl createBlockImpl(TeiidClientParser teiidParser, int nodeType) {
			return new BlockImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExpressionCriteriaImpl createExpressionCriteriaImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ExpressionCriteriaImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AliasSymbolImpl createAliasSymbolImpl(TeiidClientParser teiidParser, int nodeType) {
			return new AliasSymbolImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private BaseAggregateSymbol createBaseAggregateSymbol(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return new Aggregate7SymbolImpl((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return new Aggregate8SymbolImpl((Teiid8ClientParser) teiidParser, nodeType);
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
	private BaseWindowFunction createBaseWindowFunction(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return new Window7FunctionImpl((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return new Window8FunctionImpl((Teiid8ClientParser) teiidParser, nodeType);
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
	private WindowSpecificationImpl createWindowSpecificationImpl(TeiidClientParser teiidParser, int nodeType) {
			return new WindowSpecificationImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private TextLineImpl createTextLineImpl(TeiidClientParser teiidParser, int nodeType) {
			return new TextLineImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AlterTriggerImpl createAlterTriggerImpl(TeiidClientParser teiidParser, int nodeType) {
			return new AlterTriggerImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AlterProcedureImpl createAlterProcedureImpl(TeiidClientParser teiidParser, int nodeType) {
		if (isTeiid7ClientParser(teiidParser))
			return new Alter7ProcedureImpl((Teiid7ClientParser) teiidParser, nodeType);
		else if (isTeiid8ClientParser(teiidParser))
			return new Alter8ProcedureImpl((Teiid8ClientParser) teiidParser, nodeType);
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
	private AlterViewImpl createAlterViewImpl(TeiidClientParser teiidParser, int nodeType) {
			return new AlterViewImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private RaiseStatementImpl createRaiseStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new RaiseStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ExceptionExpressionImpl createExceptionExpressionImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ExceptionExpressionImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ReturnStatementImpl createReturnStatementImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ReturnStatementImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private CreateProcedureCommandImpl createCreateProcedureCommandImpl(TeiidClientParser teiidParser, int nodeType) {
			return new CreateProcedureCommandImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ObjectTableImpl createObjectTableImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ObjectTableImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ObjectColumnImpl createObjectColumnImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ObjectColumnImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private JSONObjectImpl createJSONObjectImpl(TeiidClientParser teiidParser, int nodeType) {
			return new JSONObjectImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private ArraySymbolImpl createArraySymbolImpl(TeiidClientParser teiidParser, int nodeType) {
			return new ArraySymbolImpl(teiidParser, nodeType);
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
private <T extends BaseLanguageObject> T create(Teiid7ClientParser teiidParser, int nodeType) {
		switch (nodeType) {
			case Teiid7ClientParserTreeConstants.JJTTRIGGERACTIONIMPL:
				return (T) createTriggerActionImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDROPIMPL:
				return (T) createDropImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCREATEIMPL:
				return (T) createCreateImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTRAISEERRORSTATEMENTIMPL:
				return (T) createRaiseErrorStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBRANCHINGSTATEMENTIMPL:
				return (T) createBranchingStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTWHILESTATEMENTIMPL:
				return (T) createWhileStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTLOOPSTATEMENTIMPL:
				return (T) createLoopStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTIFSTATEMENTIMPL:
				return (T) createIfStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCRITERIASELECTORIMPL:
				return (T) createCriteriaSelectorImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTHASCRITERIAIMPL:
				return (T) createHasCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDECLARESTATEMENTIMPL:
				return (T) createDeclareStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCOMMANDSTATEMENTIMPL:
				return (T) createCommandStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTRANSLATECRITERIAIMPL:
				return (T) createTranslateCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCREATEUPDATEPROCEDURECOMMANDIMPL:
				return (T) createCreateUpdateProcedureCommandImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDYNAMICCOMMANDIMPL:
				return (T) createDynamicCommandImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETCLAUSELISTIMPL:
				return (T) createSetClauseListImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETCLAUSEIMPL:
				return (T) createSetClauseImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTPROJECTEDCOLUMNIMPL:
				return (T) createProjectedColumnImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSTOREDPROCEDUREIMPL:
				return (T) createStoredProcedureImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTINSERTIMPL:
				return (T) createInsertImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTUPDATEIMPL:
				return (T) createUpdateImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDELETEIMPL:
				return (T) createDeleteImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTWITHQUERYCOMMANDIMPL:
				return (T) createWithQueryCommandImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETQUERYIMPL:
				return (T) createSetQueryImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTQUERYIMPL:
				return (T) createQueryImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTINTOIMPL:
				return (T) createIntoImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSELECTIMPL:
				return (T) createSelectImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTDERIVEDCOLUMNIMPL:
				return (T) createDerivedColumnImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTMULTIPLEELEMENTSYMBOLIMPL:
				return (T) createMultipleElementSymbolImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTFROMIMPL:
				return (T) createFromImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTJOINPREDICATEIMPL:
				return (T) createJoinPredicateImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTJOINTYPEIMPL:
				return (T) createJoinTypeImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLSERIALIZEIMPL:
				return (T) createXMLSerializeImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTARRAYTABLEIMPL:
				return (T) createArrayTableImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTEXTTABLEIMPL:
				return (T) createTextTableImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTEXTCOLUMNIMPL:
				return (T) createTextColumnImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLQUERYIMPL:
				return (T) createXMLQueryImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLTABLEIMPL:
				return (T) createXMLTableImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLCOLUMNIMPL:
				return (T) createXMLColumnImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSUBQUERYFROMCLAUSEIMPL:
				return (T) createSubqueryFromClauseImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTUNARYFROMCLAUSEIMPL:
				return (T) createUnaryFromClauseImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCRITERIAIMPL:
				return (T) createCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCOMPOUNDCRITERIAIMPL:
				return (T) createCompoundCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTNOTCRITERIAIMPL:
				return (T) createNotCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCOMPARECRITERIAIMPL:
				return (T) createCompareCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSUBQUERYCOMPARECRITERIAIMPL:
				return (T) createSubqueryCompareCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTMATCHCRITERIAIMPL:
				return (T) createMatchCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBETWEENCRITERIAIMPL:
				return (T) createBetweenCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTISNULLCRITERIAIMPL:
				return (T) createIsNullCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSUBQUERYSETCRITERIAIMPL:
				return (T) createSubquerySetCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSETCRITERIAIMPL:
				return (T) createSetCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTEXISTSCRITERIAIMPL:
				return (T) createExistsCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTGROUPBYIMPL:
				return (T) createGroupByImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTORDERBYIMPL:
				return (T) createOrderByImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTORDERBYITEMIMPL:
				return (T) createOrderByItemImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTEXPRESSIONSYMBOLIMPL:
				return (T) createExpressionSymbolImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTLIMITIMPL:
				return (T) createLimitImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTOPTIONIMPL:
				return (T) createOptionImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTREFERENCEIMPL:
				return (T) createReferenceImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCASEEXPRESSIONIMPL:
				return (T) createCaseExpressionImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSEARCHEDCASEEXPRESSIONIMPL:
				return (T) createSearchedCaseExpressionImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTFUNCTIONIMPL:
				return (T) createFunctionImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLPARSEIMPL:
				return (T) createXMLParseImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTQUERYSTRINGIMPL:
				return (T) createQueryStringImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLELEMENTIMPL:
				return (T) createXMLElementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLATTRIBUTESIMPL:
				return (T) createXMLAttributesImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLFORESTIMPL:
				return (T) createXMLForestImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTXMLNAMESPACESIMPL:
				return (T) createXMLNamespacesImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTASSIGNMENTSTATEMENTIMPL:
				return (T) createAssignmentStatementImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTSCALARSUBQUERYIMPL:
				return (T) createScalarSubqueryImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTGROUPSYMBOLIMPL:
				return (T) createGroupSymbolImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTCONSTANTIMPL:
				return (T) createConstantImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTELEMENTSYMBOLIMPL:
				return (T) createElementSymbolImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBLOCKIMPL:
				return (T) createBlockImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTEXPRESSIONCRITERIAIMPL:
				return (T) createExpressionCriteriaImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALIASSYMBOLIMPL:
				return (T) createAliasSymbolImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBASEAGGREGATESYMBOL:
				return (T) createBaseAggregateSymbol(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTBASEWINDOWFUNCTION:
				return (T) createBaseWindowFunction(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTWINDOWSPECIFICATIONIMPL:
				return (T) createWindowSpecificationImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTTEXTLINEIMPL:
				return (T) createTextLineImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALTERTRIGGERIMPL:
				return (T) createAlterTriggerImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALTERPROCEDUREIMPL:
				return (T) createAlterProcedureImpl(teiidParser, nodeType);
			case Teiid7ClientParserTreeConstants.JJTALTERVIEWIMPL:
				return (T) createAlterViewImpl(teiidParser, nodeType);
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
private <T extends BaseLanguageObject> T create(Teiid8ClientParser teiidParser, int nodeType) {
		switch (nodeType) {
			case Teiid8ClientParserTreeConstants.JJTTRIGGERACTIONIMPL:
				return (T) createTriggerActionImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDROPIMPL:
				return (T) createDropImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCREATEIMPL:
				return (T) createCreateImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTRAISESTATEMENTIMPL:
				return (T) createRaiseStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXCEPTIONEXPRESSIONIMPL:
				return (T) createExceptionExpressionImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBRANCHINGSTATEMENTIMPL:
				return (T) createBranchingStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTRETURNSTATEMENTIMPL:
				return (T) createReturnStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTWHILESTATEMENTIMPL:
				return (T) createWhileStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTLOOPSTATEMENTIMPL:
				return (T) createLoopStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTIFSTATEMENTIMPL:
				return (T) createIfStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDECLARESTATEMENTIMPL:
				return (T) createDeclareStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCOMMANDSTATEMENTIMPL:
				return (T) createCommandStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCREATEPROCEDURECOMMANDIMPL:
				return (T) createCreateProcedureCommandImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDYNAMICCOMMANDIMPL:
				return (T) createDynamicCommandImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETCLAUSELISTIMPL:
				return (T) createSetClauseListImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETCLAUSEIMPL:
				return (T) createSetClauseImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTPROJECTEDCOLUMNIMPL:
				return (T) createProjectedColumnImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSTOREDPROCEDUREIMPL:
				return (T) createStoredProcedureImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTINSERTIMPL:
				return (T) createInsertImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTUPDATEIMPL:
				return (T) createUpdateImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDELETEIMPL:
				return (T) createDeleteImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTWITHQUERYCOMMANDIMPL:
				return (T) createWithQueryCommandImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETQUERYIMPL:
				return (T) createSetQueryImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTQUERYIMPL:
				return (T) createQueryImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTINTOIMPL:
				return (T) createIntoImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSELECTIMPL:
				return (T) createSelectImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXPRESSIONSYMBOLIMPL:
				return (T) createExpressionSymbolImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTDERIVEDCOLUMNIMPL:
				return (T) createDerivedColumnImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTMULTIPLEELEMENTSYMBOLIMPL:
				return (T) createMultipleElementSymbolImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTFROMIMPL:
				return (T) createFromImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTJOINPREDICATEIMPL:
				return (T) createJoinPredicateImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTJOINTYPEIMPL:
				return (T) createJoinTypeImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLSERIALIZEIMPL:
				return (T) createXMLSerializeImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTARRAYTABLEIMPL:
				return (T) createArrayTableImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTTEXTTABLEIMPL:
				return (T) createTextTableImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTTEXTCOLUMNIMPL:
				return (T) createTextColumnImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLQUERYIMPL:
				return (T) createXMLQueryImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTOBJECTTABLEIMPL:
				return (T) createObjectTableImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTOBJECTCOLUMNIMPL:
				return (T) createObjectColumnImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLTABLEIMPL:
				return (T) createXMLTableImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLCOLUMNIMPL:
				return (T) createXMLColumnImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSUBQUERYFROMCLAUSEIMPL:
				return (T) createSubqueryFromClauseImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTUNARYFROMCLAUSEIMPL:
				return (T) createUnaryFromClauseImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCRITERIAIMPL:
				return (T) createCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCOMPOUNDCRITERIAIMPL:
				return (T) createCompoundCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTNOTCRITERIAIMPL:
				return (T) createNotCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCOMPARECRITERIAIMPL:
				return (T) createCompareCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSUBQUERYCOMPARECRITERIAIMPL:
				return (T) createSubqueryCompareCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTMATCHCRITERIAIMPL:
				return (T) createMatchCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBETWEENCRITERIAIMPL:
				return (T) createBetweenCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTISNULLCRITERIAIMPL:
				return (T) createIsNullCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSUBQUERYSETCRITERIAIMPL:
				return (T) createSubquerySetCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSETCRITERIAIMPL:
				return (T) createSetCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXISTSCRITERIAIMPL:
				return (T) createExistsCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTGROUPBYIMPL:
				return (T) createGroupByImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTORDERBYIMPL:
				return (T) createOrderByImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTORDERBYITEMIMPL:
				return (T) createOrderByItemImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTLIMITIMPL:
				return (T) createLimitImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTOPTIONIMPL:
				return (T) createOptionImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTREFERENCEIMPL:
				return (T) createReferenceImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCASEEXPRESSIONIMPL:
				return (T) createCaseExpressionImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSEARCHEDCASEEXPRESSIONIMPL:
				return (T) createSearchedCaseExpressionImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTFUNCTIONIMPL:
				return (T) createFunctionImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLPARSEIMPL:
				return (T) createXMLParseImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTQUERYSTRINGIMPL:
				return (T) createQueryStringImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLELEMENTIMPL:
				return (T) createXMLElementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLATTRIBUTESIMPL:
				return (T) createXMLAttributesImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTJSONOBJECTIMPL:
				return (T) createJSONObjectImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLFORESTIMPL:
				return (T) createXMLForestImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTXMLNAMESPACESIMPL:
				return (T) createXMLNamespacesImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTASSIGNMENTSTATEMENTIMPL:
				return (T) createAssignmentStatementImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTSCALARSUBQUERYIMPL:
				return (T) createScalarSubqueryImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTGROUPSYMBOLIMPL:
				return (T) createGroupSymbolImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTCONSTANTIMPL:
				return (T) createConstantImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTELEMENTSYMBOLIMPL:
				return (T) createElementSymbolImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBLOCKIMPL:
				return (T) createBlockImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTEXPRESSIONCRITERIAIMPL:
				return (T) createExpressionCriteriaImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALIASSYMBOLIMPL:
				return (T) createAliasSymbolImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBASEAGGREGATESYMBOL:
				return (T) createBaseAggregateSymbol(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTBASEWINDOWFUNCTION:
				return (T) createBaseWindowFunction(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTWINDOWSPECIFICATIONIMPL:
				return (T) createWindowSpecificationImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTTEXTLINEIMPL:
				return (T) createTextLineImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALTERTRIGGERIMPL:
				return (T) createAlterTriggerImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALTERPROCEDUREIMPL:
				return (T) createAlterProcedureImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTALTERVIEWIMPL:
				return (T) createAlterViewImpl(teiidParser, nodeType);
			case Teiid8ClientParserTreeConstants.JJTARRAYSYMBOLIMPL:
				return (T) createArraySymbolImpl(teiidParser, nodeType);
		default:
			throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
		}
	}


}