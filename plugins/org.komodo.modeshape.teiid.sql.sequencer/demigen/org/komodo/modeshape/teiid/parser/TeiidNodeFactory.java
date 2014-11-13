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
package org.komodo.modeshape.teiid.parser;

import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.Messages;
import org.komodo.modeshape.teiid.sql.lang.*;
import org.komodo.modeshape.teiid.sql.proc.*;
import org.komodo.modeshape.teiid.sql.symbol.*;

/**
 * Factory used to generate parser nodes
 */
public class TeiidNodeFactory {

	private static TeiidNodeFactory instance;

	/**
	 * @return Singleton instance of this factory
	 *
	 * @generated
	 */
	public static TeiidNodeFactory getInstance() {
		if (instance == null) instance = new TeiidNodeFactory();
		return instance;
	}

	/**
	 * Method used by the generated parsers for constructing nodes
	 *
	 * @param teiidParser parent parser
	 * @param nodeType type of node
	 *
	 * @return created language object
	 *
	 * @generated
	 */
	public static BaseLanguageObject jjtCreate(TeiidSeqParser teiidParser, int nodeType) {
		return getInstance().create(teiidParser, nodeType);
	}

	/**
	 * Create a parser node for the node with the given common node name
	 *
	 * @param teiidParser parent parser
	 * @param nodeType type of node
	 *
	 * @return node applicable to the given parser
	 *
	 * @generated
	 */
	public <T extends BaseLanguageObject> T create(TeiidSeqParser teiidParser, ASTNodes nodeType) {

		for (int i = 0; i < TeiidSequencingParserTreeConstants.jjtNodeName.length; ++i) {
			String constantName = TeiidSequencingParserTreeConstants.jjtNodeName[i];
				if (! constantName.equalsIgnoreCase(nodeType.getName()))
					continue;

			return create(teiidParser, i);
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
		 * RaiseStatement
		 * @generated
		 */
		RAISE_STATEMENT("RaiseStatementImpl"), //$NON-NLS-1$

		/**
		 * ExceptionExpression
		 * @generated
		 */
		EXCEPTION_EXPRESSION("ExceptionExpressionImpl"), //$NON-NLS-1$

		/**
		 * BranchingStatement
		 * @generated
		 */
		BRANCHING_STATEMENT("BranchingStatementImpl"), //$NON-NLS-1$

		/**
		 * ReturnStatement
		 * @generated
		 */
		RETURN_STATEMENT("ReturnStatementImpl"), //$NON-NLS-1$

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
		 * CreateProcedureCommand
		 * @generated
		 */
		CREATE_PROCEDURE_COMMAND("CreateProcedureCommandImpl"), //$NON-NLS-1$

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
		 * ExpressionSymbol
		 * @generated
		 */
		EXPRESSION_SYMBOL("ExpressionSymbolImpl"), //$NON-NLS-1$

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
		 * ObjectTable
		 * @generated
		 */
		OBJECT_TABLE("ObjectTableImpl"), //$NON-NLS-1$

		/**
		 * ObjectColumn
		 * @generated
		 */
		OBJECT_COLUMN("ObjectColumnImpl"), //$NON-NLS-1$

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
		 * JSONObject
		 * @generated
		 */
		JSON_OBJECT("JSONObjectImpl"), //$NON-NLS-1$

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
		 * NamespaceItem
		 * @generated
		 */
		NAMESPACE_ITEM("NamespaceItem"), //$NON-NLS-1$

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
		AGGREGATE_SYMBOL("AggregateSymbolImpl"), //$NON-NLS-1$

		/**
		 * WindowFunction
		 * @generated
		 */
		WINDOW_FUNCTION("WindowFunctionImpl"), //$NON-NLS-1$

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
		 * ArraySymbol
		 * @generated
		 */
		ARRAY_SYMBOL("ArraySymbolImpl"), //$NON-NLS-1$

		/**
		 * SPParameter
		 * @generated
		 */
		SP_PARAMETER("SPParameterImpl"), //$NON-NLS-1$

		/**
		 * SourceHint
		 * @generated
		 */
		SOURCE_HINT("SourceHintImpl"), //$NON-NLS-1$

		/**
		 * SpecificHint
		 * @generated
		 */
		SPECIFIC_HINT("SpecificHint"), //$NON-NLS-1$

		/**
		 * SubqueryHint
		 * @generated
		 */
		SUBQUERY_HINT("SubqueryHint"), //$NON-NLS-1$

		/**
		 * MakeDep
		 * @generated
		 */
		MAKE_DEP("MakeDep"); //$NON-NLS-1$

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
	private TriggerActionImpl createTriggerActionImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private RaiseStatementImpl createRaiseStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExceptionExpressionImpl createExceptionExpressionImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private BranchingStatementImpl createBranchingStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ReturnStatementImpl createReturnStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private WhileStatementImpl createWhileStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private LoopStatementImpl createLoopStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private IfStatementImpl createIfStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private DeclareStatementImpl createDeclareStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private CommandStatementImpl createCommandStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private CreateProcedureCommandImpl createCreateProcedureCommandImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private DynamicCommandImpl createDynamicCommandImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetClauseListImpl createSetClauseListImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetClauseImpl createSetClauseImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ProjectedColumnImpl createProjectedColumnImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private StoredProcedureImpl createStoredProcedureImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private InsertImpl createInsertImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private UpdateImpl createUpdateImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private DeleteImpl createDeleteImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private WithQueryCommandImpl createWithQueryCommandImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetQueryImpl createSetQueryImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private QueryImpl createQueryImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private IntoImpl createIntoImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SelectImpl createSelectImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExpressionSymbolImpl createExpressionSymbolImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private DerivedColumnImpl createDerivedColumnImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private MultipleElementSymbolImpl createMultipleElementSymbolImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private FromImpl createFromImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private JoinPredicateImpl createJoinPredicateImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private JoinTypeImpl createJoinTypeImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLSerializeImpl createXMLSerializeImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ArrayTableImpl createArrayTableImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private TextTableImpl createTextTableImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private TextColumnImpl createTextColumnImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLQueryImpl createXMLQueryImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ObjectTableImpl createObjectTableImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ObjectColumnImpl createObjectColumnImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLTableImpl createXMLTableImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLColumnImpl createXMLColumnImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SubqueryFromClauseImpl createSubqueryFromClauseImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private UnaryFromClauseImpl createUnaryFromClauseImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private CriteriaImpl createCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private CompoundCriteriaImpl createCompoundCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private NotCriteriaImpl createNotCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private CompareCriteriaImpl createCompareCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SubqueryCompareCriteriaImpl createSubqueryCompareCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private MatchCriteriaImpl createMatchCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private BetweenCriteriaImpl createBetweenCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private IsNullCriteriaImpl createIsNullCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SubquerySetCriteriaImpl createSubquerySetCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetCriteriaImpl createSetCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExistsCriteriaImpl createExistsCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private GroupByImpl createGroupByImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private OrderByImpl createOrderByImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private OrderByItemImpl createOrderByItemImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private LimitImpl createLimitImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private OptionImpl createOptionImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ReferenceImpl createReferenceImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private CaseExpressionImpl createCaseExpressionImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private SearchedCaseExpressionImpl createSearchedCaseExpressionImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private FunctionImpl createFunctionImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLParseImpl createXMLParseImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private QueryStringImpl createQueryStringImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLElementImpl createXMLElementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLAttributesImpl createXMLAttributesImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private JSONObjectImpl createJSONObjectImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLForestImpl createXMLForestImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLNamespacesImpl createXMLNamespacesImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private NamespaceItem createNamespaceItem(TeiidSeqParser teiidParser, int nodeType) {
		return new NamespaceItem(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AssignmentStatementImpl createAssignmentStatementImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ScalarSubqueryImpl createScalarSubqueryImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private GroupSymbolImpl createGroupSymbolImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ConstantImpl createConstantImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ElementSymbolImpl createElementSymbolImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private BlockImpl createBlockImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExpressionCriteriaImpl createExpressionCriteriaImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private AliasSymbolImpl createAliasSymbolImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private AggregateSymbolImpl createAggregateSymbolImpl(TeiidSeqParser teiidParser, int nodeType) {
		return new AggregateSymbolImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WindowFunctionImpl createWindowFunctionImpl(TeiidSeqParser teiidParser, int nodeType) {
		return new WindowFunctionImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WindowSpecificationImpl createWindowSpecificationImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private TextLineImpl createTextLineImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private AlterTriggerImpl createAlterTriggerImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private AlterProcedureImpl createAlterProcedureImpl(TeiidSeqParser teiidParser, int nodeType) {
		return new AlterProcedureImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AlterViewImpl createAlterViewImpl(TeiidSeqParser teiidParser, int nodeType) {
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
	private ArraySymbolImpl createArraySymbolImpl(TeiidSeqParser teiidParser, int nodeType) {
		return new ArraySymbolImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SPParameterImpl createSPParameterImpl(TeiidSeqParser teiidParser, int nodeType) {
		return new SPParameterImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SourceHintImpl createSourceHintImpl(TeiidSeqParser teiidParser, int nodeType) {
		return new SourceHintImpl(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SpecificHint createSpecificHint(TeiidSeqParser teiidParser, int nodeType) {
		return new SpecificHint(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SubqueryHint createSubqueryHint(TeiidSeqParser teiidParser, int nodeType) {
		return new SubqueryHint(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private MakeDep createMakeDep(TeiidSeqParser teiidParser, int nodeType) {
		return new MakeDep(teiidParser, nodeType);
	}

	/**
	 * Create a teiid parser node for the given node type.
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return teiid parser node
	 */
	protected <T extends BaseLanguageObject> T create(TeiidSeqParser teiidParser, int nodeType) {
		switch (nodeType) {
			case TeiidSequencingParserTreeConstants.JJTTRIGGERACTIONIMPL:
				return (T) createTriggerActionImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTRAISESTATEMENTIMPL:
				return (T) createRaiseStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXCEPTIONEXPRESSIONIMPL:
				return (T) createExceptionExpressionImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTBRANCHINGSTATEMENTIMPL:
				return (T) createBranchingStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTRETURNSTATEMENTIMPL:
				return (T) createReturnStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWHILESTATEMENTIMPL:
				return (T) createWhileStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTLOOPSTATEMENTIMPL:
				return (T) createLoopStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTIFSTATEMENTIMPL:
				return (T) createIfStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDECLARESTATEMENTIMPL:
				return (T) createDeclareStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCOMMANDSTATEMENTIMPL:
				return (T) createCommandStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCREATEPROCEDURECOMMANDIMPL:
				return (T) createCreateProcedureCommandImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDYNAMICCOMMANDIMPL:
				return (T) createDynamicCommandImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETCLAUSELISTIMPL:
				return (T) createSetClauseListImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETCLAUSEIMPL:
				return (T) createSetClauseImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTPROJECTEDCOLUMNIMPL:
				return (T) createProjectedColumnImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSTOREDPROCEDUREIMPL:
				return (T) createStoredProcedureImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTINSERTIMPL:
				return (T) createInsertImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTUPDATEIMPL:
				return (T) createUpdateImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDELETEIMPL:
				return (T) createDeleteImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWITHQUERYCOMMANDIMPL:
				return (T) createWithQueryCommandImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETQUERYIMPL:
				return (T) createSetQueryImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTQUERYIMPL:
				return (T) createQueryImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTINTOIMPL:
				return (T) createIntoImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSELECTIMPL:
				return (T) createSelectImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXPRESSIONSYMBOLIMPL:
				return (T) createExpressionSymbolImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDERIVEDCOLUMNIMPL:
				return (T) createDerivedColumnImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTMULTIPLEELEMENTSYMBOLIMPL:
				return (T) createMultipleElementSymbolImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTFROMIMPL:
				return (T) createFromImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTJOINPREDICATEIMPL:
				return (T) createJoinPredicateImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTJOINTYPEIMPL:
				return (T) createJoinTypeImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLSERIALIZEIMPL:
				return (T) createXMLSerializeImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTARRAYTABLEIMPL:
				return (T) createArrayTableImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTTEXTTABLEIMPL:
				return (T) createTextTableImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTTEXTCOLUMNIMPL:
				return (T) createTextColumnImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLQUERYIMPL:
				return (T) createXMLQueryImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTOBJECTTABLEIMPL:
				return (T) createObjectTableImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTOBJECTCOLUMNIMPL:
				return (T) createObjectColumnImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLTABLEIMPL:
				return (T) createXMLTableImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLCOLUMNIMPL:
				return (T) createXMLColumnImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSUBQUERYFROMCLAUSEIMPL:
				return (T) createSubqueryFromClauseImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTUNARYFROMCLAUSEIMPL:
				return (T) createUnaryFromClauseImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCRITERIAIMPL:
				return (T) createCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCOMPOUNDCRITERIAIMPL:
				return (T) createCompoundCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTNOTCRITERIAIMPL:
				return (T) createNotCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCOMPARECRITERIAIMPL:
				return (T) createCompareCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSUBQUERYCOMPARECRITERIAIMPL:
				return (T) createSubqueryCompareCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTMATCHCRITERIAIMPL:
				return (T) createMatchCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTBETWEENCRITERIAIMPL:
				return (T) createBetweenCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTISNULLCRITERIAIMPL:
				return (T) createIsNullCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSUBQUERYSETCRITERIAIMPL:
				return (T) createSubquerySetCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETCRITERIAIMPL:
				return (T) createSetCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXISTSCRITERIAIMPL:
				return (T) createExistsCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTGROUPBYIMPL:
				return (T) createGroupByImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTORDERBYIMPL:
				return (T) createOrderByImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTORDERBYITEMIMPL:
				return (T) createOrderByItemImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTLIMITIMPL:
				return (T) createLimitImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTOPTIONIMPL:
				return (T) createOptionImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTREFERENCEIMPL:
				return (T) createReferenceImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCASEEXPRESSIONIMPL:
				return (T) createCaseExpressionImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSEARCHEDCASEEXPRESSIONIMPL:
				return (T) createSearchedCaseExpressionImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTFUNCTIONIMPL:
				return (T) createFunctionImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLPARSEIMPL:
				return (T) createXMLParseImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTQUERYSTRINGIMPL:
				return (T) createQueryStringImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLELEMENTIMPL:
				return (T) createXMLElementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLATTRIBUTESIMPL:
				return (T) createXMLAttributesImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTJSONOBJECTIMPL:
				return (T) createJSONObjectImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLFORESTIMPL:
				return (T) createXMLForestImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLNAMESPACESIMPL:
				return (T) createXMLNamespacesImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTNAMESPACEITEM:
				return (T) createNamespaceItem(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTASSIGNMENTSTATEMENTIMPL:
				return (T) createAssignmentStatementImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSCALARSUBQUERYIMPL:
				return (T) createScalarSubqueryImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTGROUPSYMBOLIMPL:
				return (T) createGroupSymbolImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCONSTANTIMPL:
				return (T) createConstantImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTELEMENTSYMBOLIMPL:
				return (T) createElementSymbolImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTBLOCKIMPL:
				return (T) createBlockImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXPRESSIONCRITERIAIMPL:
				return (T) createExpressionCriteriaImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALIASSYMBOLIMPL:
				return (T) createAliasSymbolImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTAGGREGATESYMBOLIMPL:
				return (T) createAggregateSymbolImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWINDOWFUNCTIONIMPL:
				return (T) createWindowFunctionImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWINDOWSPECIFICATIONIMPL:
				return (T) createWindowSpecificationImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTTEXTLINEIMPL:
				return (T) createTextLineImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALTERTRIGGERIMPL:
				return (T) createAlterTriggerImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALTERPROCEDUREIMPL:
				return (T) createAlterProcedureImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALTERVIEWIMPL:
				return (T) createAlterViewImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTARRAYSYMBOLIMPL:
				return (T) createArraySymbolImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSPPARAMETERIMPL:
				return (T) createSPParameterImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSOURCEHINTIMPL:
				return (T) createSourceHintImpl(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSPECIFICHINT:
				return (T) createSpecificHint(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSUBQUERYHINT:
				return (T) createSubqueryHint(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTMAKEDEP:
				return (T) createMakeDep(teiidParser, nodeType);
		default:
			throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
		}
	}


}