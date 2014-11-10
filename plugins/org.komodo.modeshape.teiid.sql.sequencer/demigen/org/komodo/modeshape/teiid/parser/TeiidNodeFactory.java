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
	public static LanguageObject jjtCreate(TeiidSeqParser teiidParser, int nodeType) {
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
	public <T extends LanguageObject> T create(TeiidSeqParser teiidParser, ASTNodes nodeType) {

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
		TRIGGER_ACTION("TriggerAction"), //$NON-NLS-1$

		/**
		 * RaiseStatement
		 * @generated
		 */
		RAISE_STATEMENT("RaiseStatement"), //$NON-NLS-1$

		/**
		 * ExceptionExpression
		 * @generated
		 */
		EXCEPTION_EXPRESSION("ExceptionExpression"), //$NON-NLS-1$

		/**
		 * BranchingStatement
		 * @generated
		 */
		BRANCHING_STATEMENT("BranchingStatement"), //$NON-NLS-1$

		/**
		 * ReturnStatement
		 * @generated
		 */
		RETURN_STATEMENT("ReturnStatement"), //$NON-NLS-1$

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
		 * CreateProcedureCommand
		 * @generated
		 */
		CREATE_PROCEDURE_COMMAND("CreateProcedureCommand"), //$NON-NLS-1$

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
		 * ExpressionSymbol
		 * @generated
		 */
		EXPRESSION_SYMBOL("ExpressionSymbol"), //$NON-NLS-1$

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
		 * ObjectTable
		 * @generated
		 */
		OBJECT_TABLE("ObjectTable"), //$NON-NLS-1$

		/**
		 * ObjectColumn
		 * @generated
		 */
		OBJECT_COLUMN("ObjectColumn"), //$NON-NLS-1$

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
		 * JSONObject
		 * @generated
		 */
		JSON_OBJECT("JSONObject"), //$NON-NLS-1$

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
		 * NamespaceItem
		 * @generated
		 */
		NAMESPACE_ITEM("NamespaceItem"), //$NON-NLS-1$

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
		 * Array
		 * @generated
		 */
		ARRAY("Array"), //$NON-NLS-1$

		/**
		 * SPParameter
		 * @generated
		 */
		SP_PARAMETER("SPParameter"), //$NON-NLS-1$

		/**
		 * SourceHint
		 * @generated
		 */
		SOURCE_HINT("SourceHint"), //$NON-NLS-1$

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
	private TriggerAction createTriggerAction(TeiidSeqParser teiidParser, int nodeType) {
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
	private RaiseStatement createRaiseStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExceptionExpression createExceptionExpression(TeiidSeqParser teiidParser, int nodeType) {
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
	private BranchingStatement createBranchingStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private ReturnStatement createReturnStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private WhileStatement createWhileStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private LoopStatement createLoopStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private IfStatement createIfStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private DeclareStatement createDeclareStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private CommandStatement createCommandStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private CreateProcedureCommand createCreateProcedureCommand(TeiidSeqParser teiidParser, int nodeType) {
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
	private DynamicCommand createDynamicCommand(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetClauseList createSetClauseList(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetClause createSetClause(TeiidSeqParser teiidParser, int nodeType) {
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
	private ProjectedColumn createProjectedColumn(TeiidSeqParser teiidParser, int nodeType) {
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
	private StoredProcedure createStoredProcedure(TeiidSeqParser teiidParser, int nodeType) {
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
	private Insert createInsert(TeiidSeqParser teiidParser, int nodeType) {
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
	private Update createUpdate(TeiidSeqParser teiidParser, int nodeType) {
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
	private Delete createDelete(TeiidSeqParser teiidParser, int nodeType) {
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
	private WithQueryCommand createWithQueryCommand(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetQuery createSetQuery(TeiidSeqParser teiidParser, int nodeType) {
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
	private Query createQuery(TeiidSeqParser teiidParser, int nodeType) {
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
	private Into createInto(TeiidSeqParser teiidParser, int nodeType) {
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
	private Select createSelect(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExpressionSymbol createExpressionSymbol(TeiidSeqParser teiidParser, int nodeType) {
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
	private DerivedColumn createDerivedColumn(TeiidSeqParser teiidParser, int nodeType) {
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
	private MultipleElementSymbol createMultipleElementSymbol(TeiidSeqParser teiidParser, int nodeType) {
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
	private From createFrom(TeiidSeqParser teiidParser, int nodeType) {
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
	private JoinPredicate createJoinPredicate(TeiidSeqParser teiidParser, int nodeType) {
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
	private JoinType createJoinType(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLSerialize createXMLSerialize(TeiidSeqParser teiidParser, int nodeType) {
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
	private ArrayTable createArrayTable(TeiidSeqParser teiidParser, int nodeType) {
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
	private TextTable createTextTable(TeiidSeqParser teiidParser, int nodeType) {
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
	private TextColumn createTextColumn(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLQuery createXMLQuery(TeiidSeqParser teiidParser, int nodeType) {
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
	private ObjectTable createObjectTable(TeiidSeqParser teiidParser, int nodeType) {
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
	private ObjectColumn createObjectColumn(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLTable createXMLTable(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLColumn createXMLColumn(TeiidSeqParser teiidParser, int nodeType) {
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
	private SubqueryFromClause createSubqueryFromClause(TeiidSeqParser teiidParser, int nodeType) {
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
	private UnaryFromClause createUnaryFromClause(TeiidSeqParser teiidParser, int nodeType) {
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
	private Criteria createCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private CompoundCriteria createCompoundCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private NotCriteria createNotCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private CompareCriteria createCompareCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private SubqueryCompareCriteria createSubqueryCompareCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private MatchCriteria createMatchCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private BetweenCriteria createBetweenCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private IsNullCriteria createIsNullCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private SubquerySetCriteria createSubquerySetCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private SetCriteria createSetCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExistsCriteria createExistsCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private GroupBy createGroupBy(TeiidSeqParser teiidParser, int nodeType) {
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
	private OrderBy createOrderBy(TeiidSeqParser teiidParser, int nodeType) {
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
	private OrderByItem createOrderByItem(TeiidSeqParser teiidParser, int nodeType) {
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
	private Limit createLimit(TeiidSeqParser teiidParser, int nodeType) {
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
	private Option createOption(TeiidSeqParser teiidParser, int nodeType) {
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
	private Reference createReference(TeiidSeqParser teiidParser, int nodeType) {
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
	private CaseExpression createCaseExpression(TeiidSeqParser teiidParser, int nodeType) {
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
	private SearchedCaseExpression createSearchedCaseExpression(TeiidSeqParser teiidParser, int nodeType) {
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
	private Function createFunction(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLParse createXMLParse(TeiidSeqParser teiidParser, int nodeType) {
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
	private QueryString createQueryString(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLElement createXMLElement(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLAttributes createXMLAttributes(TeiidSeqParser teiidParser, int nodeType) {
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
	private JSONObject createJSONObject(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLForest createXMLForest(TeiidSeqParser teiidParser, int nodeType) {
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
	private XMLNamespaces createXMLNamespaces(TeiidSeqParser teiidParser, int nodeType) {
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
	private AssignmentStatement createAssignmentStatement(TeiidSeqParser teiidParser, int nodeType) {
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
	private ScalarSubquery createScalarSubquery(TeiidSeqParser teiidParser, int nodeType) {
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
	private GroupSymbol createGroupSymbol(TeiidSeqParser teiidParser, int nodeType) {
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
	private Constant createConstant(TeiidSeqParser teiidParser, int nodeType) {
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
	private ElementSymbol createElementSymbol(TeiidSeqParser teiidParser, int nodeType) {
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
	private Block createBlock(TeiidSeqParser teiidParser, int nodeType) {
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
	private ExpressionCriteria createExpressionCriteria(TeiidSeqParser teiidParser, int nodeType) {
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
	private AliasSymbol createAliasSymbol(TeiidSeqParser teiidParser, int nodeType) {
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
	private AggregateSymbol createAggregateSymbol(TeiidSeqParser teiidParser, int nodeType) {
		return new AggregateSymbol(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WindowFunction createWindowFunction(TeiidSeqParser teiidParser, int nodeType) {
		return new WindowFunction(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private WindowSpecification createWindowSpecification(TeiidSeqParser teiidParser, int nodeType) {
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
	private TextLine createTextLine(TeiidSeqParser teiidParser, int nodeType) {
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
	private AlterTrigger createAlterTrigger(TeiidSeqParser teiidParser, int nodeType) {
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
	private AlterProcedure createAlterProcedure(TeiidSeqParser teiidParser, int nodeType) {
		return new AlterProcedure(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private AlterView createAlterView(TeiidSeqParser teiidParser, int nodeType) {
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
	private Array createArray(TeiidSeqParser teiidParser, int nodeType) {
		return new Array(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SPParameter createSPParameter(TeiidSeqParser teiidParser, int nodeType) {
		return new SPParameter(teiidParser, nodeType);
	}

	/**
	 *
	 * @generated
	 *
	 * @param teiidParser
	 * @param nodeType
	 * @return
	 */
	private SourceHint createSourceHint(TeiidSeqParser teiidParser, int nodeType) {
		return new SourceHint(teiidParser, nodeType);
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
	protected <T extends LanguageObject> T create(TeiidSeqParser teiidParser, int nodeType) {
		switch (nodeType) {
			case TeiidSequencingParserTreeConstants.JJTTRIGGERACTION:
				return (T) createTriggerAction(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTRAISESTATEMENT:
				return (T) createRaiseStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXCEPTIONEXPRESSION:
				return (T) createExceptionExpression(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTBRANCHINGSTATEMENT:
				return (T) createBranchingStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTRETURNSTATEMENT:
				return (T) createReturnStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWHILESTATEMENT:
				return (T) createWhileStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTLOOPSTATEMENT:
				return (T) createLoopStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTIFSTATEMENT:
				return (T) createIfStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDECLARESTATEMENT:
				return (T) createDeclareStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCOMMANDSTATEMENT:
				return (T) createCommandStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCREATEPROCEDURECOMMAND:
				return (T) createCreateProcedureCommand(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDYNAMICCOMMAND:
				return (T) createDynamicCommand(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETCLAUSELIST:
				return (T) createSetClauseList(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETCLAUSE:
				return (T) createSetClause(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTPROJECTEDCOLUMN:
				return (T) createProjectedColumn(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSTOREDPROCEDURE:
				return (T) createStoredProcedure(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTINSERT:
				return (T) createInsert(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTUPDATE:
				return (T) createUpdate(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDELETE:
				return (T) createDelete(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWITHQUERYCOMMAND:
				return (T) createWithQueryCommand(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETQUERY:
				return (T) createSetQuery(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTQUERY:
				return (T) createQuery(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTINTO:
				return (T) createInto(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSELECT:
				return (T) createSelect(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXPRESSIONSYMBOL:
				return (T) createExpressionSymbol(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTDERIVEDCOLUMN:
				return (T) createDerivedColumn(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTMULTIPLEELEMENTSYMBOL:
				return (T) createMultipleElementSymbol(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTFROM:
				return (T) createFrom(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTJOINPREDICATE:
				return (T) createJoinPredicate(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTJOINTYPE:
				return (T) createJoinType(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLSERIALIZE:
				return (T) createXMLSerialize(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTARRAYTABLE:
				return (T) createArrayTable(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTTEXTTABLE:
				return (T) createTextTable(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTTEXTCOLUMN:
				return (T) createTextColumn(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLQUERY:
				return (T) createXMLQuery(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTOBJECTTABLE:
				return (T) createObjectTable(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTOBJECTCOLUMN:
				return (T) createObjectColumn(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLTABLE:
				return (T) createXMLTable(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLCOLUMN:
				return (T) createXMLColumn(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSUBQUERYFROMCLAUSE:
				return (T) createSubqueryFromClause(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTUNARYFROMCLAUSE:
				return (T) createUnaryFromClause(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCRITERIA:
				return (T) createCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCOMPOUNDCRITERIA:
				return (T) createCompoundCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTNOTCRITERIA:
				return (T) createNotCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCOMPARECRITERIA:
				return (T) createCompareCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSUBQUERYCOMPARECRITERIA:
				return (T) createSubqueryCompareCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTMATCHCRITERIA:
				return (T) createMatchCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTBETWEENCRITERIA:
				return (T) createBetweenCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTISNULLCRITERIA:
				return (T) createIsNullCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSUBQUERYSETCRITERIA:
				return (T) createSubquerySetCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSETCRITERIA:
				return (T) createSetCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXISTSCRITERIA:
				return (T) createExistsCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTGROUPBY:
				return (T) createGroupBy(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTORDERBY:
				return (T) createOrderBy(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTORDERBYITEM:
				return (T) createOrderByItem(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTLIMIT:
				return (T) createLimit(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTOPTION:
				return (T) createOption(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTREFERENCE:
				return (T) createReference(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCASEEXPRESSION:
				return (T) createCaseExpression(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSEARCHEDCASEEXPRESSION:
				return (T) createSearchedCaseExpression(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTFUNCTION:
				return (T) createFunction(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLPARSE:
				return (T) createXMLParse(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTQUERYSTRING:
				return (T) createQueryString(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLELEMENT:
				return (T) createXMLElement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLATTRIBUTES:
				return (T) createXMLAttributes(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTJSONOBJECT:
				return (T) createJSONObject(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLFOREST:
				return (T) createXMLForest(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTXMLNAMESPACES:
				return (T) createXMLNamespaces(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTNAMESPACEITEM:
				return (T) createNamespaceItem(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTASSIGNMENTSTATEMENT:
				return (T) createAssignmentStatement(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSCALARSUBQUERY:
				return (T) createScalarSubquery(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTGROUPSYMBOL:
				return (T) createGroupSymbol(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTCONSTANT:
				return (T) createConstant(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTELEMENTSYMBOL:
				return (T) createElementSymbol(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTBLOCK:
				return (T) createBlock(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTEXPRESSIONCRITERIA:
				return (T) createExpressionCriteria(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALIASSYMBOL:
				return (T) createAliasSymbol(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTAGGREGATESYMBOL:
				return (T) createAggregateSymbol(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWINDOWFUNCTION:
				return (T) createWindowFunction(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTWINDOWSPECIFICATION:
				return (T) createWindowSpecification(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTTEXTLINE:
				return (T) createTextLine(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALTERTRIGGER:
				return (T) createAlterTrigger(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALTERPROCEDURE:
				return (T) createAlterProcedure(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTALTERVIEW:
				return (T) createAlterView(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTARRAY:
				return (T) createArray(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSPPARAMETER:
				return (T) createSPParameter(teiidParser, nodeType);
			case TeiidSequencingParserTreeConstants.JJTSOURCEHINT:
				return (T) createSourceHint(teiidParser, nodeType);
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