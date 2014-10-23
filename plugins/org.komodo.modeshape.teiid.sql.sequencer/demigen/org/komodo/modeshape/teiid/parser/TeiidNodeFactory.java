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

import org.komodo.modeshape.teiid.parser.ITeiidParser;
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
	public static LanguageObject jjtCreate(ITeiidParser teiidParser, int nodeType) {
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
	public <T extends LanguageObject> T create(ITeiidParser teiidParser, ASTNodes nodeType) {

		for (int i = 0; i < TeiidParserTreeConstants.jjtNodeName.length; ++i) {
			String constantName = TeiidParserTreeConstants.jjtNodeName[i];
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
	private TriggerAction createTriggerAction(ITeiidParser teiidParser, int nodeType) {
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
	private RaiseStatement createRaiseStatement(ITeiidParser teiidParser, int nodeType) {
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
	private ExceptionExpression createExceptionExpression(ITeiidParser teiidParser, int nodeType) {
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
	private BranchingStatement createBranchingStatement(ITeiidParser teiidParser, int nodeType) {
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
	private ReturnStatement createReturnStatement(ITeiidParser teiidParser, int nodeType) {
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
	private WhileStatement createWhileStatement(ITeiidParser teiidParser, int nodeType) {
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
	private LoopStatement createLoopStatement(ITeiidParser teiidParser, int nodeType) {
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
	private IfStatement createIfStatement(ITeiidParser teiidParser, int nodeType) {
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
	private DeclareStatement createDeclareStatement(ITeiidParser teiidParser, int nodeType) {
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
	private CommandStatement createCommandStatement(ITeiidParser teiidParser, int nodeType) {
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
	private CreateProcedureCommand createCreateProcedureCommand(ITeiidParser teiidParser, int nodeType) {
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
	private DynamicCommand createDynamicCommand(ITeiidParser teiidParser, int nodeType) {
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
	private SetClauseList createSetClauseList(ITeiidParser teiidParser, int nodeType) {
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
	private SetClause createSetClause(ITeiidParser teiidParser, int nodeType) {
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
	private ProjectedColumn createProjectedColumn(ITeiidParser teiidParser, int nodeType) {
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
	private StoredProcedure createStoredProcedure(ITeiidParser teiidParser, int nodeType) {
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
	private Insert createInsert(ITeiidParser teiidParser, int nodeType) {
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
	private Update createUpdate(ITeiidParser teiidParser, int nodeType) {
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
	private Delete createDelete(ITeiidParser teiidParser, int nodeType) {
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
	private WithQueryCommand createWithQueryCommand(ITeiidParser teiidParser, int nodeType) {
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
	private SetQuery createSetQuery(ITeiidParser teiidParser, int nodeType) {
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
	private Query createQuery(ITeiidParser teiidParser, int nodeType) {
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
	private Into createInto(ITeiidParser teiidParser, int nodeType) {
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
	private Select createSelect(ITeiidParser teiidParser, int nodeType) {
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
	private ExpressionSymbol createExpressionSymbol(ITeiidParser teiidParser, int nodeType) {
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
	private DerivedColumn createDerivedColumn(ITeiidParser teiidParser, int nodeType) {
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
	private MultipleElementSymbol createMultipleElementSymbol(ITeiidParser teiidParser, int nodeType) {
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
	private From createFrom(ITeiidParser teiidParser, int nodeType) {
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
	private JoinPredicate createJoinPredicate(ITeiidParser teiidParser, int nodeType) {
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
	private JoinType createJoinType(ITeiidParser teiidParser, int nodeType) {
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
	private XMLSerialize createXMLSerialize(ITeiidParser teiidParser, int nodeType) {
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
	private ArrayTable createArrayTable(ITeiidParser teiidParser, int nodeType) {
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
	private TextTable createTextTable(ITeiidParser teiidParser, int nodeType) {
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
	private TextColumn createTextColumn(ITeiidParser teiidParser, int nodeType) {
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
	private XMLQuery createXMLQuery(ITeiidParser teiidParser, int nodeType) {
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
	private ObjectTable createObjectTable(ITeiidParser teiidParser, int nodeType) {
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
	private ObjectColumn createObjectColumn(ITeiidParser teiidParser, int nodeType) {
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
	private XMLTable createXMLTable(ITeiidParser teiidParser, int nodeType) {
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
	private XMLColumn createXMLColumn(ITeiidParser teiidParser, int nodeType) {
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
	private SubqueryFromClause createSubqueryFromClause(ITeiidParser teiidParser, int nodeType) {
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
	private UnaryFromClause createUnaryFromClause(ITeiidParser teiidParser, int nodeType) {
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
	private Criteria createCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private CompoundCriteria createCompoundCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private NotCriteria createNotCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private CompareCriteria createCompareCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private SubqueryCompareCriteria createSubqueryCompareCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private MatchCriteria createMatchCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private BetweenCriteria createBetweenCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private IsNullCriteria createIsNullCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private SubquerySetCriteria createSubquerySetCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private SetCriteria createSetCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private ExistsCriteria createExistsCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private GroupBy createGroupBy(ITeiidParser teiidParser, int nodeType) {
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
	private OrderBy createOrderBy(ITeiidParser teiidParser, int nodeType) {
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
	private OrderByItem createOrderByItem(ITeiidParser teiidParser, int nodeType) {
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
	private Limit createLimit(ITeiidParser teiidParser, int nodeType) {
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
	private Option createOption(ITeiidParser teiidParser, int nodeType) {
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
	private Reference createReference(ITeiidParser teiidParser, int nodeType) {
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
	private CaseExpression createCaseExpression(ITeiidParser teiidParser, int nodeType) {
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
	private SearchedCaseExpression createSearchedCaseExpression(ITeiidParser teiidParser, int nodeType) {
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
	private Function createFunction(ITeiidParser teiidParser, int nodeType) {
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
	private XMLParse createXMLParse(ITeiidParser teiidParser, int nodeType) {
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
	private QueryString createQueryString(ITeiidParser teiidParser, int nodeType) {
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
	private XMLElement createXMLElement(ITeiidParser teiidParser, int nodeType) {
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
	private XMLAttributes createXMLAttributes(ITeiidParser teiidParser, int nodeType) {
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
	private JSONObject createJSONObject(ITeiidParser teiidParser, int nodeType) {
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
	private XMLForest createXMLForest(ITeiidParser teiidParser, int nodeType) {
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
	private XMLNamespaces createXMLNamespaces(ITeiidParser teiidParser, int nodeType) {
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
	private NamespaceItem createNamespaceItem(ITeiidParser teiidParser, int nodeType) {
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
	private AssignmentStatement createAssignmentStatement(ITeiidParser teiidParser, int nodeType) {
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
	private ScalarSubquery createScalarSubquery(ITeiidParser teiidParser, int nodeType) {
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
	private GroupSymbol createGroupSymbol(ITeiidParser teiidParser, int nodeType) {
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
	private Constant createConstant(ITeiidParser teiidParser, int nodeType) {
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
	private ElementSymbol createElementSymbol(ITeiidParser teiidParser, int nodeType) {
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
	private Block createBlock(ITeiidParser teiidParser, int nodeType) {
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
	private ExpressionCriteria createExpressionCriteria(ITeiidParser teiidParser, int nodeType) {
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
	private AliasSymbol createAliasSymbol(ITeiidParser teiidParser, int nodeType) {
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
	private AggregateSymbol createAggregateSymbol(ITeiidParser teiidParser, int nodeType) {
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
	private WindowFunction createWindowFunction(ITeiidParser teiidParser, int nodeType) {
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
	private WindowSpecification createWindowSpecification(ITeiidParser teiidParser, int nodeType) {
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
	private TextLine createTextLine(ITeiidParser teiidParser, int nodeType) {
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
	private AlterTrigger createAlterTrigger(ITeiidParser teiidParser, int nodeType) {
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
	private AlterProcedure createAlterProcedure(ITeiidParser teiidParser, int nodeType) {
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
	private AlterView createAlterView(ITeiidParser teiidParser, int nodeType) {
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
	private Array createArray(ITeiidParser teiidParser, int nodeType) {
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
	private SPParameter createSPParameter(ITeiidParser teiidParser, int nodeType) {
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
	private SourceHint createSourceHint(ITeiidParser teiidParser, int nodeType) {
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
	private SpecificHint createSpecificHint(ITeiidParser teiidParser, int nodeType) {
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
	private SubqueryHint createSubqueryHint(ITeiidParser teiidParser, int nodeType) {
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
	private MakeDep createMakeDep(ITeiidParser teiidParser, int nodeType) {
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
	protected <T extends LanguageObject> T create(ITeiidParser teiidParser, int nodeType) {
		switch (nodeType) {
			case TeiidParserTreeConstants.JJTTRIGGERACTION:
				return (T) createTriggerAction(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTRAISESTATEMENT:
				return (T) createRaiseStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTEXCEPTIONEXPRESSION:
				return (T) createExceptionExpression(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTBRANCHINGSTATEMENT:
				return (T) createBranchingStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTRETURNSTATEMENT:
				return (T) createReturnStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTWHILESTATEMENT:
				return (T) createWhileStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTLOOPSTATEMENT:
				return (T) createLoopStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTIFSTATEMENT:
				return (T) createIfStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTDECLARESTATEMENT:
				return (T) createDeclareStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTCOMMANDSTATEMENT:
				return (T) createCommandStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTCREATEPROCEDURECOMMAND:
				return (T) createCreateProcedureCommand(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTDYNAMICCOMMAND:
				return (T) createDynamicCommand(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSETCLAUSELIST:
				return (T) createSetClauseList(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSETCLAUSE:
				return (T) createSetClause(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTPROJECTEDCOLUMN:
				return (T) createProjectedColumn(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSTOREDPROCEDURE:
				return (T) createStoredProcedure(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTINSERT:
				return (T) createInsert(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTUPDATE:
				return (T) createUpdate(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTDELETE:
				return (T) createDelete(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTWITHQUERYCOMMAND:
				return (T) createWithQueryCommand(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSETQUERY:
				return (T) createSetQuery(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTQUERY:
				return (T) createQuery(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTINTO:
				return (T) createInto(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSELECT:
				return (T) createSelect(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTEXPRESSIONSYMBOL:
				return (T) createExpressionSymbol(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTDERIVEDCOLUMN:
				return (T) createDerivedColumn(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTMULTIPLEELEMENTSYMBOL:
				return (T) createMultipleElementSymbol(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTFROM:
				return (T) createFrom(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTJOINPREDICATE:
				return (T) createJoinPredicate(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTJOINTYPE:
				return (T) createJoinType(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLSERIALIZE:
				return (T) createXMLSerialize(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTARRAYTABLE:
				return (T) createArrayTable(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTTEXTTABLE:
				return (T) createTextTable(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTTEXTCOLUMN:
				return (T) createTextColumn(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLQUERY:
				return (T) createXMLQuery(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTOBJECTTABLE:
				return (T) createObjectTable(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTOBJECTCOLUMN:
				return (T) createObjectColumn(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLTABLE:
				return (T) createXMLTable(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLCOLUMN:
				return (T) createXMLColumn(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSUBQUERYFROMCLAUSE:
				return (T) createSubqueryFromClause(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTUNARYFROMCLAUSE:
				return (T) createUnaryFromClause(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTCRITERIA:
				return (T) createCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTCOMPOUNDCRITERIA:
				return (T) createCompoundCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTNOTCRITERIA:
				return (T) createNotCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTCOMPARECRITERIA:
				return (T) createCompareCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSUBQUERYCOMPARECRITERIA:
				return (T) createSubqueryCompareCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTMATCHCRITERIA:
				return (T) createMatchCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTBETWEENCRITERIA:
				return (T) createBetweenCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTISNULLCRITERIA:
				return (T) createIsNullCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSUBQUERYSETCRITERIA:
				return (T) createSubquerySetCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSETCRITERIA:
				return (T) createSetCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTEXISTSCRITERIA:
				return (T) createExistsCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTGROUPBY:
				return (T) createGroupBy(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTORDERBY:
				return (T) createOrderBy(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTORDERBYITEM:
				return (T) createOrderByItem(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTLIMIT:
				return (T) createLimit(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTOPTION:
				return (T) createOption(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTREFERENCE:
				return (T) createReference(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTCASEEXPRESSION:
				return (T) createCaseExpression(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSEARCHEDCASEEXPRESSION:
				return (T) createSearchedCaseExpression(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTFUNCTION:
				return (T) createFunction(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLPARSE:
				return (T) createXMLParse(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTQUERYSTRING:
				return (T) createQueryString(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLELEMENT:
				return (T) createXMLElement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLATTRIBUTES:
				return (T) createXMLAttributes(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTJSONOBJECT:
				return (T) createJSONObject(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLFOREST:
				return (T) createXMLForest(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTXMLNAMESPACES:
				return (T) createXMLNamespaces(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTNAMESPACEITEM:
				return (T) createNamespaceItem(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTASSIGNMENTSTATEMENT:
				return (T) createAssignmentStatement(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSCALARSUBQUERY:
				return (T) createScalarSubquery(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTGROUPSYMBOL:
				return (T) createGroupSymbol(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTCONSTANT:
				return (T) createConstant(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTELEMENTSYMBOL:
				return (T) createElementSymbol(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTBLOCK:
				return (T) createBlock(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTEXPRESSIONCRITERIA:
				return (T) createExpressionCriteria(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTALIASSYMBOL:
				return (T) createAliasSymbol(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTAGGREGATESYMBOL:
				return (T) createAggregateSymbol(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTWINDOWFUNCTION:
				return (T) createWindowFunction(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTWINDOWSPECIFICATION:
				return (T) createWindowSpecification(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTTEXTLINE:
				return (T) createTextLine(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTALTERTRIGGER:
				return (T) createAlterTrigger(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTALTERPROCEDURE:
				return (T) createAlterProcedure(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTALTERVIEW:
				return (T) createAlterView(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTARRAY:
				return (T) createArray(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSPPARAMETER:
				return (T) createSPParameter(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSOURCEHINT:
				return (T) createSourceHint(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSPECIFICHINT:
				return (T) createSpecificHint(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTSUBQUERYHINT:
				return (T) createSubqueryHint(teiidParser, nodeType);
			case TeiidParserTreeConstants.JJTMAKEDEP:
				return (T) createMakeDep(teiidParser, nodeType);
		default:
			throw new IllegalArgumentException(Messages.getString(Messages.TeiidParser.invalidNodeType, nodeType, teiidParser.getVersion()));
		}
	}


}