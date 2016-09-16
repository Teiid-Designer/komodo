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

package org.komodo.spi.lexicon;

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;


/**
 * @generated using org.komodo.modeshape.teiid.generators.TeiidCndGenerator
 */
@SuppressWarnings( { "javadoc", "nls" })
public class TeiidSqlLexicon implements StringConstants {

	public interface Namespace {
		public static final String PREFIX = "tsql";
		public static final String URI = "http://www.teiid.org/sql/1.0";
	}

	/**
	 * tsql:expressionStatement
	 */
	public interface ExpressionStatement {

		String ID = Namespace.PREFIX + COLON + "expressionStatement";

		/**
		 * EXPECTED_TYPE_CLASS Property
		 */
		String EXPECTED_TYPE_CLASS_PROP_NAME = Namespace.PREFIX + COLON + "expectedTypeClass";

		Class<?> EXPECTED_TYPE_CLASS_PROP_TYPE =  String.class;

		boolean EXPECTED_TYPE_CLASS_PROP_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:languageObject
	 */
	public interface LanguageObject {

		String ID = Namespace.PREFIX + COLON + "languageObject";

		/**
		 * TEIID_VERSION Property
		 */
		String TEIID_VERSION_PROP_NAME = Namespace.PREFIX + COLON + "teiidVersion";

		Class<?> TEIID_VERSION_PROP_TYPE =  String.class;

		boolean TEIID_VERSION_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:expression
	 */
	public interface Expression extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "expression";

		/**
		 * TYPE_CLASS Property
		 */
		String TYPE_CLASS_PROP_NAME = Namespace.PREFIX + COLON + "typeClass";

		Class<?> TYPE_CLASS_PROP_TYPE =  String.class;

		boolean TYPE_CLASS_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:predicateCriteria
	 */
	public interface PredicateCriteria extends Expression {

		String ID = Namespace.PREFIX + COLON + "predicateCriteria";

	}

	/**
	 * tsql:isDistinctCriteria
	 */
	public interface IsDistinctCriteria extends PredicateCriteria {

	    String ID = Namespace.PREFIX + COLON + "isDistinctCriteria";

	    /**
	     * NEGATED Property
	     */
	    String NEGATED_PROP_NAME = Namespace.PREFIX + COLON + "negated";

	    Class<?> NEGATED_PROP_TYPE =  Boolean.class;

	    boolean NEGATED_PROP_MULTIPLE = false;

	    /**
	     * LEFT_ROW_VALUE Reference
	     */
	    String LEFT_ROW_VALUE_REF_NAME = Namespace.PREFIX + COLON + "leftRowValue";

	    Class<?> LEFT_ROW_VALUE_REF_TYPE =  GroupSymbol.class;

	    boolean LEFT_ROW_VALUE_REF_MULTIPLE = false;

	    /**
	     * RIGHT_ROW_VALUE Reference
	     */
	    String RIGHT_ROW_VALUE_REF_NAME = Namespace.PREFIX + COLON + "leftRowValue";

	    Class<?> RIGHT_ROW_VALUE_REF_TYPE =  GroupSymbol.class;

	    boolean RIGHT_ROW_VALUE_REF_MULTIPLE = false;
	}

	/**
	 * tsql:subqueryContainer
	 */
	public interface SubqueryContainer extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "subqueryContainer";

		/**
		 * COMMAND Reference
		 */
		String COMMAND_REF_NAME = Namespace.PREFIX + COLON + "command";

		Class<?> COMMAND_REF_TYPE =  Command.class;

		boolean COMMAND_REF_MULTIPLE = false;

	}

	/**
	 * tsql:targetedCommand
	 */
	public interface TargetedCommand {

		String ID = Namespace.PREFIX + COLON + "targetedCommand";

		/**
		 * GROUP Reference
		 */
		String GROUP_REF_NAME = Namespace.PREFIX + COLON + "group";

		Class<?> GROUP_REF_TYPE =  GroupSymbol.class;

		boolean GROUP_REF_MULTIPLE = false;

	}

	/**
	 * tsql:labeled
	 */
	public interface Labeled {

		String ID = Namespace.PREFIX + COLON + "labeled";

		/**
		 * LABEL Property
		 */
		String LABEL_PROP_NAME = Namespace.PREFIX + COLON + "label";

		Class<?> LABEL_PROP_TYPE =  String.class;

		boolean LABEL_PROP_MULTIPLE = false;

	}

	/**
     * tsql:create
     */
    public interface Create extends LanguageObject {

        String ID = Namespace.PREFIX + COLON + "create";

        boolean IS_ABSTRACT = false;

        String ON_COMMIT_PROP_NAME="onCommit";
        String ON_COMMIT_PROP_VALUE="PRESERVE_ROWS";
        String COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "column";
        Class<?> COLUMNS_REF_TYPE =  ElementSymbol.class;
        boolean COLUMNS_REF_MULTIPLE = true;

        String PRIMARY_KEY_COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "primaryKeyColumn";
        Class<?> PRIMARY_KEY_COLUMNS_REF_TYPE =  ElementSymbol.class;
        boolean PRIMARY_KEY_COLUMNS_REF_MULTIPLE = true;

        String TABLE_REF_NAME = Namespace.PREFIX + COLON + "table";

    }

	/**
	 * tsql:criteria
	 */
	public interface Criteria extends Expression {

		String ID = Namespace.PREFIX + COLON + "criteria";

		boolean IS_ABSTRACT = false;

	}

	/**
	 * tsql:abstractCompareCriteria
	 */
	public interface AbstractCompareCriteria extends Criteria, PredicateCriteria {

		String ID = Namespace.PREFIX + COLON + "abstractCompareCriteria";

		boolean IS_ABSTRACT = true;

		/**
		 * OPERATOR Property
		 */
		String OPERATOR_PROP_NAME = Namespace.PREFIX + COLON + "operator";

		Class<?> OPERATOR_PROP_TYPE =  String.class;

		boolean OPERATOR_PROP_MULTIPLE = false;

		String[] OPERATOR_PROP_CONSTRAINTS = { "EQ", "NE", "LT", "GT", "LE", "GE" };

		/**
		 * LEFT_EXPRESSION Reference
		 */
		String LEFT_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "leftExpression";

		Class<?> LEFT_EXPRESSION_REF_TYPE =  Expression.class;

		boolean LEFT_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:compareCriteria
	 */
	public interface CompareCriteria extends AbstractCompareCriteria {

		String ID = Namespace.PREFIX + COLON + "compareCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * OPTIONAL Property
		 */
		String OPTIONAL_PROP_NAME = Namespace.PREFIX + COLON + "optional";

		Class<?> OPTIONAL_PROP_TYPE =  Boolean.class;

		boolean OPTIONAL_PROP_MULTIPLE = false;

		/**
		 * RIGHT_EXPRESSION Reference
		 */
		String RIGHT_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "rightExpression";

		Class<?> RIGHT_EXPRESSION_REF_TYPE =  Expression.class;

		boolean RIGHT_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:subqueryCompareCriteria
	 */
	public interface SubqueryCompareCriteria extends AbstractCompareCriteria, SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "subqueryCompareCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * PREDICATE_QUANTIFIER Property
		 */
		String PREDICATE_QUANTIFIER_PROP_NAME = Namespace.PREFIX + COLON + "predicateQuantifier";

		Class<?> PREDICATE_QUANTIFIER_PROP_TYPE =  String.class;

		boolean PREDICATE_QUANTIFIER_PROP_MULTIPLE = false;

		String[] PREDICATE_QUANTIFIER_PROP_CONSTRAINTS = { "SOME", "ANY", "ALL" };

	}

	/**
	 * tsql:abstractSetCriteria
	 */
	public interface AbstractSetCriteria extends Criteria, PredicateCriteria {

		String ID = Namespace.PREFIX + COLON + "abstractSetCriteria";

		boolean IS_ABSTRACT = true;

		/**
		 * NEGATED Property
		 */
		String NEGATED_PROP_NAME = Namespace.PREFIX + COLON + "negated";

		Class<?> NEGATED_PROP_TYPE =  Boolean.class;

		boolean NEGATED_PROP_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:setCriteria
	 */
	public interface SetCriteria extends AbstractSetCriteria {

		String ID = Namespace.PREFIX + COLON + "setCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * VALUES Reference
		 */
		String VALUES_REF_NAME = Namespace.PREFIX + COLON + "values";

		Class<?> VALUES_REF_TYPE =  Expression.class;

		boolean VALUES_REF_MULTIPLE = true;

	}

	/**
	 * tsql:subquerySetCriteria
	 */
	public interface SubquerySetCriteria extends AbstractSetCriteria, SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "subquerySetCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * SUBQUERY_HINT Reference
		 */
		String SUBQUERY_HINT_REF_NAME = Namespace.PREFIX + COLON + "subqueryHint";

		Class<?> SUBQUERY_HINT_REF_TYPE =  SubqueryHint.class;

		boolean SUBQUERY_HINT_REF_MULTIPLE = false;

	}

	/**
	 * tsql:betweenCriteria
	 */
	public interface BetweenCriteria extends Criteria, PredicateCriteria {

		String ID = Namespace.PREFIX + COLON + "betweenCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * NEGATED Property
		 */
		String NEGATED_PROP_NAME = Namespace.PREFIX + COLON + "negated";

		Class<?> NEGATED_PROP_TYPE =  Boolean.class;

		boolean NEGATED_PROP_MULTIPLE = false;

		/**
		 * LOWER_EXPRESSION Reference
		 */
		String LOWER_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "lowerExpression";

		Class<?> LOWER_EXPRESSION_REF_TYPE =  Expression.class;

		boolean LOWER_EXPRESSION_REF_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

		/**
		 * UPPER_EXPRESSION Reference
		 */
		String UPPER_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "upperExpression";

		Class<?> UPPER_EXPRESSION_REF_TYPE =  Expression.class;

		boolean UPPER_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:compoundCriteria
	 */
	public interface CompoundCriteria extends Criteria {

		String ID = Namespace.PREFIX + COLON + "compoundCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * OPERATOR Property
		 */
		String OPERATOR_PROP_NAME = Namespace.PREFIX + COLON + "operator";

		Class<?> OPERATOR_PROP_TYPE =  Long.class;

		boolean OPERATOR_PROP_MULTIPLE = false;

		/**
		 * CRITERIA Reference
		 */
		String CRITERIA_REF_NAME = Namespace.PREFIX + COLON + "criteria";

		Class<?> CRITERIA_REF_TYPE =  Criteria.class;

		boolean CRITERIA_REF_MULTIPLE = true;

	}

	/**
	 * tsql:existsCriteria
	 */
	public interface ExistsCriteria extends Criteria, PredicateCriteria, SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "existsCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * NEGATED Property
		 */
		String NEGATED_PROP_NAME = Namespace.PREFIX + COLON + "negated";

		Class<?> NEGATED_PROP_TYPE =  Boolean.class;

		boolean NEGATED_PROP_MULTIPLE = false;

		/**
		 * SUBQUERY_HINT Reference
		 */
		String SUBQUERY_HINT_REF_NAME = Namespace.PREFIX + COLON + "subqueryHint";

		Class<?> SUBQUERY_HINT_REF_TYPE =  SubqueryHint.class;

		boolean SUBQUERY_HINT_REF_MULTIPLE = false;

	}

	/**
	 * tsql:expressionCriteria
	 */
	public interface ExpressionCriteria extends Criteria {

		String ID = Namespace.PREFIX + COLON + "expressionCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:isNullCriteria
	 */
	public interface IsNullCriteria extends Criteria, PredicateCriteria {

		String ID = Namespace.PREFIX + COLON + "isNullCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * NEGATED Property
		 */
		String NEGATED_PROP_NAME = Namespace.PREFIX + COLON + "negated";

		Class<?> NEGATED_PROP_TYPE =  Boolean.class;

		boolean NEGATED_PROP_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:matchCriteria
	 */
	public interface MatchCriteria extends Criteria, PredicateCriteria {

		String ID = Namespace.PREFIX + COLON + "matchCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * ESCAPE_CHAR Property
		 */
		String ESCAPE_CHAR_PROP_NAME = Namespace.PREFIX + COLON + "escapeChar";

		Class<?> ESCAPE_CHAR_PROP_TYPE =  String.class;

		boolean ESCAPE_CHAR_PROP_MULTIPLE = false;

		/**
		 * MODE Property
		 */
		String MODE_PROP_NAME = Namespace.PREFIX + COLON + "mode";

		Class<?> MODE_PROP_TYPE =  String.class;

		boolean MODE_PROP_MULTIPLE = false;

		String[] MODE_PROP_CONSTRAINTS = { "LIKE", "SIMILAR", "REGEX" };

		/**
		 * NEGATED Property
		 */
		String NEGATED_PROP_NAME = Namespace.PREFIX + COLON + "negated";

		Class<?> NEGATED_PROP_TYPE =  Boolean.class;

		boolean NEGATED_PROP_MULTIPLE = false;

		/**
		 * RIGHT_EXPRESSION Reference
		 */
		String RIGHT_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "rightExpression";

		Class<?> RIGHT_EXPRESSION_REF_TYPE =  Expression.class;

		boolean RIGHT_EXPRESSION_REF_MULTIPLE = false;

		/**
		 * LEFT_EXPRESSION Reference
		 */
		String LEFT_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "leftExpression";

		Class<?> LEFT_EXPRESSION_REF_TYPE =  Expression.class;

		boolean LEFT_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:notCriteria
	 */
	public interface NotCriteria extends Criteria {

		String ID = Namespace.PREFIX + COLON + "notCriteria";

		boolean IS_ABSTRACT = false;

		/**
		 * CRITERIA Reference
		 */
		String CRITERIA_REF_NAME = Namespace.PREFIX + COLON + "criteria";

		Class<?> CRITERIA_REF_TYPE =  Criteria.class;

		boolean CRITERIA_REF_MULTIPLE = false;

	}

	/**
	 * tsql:command
	 */
	public interface Command extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "command";

		boolean IS_ABSTRACT = true;

		/**
		 * IS_RESOLVED Property
		 */
		String IS_RESOLVED_PROP_NAME = Namespace.PREFIX + COLON + "isResolved";

		Class<?> IS_RESOLVED_PROP_TYPE =  Boolean.class;

		boolean IS_RESOLVED_PROP_MULTIPLE = false;

		/**
		 * TYPE Property
		 */
		String TYPE_PROP_NAME = Namespace.PREFIX + COLON + "type";

		Class<?> TYPE_PROP_TYPE =  Long.class;

		boolean TYPE_PROP_MULTIPLE = false;

		/**
		 * OPTION Reference
		 */
		String OPTION_REF_NAME = Namespace.PREFIX + COLON + "option";

		Class<?> OPTION_REF_TYPE =  Option.class;

		boolean OPTION_REF_MULTIPLE = false;

		/**
		 * SOURCE_HINT Reference
		 */
		String SOURCE_HINT_REF_NAME = Namespace.PREFIX + COLON + "sourceHint";

		Class<?> SOURCE_HINT_REF_TYPE =  SourceHint.class;

		boolean SOURCE_HINT_REF_MULTIPLE = false;

		/**
		 * CACHE_HINT Reference
		 */
		String CACHE_HINT_REF_NAME = Namespace.PREFIX + COLON + "cacheHint";

		Class<?> CACHE_HINT_REF_TYPE =  CacheHint.class;

		boolean CACHE_HINT_REF_MULTIPLE = false;
	}

	/**
	 * tsql:alter
	 */
	public interface Alter extends Command {

		String ID = Namespace.PREFIX + COLON + "alter";

		boolean IS_ABSTRACT = true;

		/**
		 * TARGET Reference
		 */
		String TARGET_REF_NAME = Namespace.PREFIX + COLON + "target";

		Class<?> TARGET_REF_TYPE =  GroupSymbol.class;

		boolean TARGET_REF_MULTIPLE = false;

		/**
		 * DEFINITION Reference
		 */
		String DEFINITION_REF_NAME = Namespace.PREFIX + COLON + "definition";

		Class<?> DEFINITION_REF_TYPE =  Command.class;

		boolean DEFINITION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:alterProcedure
	 */
	public interface AlterProcedure extends Alter {

		String ID = Namespace.PREFIX + COLON + "alterProcedure";

		boolean IS_ABSTRACT = false;

	}

	/**
	 * tsql:alterTrigger
	 */
	public interface AlterTrigger extends Alter {

		String ID = Namespace.PREFIX + COLON + "alterTrigger";

		boolean IS_ABSTRACT = false;

		/**
		 * ENABLED Property
		 */
		String ENABLED_PROP_NAME = Namespace.PREFIX + COLON + "enabled";

		Class<?> ENABLED_PROP_TYPE =  Boolean.class;

		boolean ENABLED_PROP_MULTIPLE = false;

		/**
		 * EVENT Property
		 */
		String EVENT_PROP_NAME = Namespace.PREFIX + COLON + "event";

		Class<?> EVENT_PROP_TYPE =  String.class;

		boolean EVENT_PROP_MULTIPLE = false;

		String[] EVENT_PROP_CONSTRAINTS = { "INSERT", "UPDATE", "DELETE" };

		/**
		 * CREATE Property
		 */
		String CREATE_PROP_NAME = Namespace.PREFIX + COLON + "create";

		Class<?> CREATE_PROP_TYPE =  Boolean.class;

		boolean CREATE_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:alterView
	 */
	public interface AlterView extends Alter {

		String ID = Namespace.PREFIX + COLON + "alterView";

		boolean IS_ABSTRACT = false;

	}

	/**
	 * tsql:procedureContainer
	 */
	public interface ProcedureContainer extends Command {

		String ID = Namespace.PREFIX + COLON + "procedureContainer";

		boolean IS_ABSTRACT = true;

	}

	/**
	 * tsql:delete
	 */
	public interface Delete extends ProcedureContainer, TargetedCommand {

		String ID = Namespace.PREFIX + COLON + "delete";

		boolean IS_ABSTRACT = false;

		/**
		 * CRITERIA Reference
		 */
		String CRITERIA_REF_NAME = Namespace.PREFIX + COLON + "criteria";

		Class<?> CRITERIA_REF_TYPE =  Criteria.class;

		boolean CRITERIA_REF_MULTIPLE = false;

	}

	/**
	 * tsql:insert
	 */
	public interface Insert extends ProcedureContainer, TargetedCommand {

		String ID = Namespace.PREFIX + COLON + "insert";

		boolean IS_ABSTRACT = false;

		/**
		 * MERGE Property
		 */
		String MERGE_PROP_NAME = Namespace.PREFIX + COLON + "merge";

		Class<?> MERGE_PROP_TYPE =  Boolean.class;

		boolean MERGE_PROP_MULTIPLE = false;

		/**
		 * VARIABLES Reference
		 */
		String VARIABLES_REF_NAME = Namespace.PREFIX + COLON + "variables";

		Class<?> VARIABLES_REF_TYPE =  ElementSymbol.class;

		boolean VARIABLES_REF_MULTIPLE = true;

		/**
		 * VALUES Reference
		 */
		String VALUES_REF_NAME = Namespace.PREFIX + COLON + "values";

		Class<?> VALUES_REF_TYPE =  Expression.class;

		boolean VALUES_REF_MULTIPLE = true;

		/**
		 * QUERY_EXPRESSION Reference
		 */
		String QUERY_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "queryExpression";

		Class<?> QUERY_EXPRESSION_REF_TYPE =  QueryCommand.class;

		boolean QUERY_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:storedProcedure
	 */
	public interface StoredProcedure extends ProcedureContainer, TargetedCommand {

		String ID = Namespace.PREFIX + COLON + "storedProcedure";

		boolean IS_ABSTRACT = false;

		/**
		 * CALLABLE_STATEMENT Property
		 */
		String CALLABLE_STATEMENT_PROP_NAME = Namespace.PREFIX + COLON + "callableStatement";

		Class<?> CALLABLE_STATEMENT_PROP_TYPE =  Boolean.class;

		boolean CALLABLE_STATEMENT_PROP_MULTIPLE = false;

		/**
		 * PROCEDUREID Property
		 */
		String PROCEDUREID_PROP_NAME = Namespace.PREFIX + COLON + "procedureiD";

		Class<?> PROCEDUREID_PROP_TYPE =  String.class;

		boolean PROCEDUREID_PROP_MULTIPLE = false;

		/**
		 * PROCEDURE_CALLABLE_NAME Property
		 */
		String PROCEDURE_CALLABLE_NAME_PROP_NAME = Namespace.PREFIX + COLON + "procedureCallableName";

		Class<?> PROCEDURE_CALLABLE_NAME_PROP_TYPE =  String.class;

		boolean PROCEDURE_CALLABLE_NAME_PROP_MULTIPLE = false;

		/**
		 * PROCEDURE_NAME Property
		 */
		String PROCEDURE_NAME_PROP_NAME = Namespace.PREFIX + COLON + "procedureName";

		Class<?> PROCEDURE_NAME_PROP_TYPE =  String.class;

		boolean PROCEDURE_NAME_PROP_MULTIPLE = false;

		/**
		 * DISPLAY_NAMED_PARAMETERS Property
		 */
		String DISPLAY_NAMED_PARAMETERS_PROP_NAME = Namespace.PREFIX + COLON + "displayNamedParameters";

		Class<?> DISPLAY_NAMED_PARAMETERS_PROP_TYPE =  Boolean.class;

		boolean DISPLAY_NAMED_PARAMETERS_PROP_MULTIPLE = false;

		/**
		 * CALLED_WITH_RETURN Property
		 */
		String CALLED_WITH_RETURN_PROP_NAME = Namespace.PREFIX + COLON + "calledWithReturn";

		Class<?> CALLED_WITH_RETURN_PROP_TYPE =  Boolean.class;

		boolean CALLED_WITH_RETURN_PROP_MULTIPLE = false;

		/**
		 * PARAMETERS Reference
		 */
		String PARAMETERS_REF_NAME = Namespace.PREFIX + COLON + "parameters";

		Class<?> PARAMETERS_REF_TYPE =  SPParameter.class;

		boolean PARAMETERS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:update
	 */
	public interface Update extends ProcedureContainer, TargetedCommand {

		String ID = Namespace.PREFIX + COLON + "update";

		boolean IS_ABSTRACT = false;

		/**
		 * CRITERIA Reference
		 */
		String CRITERIA_REF_NAME = Namespace.PREFIX + COLON + "criteria";

		Class<?> CRITERIA_REF_TYPE =  Criteria.class;

		boolean CRITERIA_REF_MULTIPLE = false;

		/**
		 * CHANGE_LIST Reference
		 */
		String CHANGE_LIST_REF_NAME = Namespace.PREFIX + COLON + "changeList";

		Class<?> CHANGE_LIST_REF_TYPE =  SetClauseList.class;

		boolean CHANGE_LIST_REF_MULTIPLE = false;

	}

	/**
	 * tsql:dynamicCommand
	 */
	public interface DynamicCommand extends Command {

		String ID = Namespace.PREFIX + COLON + "dynamicCommand";

		boolean IS_ABSTRACT = false;

		/**
		 * UPDATING_MODEL_COUNT Property
		 */
		String UPDATING_MODEL_COUNT_PROP_NAME = Namespace.PREFIX + COLON + "updatingModelCount";

		Class<?> UPDATING_MODEL_COUNT_PROP_TYPE =  Long.class;

		boolean UPDATING_MODEL_COUNT_PROP_MULTIPLE = false;

		/**
		 * AS_CLAUSE_SET Property
		 */
		String AS_CLAUSE_SET_PROP_NAME = Namespace.PREFIX + COLON + "asClauseSet";

		Class<?> AS_CLAUSE_SET_PROP_TYPE =  Boolean.class;

		boolean AS_CLAUSE_SET_PROP_MULTIPLE = false;

		/**
		 * USING Reference
		 */
		String USING_REF_NAME = Namespace.PREFIX + COLON + "using";

		Class<?> USING_REF_TYPE =  SetClauseList.class;

		boolean USING_REF_MULTIPLE = false;

		/**
		 * INTO_GROUP Reference
		 */
		String INTO_GROUP_REF_NAME = Namespace.PREFIX + COLON + "intoGroup";

		Class<?> INTO_GROUP_REF_TYPE =  GroupSymbol.class;

		boolean INTO_GROUP_REF_MULTIPLE = false;

		/**
		 * AS_COLUMNS Reference
		 */
		String AS_COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "asColumns";

		Class<?> AS_COLUMNS_REF_TYPE =  ElementSymbol.class;

		boolean AS_COLUMNS_REF_MULTIPLE = true;

		/**
		 * SQL Reference
		 */
		String SQL_REF_NAME = Namespace.PREFIX + COLON + "sql";

		Class<?> SQL_REF_TYPE =  Expression.class;

		boolean SQL_REF_MULTIPLE = false;

	}

	/**
	 * tsql:queryCommand
	 */
	public interface QueryCommand extends Command {

		String ID = Namespace.PREFIX + COLON + "queryCommand";

		boolean IS_ABSTRACT = true;

		/**
		 * LIMIT Reference
		 */
		String LIMIT_REF_NAME = Namespace.PREFIX + COLON + "limit";

		Class<?> LIMIT_REF_TYPE =  Limit.class;

		boolean LIMIT_REF_MULTIPLE = false;

		/**
		 * ORDER_BY Reference
		 */
		String ORDER_BY_REF_NAME = Namespace.PREFIX + COLON + "orderBy";

		Class<?> ORDER_BY_REF_TYPE =  OrderBy.class;

		boolean ORDER_BY_REF_MULTIPLE = false;

		/**
		 * WITH Reference
		 */
		String WITH_REF_NAME = Namespace.PREFIX + COLON + "with";

		Class<?> WITH_REF_TYPE =  WithQueryCommand.class;

		boolean WITH_REF_MULTIPLE = true;

	}

	/**
	 * tsql:query
	 */
	public interface Query extends QueryCommand {

		String ID = Namespace.PREFIX + COLON + "query";

		boolean IS_ABSTRACT = false;

		/**
		 * ROW_CONSTRUCTOR Property
		 */
		String ROW_CONSTRUCTOR_PROP_NAME = Namespace.PREFIX + COLON + "rowConstructor";

		Class<?> ROW_CONSTRUCTOR_PROP_TYPE =  Boolean.class;

		boolean ROW_CONSTRUCTOR_PROP_MULTIPLE = false;

		/**
		 * SELECT Reference
		 */
		String SELECT_REF_NAME = Namespace.PREFIX + COLON + "select";

		Class<?> SELECT_REF_TYPE =  Select.class;

		boolean SELECT_REF_MULTIPLE = false;

		/**
		 * HAVING Reference
		 */
		String HAVING_REF_NAME = Namespace.PREFIX + COLON + "having";

		Class<?> HAVING_REF_TYPE =  Criteria.class;

		boolean HAVING_REF_MULTIPLE = false;

		/**
		 * FROM Reference
		 */
		String FROM_REF_NAME = Namespace.PREFIX + COLON + "from";

		Class<?> FROM_REF_TYPE =  From.class;

		boolean FROM_REF_MULTIPLE = false;

		/**
		 * INTO Reference
		 */
		String INTO_REF_NAME = Namespace.PREFIX + COLON + "into";

		Class<?> INTO_REF_TYPE =  Into.class;

		boolean INTO_REF_MULTIPLE = false;

		/**
		 * CRITERIA Reference
		 */
		String CRITERIA_REF_NAME = Namespace.PREFIX + COLON + "criteria";

		Class<?> CRITERIA_REF_TYPE =  Criteria.class;

		boolean CRITERIA_REF_MULTIPLE = false;

		/**
		 * GROUP_BY Reference
		 */
		String GROUP_BY_REF_NAME = Namespace.PREFIX + COLON + "groupBy";

		Class<?> GROUP_BY_REF_TYPE =  GroupBy.class;

		boolean GROUP_BY_REF_MULTIPLE = false;

	}

	/**
	 * tsql:setQuery
	 */
	public interface SetQuery extends QueryCommand {

		String ID = Namespace.PREFIX + COLON + "setQuery";

		boolean IS_ABSTRACT = false;

		/**
		 * OPERATION Property
		 */
		String OPERATION_PROP_NAME = Namespace.PREFIX + COLON + "operation";

		Class<?> OPERATION_PROP_TYPE =  String.class;

		boolean OPERATION_PROP_MULTIPLE = false;

		String[] OPERATION_PROP_CONSTRAINTS = { "UNION", "INTERSECT", "EXCEPT" };

		/**
		 * ALL Property
		 */
		String ALL_PROP_NAME = Namespace.PREFIX + COLON + "all";

		Class<?> ALL_PROP_TYPE =  Boolean.class;

		boolean ALL_PROP_MULTIPLE = false;

		/**
		 * RIGHT_QUERY Reference
		 */
		String RIGHT_QUERY_REF_NAME = Namespace.PREFIX + COLON + "rightQuery";

		Class<?> RIGHT_QUERY_REF_TYPE =  QueryCommand.class;

		boolean RIGHT_QUERY_REF_MULTIPLE = false;

		/**
		 * LEFT_QUERY Reference
		 */
		String LEFT_QUERY_REF_NAME = Namespace.PREFIX + COLON + "leftQuery";

		Class<?> LEFT_QUERY_REF_TYPE =  QueryCommand.class;

		boolean LEFT_QUERY_REF_MULTIPLE = false;

	}

	/**
	 * tsql:createProcedureCommand
	 */
	public interface CreateProcedureCommand extends Command {

		String ID = Namespace.PREFIX + COLON + "createProcedureCommand";

		boolean IS_ABSTRACT = false;

		/**
		 * VIRTUAL_GROUP Reference
		 */
		String VIRTUAL_GROUP_REF_NAME = Namespace.PREFIX + COLON + "virtualGroup";

		Class<?> VIRTUAL_GROUP_REF_TYPE =  GroupSymbol.class;

		boolean VIRTUAL_GROUP_REF_MULTIPLE = false;

		/**
		 * BLOCK Reference
		 */
		String BLOCK_REF_NAME = Namespace.PREFIX + COLON + "block";

		Class<?> BLOCK_REF_TYPE =  Block.class;

		boolean BLOCK_REF_MULTIPLE = false;

	}

	/**
	 * tsql:triggerAction
	 */
	public interface TriggerAction extends Command {

		String ID = Namespace.PREFIX + COLON + "triggerAction";

		boolean IS_ABSTRACT = false;

		/**
		 * BLOCK Reference
		 */
		String BLOCK_REF_NAME = Namespace.PREFIX + COLON + "block";

		Class<?> BLOCK_REF_TYPE =  Block.class;

		boolean BLOCK_REF_MULTIPLE = false;

	}

	/**
	 * tsql:fromClause
	 */
	public interface FromClause extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "fromClause";

		boolean IS_ABSTRACT = true;

		/**
		 * MAKE_IND Property
		 */
		String MAKE_IND_PROP_NAME = Namespace.PREFIX + COLON + "makeInd";

		Class<?> MAKE_IND_PROP_TYPE =  Boolean.class;

		boolean MAKE_IND_PROP_MULTIPLE = false;

		/**
		 * NO_UNNEST Property
		 */
		String NO_UNNEST_PROP_NAME = Namespace.PREFIX + COLON + "noUnnest";

		Class<?> NO_UNNEST_PROP_TYPE =  Boolean.class;

		boolean NO_UNNEST_PROP_MULTIPLE = false;

		/**
		 * MAKE_DEP Property
		 */
		String MAKE_DEP_PROP_NAME = Namespace.PREFIX + COLON + "makeDep";

		Class<?> MAKE_DEP_PROP_TYPE =  Boolean.class;

		boolean MAKE_DEP_PROP_MULTIPLE = false;

		/**
		 * PRESERVE Property
		 */
		String PRESERVE_PROP_NAME = Namespace.PREFIX + COLON + "preserve";

		Class<?> PRESERVE_PROP_TYPE =  Boolean.class;

		boolean PRESERVE_PROP_MULTIPLE = false;

		/**
		 * MAKE_NOT_DEP Property
		 */
		String MAKE_NOT_DEP_PROP_NAME = Namespace.PREFIX + COLON + "makeNotDep";

		Class<?> MAKE_NOT_DEP_PROP_TYPE =  Boolean.class;

		boolean MAKE_NOT_DEP_PROP_MULTIPLE = false;

		/**
		 * OPTIONAL Property
		 */
		String OPTIONAL_PROP_NAME = Namespace.PREFIX + COLON + "optional";

		Class<?> OPTIONAL_PROP_TYPE =  Boolean.class;

		boolean OPTIONAL_PROP_MULTIPLE = false;

		/**
		 * MAKE_DEPENDENCY Reference
		 */
		String MAKE_DEPENDENCY_REF_NAME = Namespace.PREFIX + COLON + "makeDependency";

		Class<?> MAKE_DEPENDENCY_REF_TYPE =  MakeDep.class;

		boolean MAKE_DEPENDENCY_REF_MULTIPLE = false;

	}

	/**
	 * tsql:tableFunctionReference
	 */
	public interface TableFunctionReference extends FromClause {

		String ID = Namespace.PREFIX + COLON + "tableFunctionReference";

		boolean IS_ABSTRACT = true;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:arrayTable
	 */
	public interface ArrayTable extends TableFunctionReference {

		String ID = Namespace.PREFIX + COLON + "arrayTable";

		boolean IS_ABSTRACT = false;

		/**
		 * COLUMNS Reference
		 */
		String COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "columns";

		Class<?> COLUMNS_REF_TYPE =  ProjectedColumn.class;

		boolean COLUMNS_REF_MULTIPLE = true;

		/**
		 * ARRAY_VALUE Reference
		 */
		String ARRAY_VALUE_REF_NAME = Namespace.PREFIX + COLON + "arrayValue";

		Class<?> ARRAY_VALUE_REF_TYPE =  Expression.class;

		boolean ARRAY_VALUE_REF_MULTIPLE = false;

	}

	/**
	 * tsql:objectTable
	 */
	public interface ObjectTable extends TableFunctionReference {

		String ID = Namespace.PREFIX + COLON + "objectTable";

		boolean IS_ABSTRACT = false;

		/**
		 * ROW_SCRIPT Property
		 */
		String ROW_SCRIPT_PROP_NAME = Namespace.PREFIX + COLON + "rowScript";

		Class<?> ROW_SCRIPT_PROP_TYPE =  String.class;

		boolean ROW_SCRIPT_PROP_MULTIPLE = false;

		/**
		 * SCRIPTING_LANGUAGE Property
		 */
		String SCRIPTING_LANGUAGE_PROP_NAME = Namespace.PREFIX + COLON + "scriptingLanguage";

		Class<?> SCRIPTING_LANGUAGE_PROP_TYPE =  String.class;

		boolean SCRIPTING_LANGUAGE_PROP_MULTIPLE = false;

		/**
		 * COLUMNS Reference
		 */
		String COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "columns";

		Class<?> COLUMNS_REF_TYPE =  ObjectColumn.class;

		boolean COLUMNS_REF_MULTIPLE = true;

		/**
		 * PASSING Reference
		 */
		String PASSING_REF_NAME = Namespace.PREFIX + COLON + "passing";

		Class<?> PASSING_REF_TYPE =  DerivedColumn.class;

		boolean PASSING_REF_MULTIPLE = true;

	}

	/**
	 * tsql:textTable
	 */
	public interface TextTable extends TableFunctionReference {

		String ID = Namespace.PREFIX + COLON + "textTable";

		boolean IS_ABSTRACT = false;

		/**
		 * QUOTE Property
		 */
		String QUOTE_PROP_NAME = Namespace.PREFIX + COLON + "quote";

		Class<?> QUOTE_PROP_TYPE =  String.class;

		boolean QUOTE_PROP_MULTIPLE = false;

		/**
		 * USING_ROW_DELIMITER Property
		 */
		String USING_ROW_DELIMITER_PROP_NAME = Namespace.PREFIX + COLON + "usingRowDelimiter";

		Class<?> USING_ROW_DELIMITER_PROP_TYPE =  Boolean.class;

		boolean USING_ROW_DELIMITER_PROP_MULTIPLE = false;

		/**
		 * SKIP Property
		 */
		String SKIP_PROP_NAME = Namespace.PREFIX + COLON + "skip";

		Class<?> SKIP_PROP_TYPE =  Long.class;

		boolean SKIP_PROP_MULTIPLE = false;

		/**
		 * DELIMITER Property
		 */
		String DELIMITER_PROP_NAME = Namespace.PREFIX + COLON + "delimiter";

		Class<?> DELIMITER_PROP_TYPE =  String.class;

		boolean DELIMITER_PROP_MULTIPLE = false;

		/**
		 * SELECTOR Property
		 */
		String SELECTOR_PROP_NAME = Namespace.PREFIX + COLON + "selector";

		Class<?> SELECTOR_PROP_TYPE =  String.class;

		boolean SELECTOR_PROP_MULTIPLE = false;

		/**
		 * ESCAPE Property
		 */
		String ESCAPE_PROP_NAME = Namespace.PREFIX + COLON + "escape";

		Class<?> ESCAPE_PROP_TYPE =  Boolean.class;

		boolean ESCAPE_PROP_MULTIPLE = false;

		/**
		 * HEADER Property
		 */
		String HEADER_PROP_NAME = Namespace.PREFIX + COLON + "header";

		Class<?> HEADER_PROP_TYPE =  Long.class;

		boolean HEADER_PROP_MULTIPLE = false;

		/**
		 * FIXED_WIDTH Property
		 */
		String FIXED_WIDTH_PROP_NAME = Namespace.PREFIX + COLON + "fixedWidth";

		Class<?> FIXED_WIDTH_PROP_TYPE =  Boolean.class;

		boolean FIXED_WIDTH_PROP_MULTIPLE = false;

		/**
		 * COLUMNS Reference
		 */
		String COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "columns";

		Class<?> COLUMNS_REF_TYPE =  TextColumn.class;

		boolean COLUMNS_REF_MULTIPLE = true;

		/**
		 * FILE Reference
		 */
		String FILE_REF_NAME = Namespace.PREFIX + COLON + "file";

		Class<?> FILE_REF_TYPE =  Expression.class;

		boolean FILE_REF_MULTIPLE = false;

	}

	/**
	 * tsql:xmlTable
	 */
	public interface XMLTable extends TableFunctionReference {

		String ID = Namespace.PREFIX + COLON + "xmlTable";

		boolean IS_ABSTRACT = false;

		/**
		 * XQUERY Property
		 */
		String XQUERY_PROP_NAME = Namespace.PREFIX + COLON + "xquery";

		Class<?> XQUERY_PROP_TYPE =  String.class;

		boolean XQUERY_PROP_MULTIPLE = false;

		/**
		 * USING_DEFAULT_COLUMN Property
		 */
		String USING_DEFAULT_COLUMN_PROP_NAME = Namespace.PREFIX + COLON + "usingDefaultColumn";

		Class<?> USING_DEFAULT_COLUMN_PROP_TYPE =  Boolean.class;

		boolean USING_DEFAULT_COLUMN_PROP_MULTIPLE = false;

		/**
		 * COLUMNS Reference
		 */
		String COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "columns";

		Class<?> COLUMNS_REF_TYPE =  XMLColumn.class;

		boolean COLUMNS_REF_MULTIPLE = true;

		/**
		 * NAMESPACES Reference
		 */
		String NAMESPACES_REF_NAME = Namespace.PREFIX + COLON + "namespaces";

		Class<?> NAMESPACES_REF_TYPE =  XMLNamespaces.class;

		boolean NAMESPACES_REF_MULTIPLE = false;

		/**
		 * PASSING Reference
		 */
		String PASSING_REF_NAME = Namespace.PREFIX + COLON + "passing";

		Class<?> PASSING_REF_TYPE =  DerivedColumn.class;

		boolean PASSING_REF_MULTIPLE = true;

	}

	/**
	 * tsql:joinPredicate
	 */
	public interface JoinPredicate extends FromClause {

		String ID = Namespace.PREFIX + COLON + "joinPredicate";

		boolean IS_ABSTRACT = false;

		/**
		 * JOIN_TYPE Reference
		 */
		String JOIN_TYPE_REF_NAME = Namespace.PREFIX + COLON + "joinType";

		Class<?> JOIN_TYPE_REF_TYPE =  JoinType.class;

		boolean JOIN_TYPE_REF_MULTIPLE = false;

		/**
		 * RIGHT_CLAUSE Reference
		 */
		String RIGHT_CLAUSE_REF_NAME = Namespace.PREFIX + COLON + "rightClause";

		Class<?> RIGHT_CLAUSE_REF_TYPE =  FromClause.class;

		boolean RIGHT_CLAUSE_REF_MULTIPLE = false;

		/**
		 * JOIN_CRITERIA Reference
		 */
		String JOIN_CRITERIA_REF_NAME = Namespace.PREFIX + COLON + "joinCriteria";

		Class<?> JOIN_CRITERIA_REF_TYPE =  Criteria.class;

		boolean JOIN_CRITERIA_REF_MULTIPLE = true;

		/**
		 * LEFT_CLAUSE Reference
		 */
		String LEFT_CLAUSE_REF_NAME = Namespace.PREFIX + COLON + "leftClause";

		Class<?> LEFT_CLAUSE_REF_TYPE =  FromClause.class;

		boolean LEFT_CLAUSE_REF_MULTIPLE = false;

	}

	/**
	 * tsql:subqueryFromClause
	 */
	public interface SubqueryFromClause extends FromClause, SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "subqueryFromClause";

		boolean IS_ABSTRACT = false;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * TABLE Property
		 */
		String TABLE_PROP_NAME = Namespace.PREFIX + COLON + "table";

		Class<?> TABLE_PROP_TYPE =  Boolean.class;

		boolean TABLE_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:unaryFromClause
	 */
	public interface UnaryFromClause extends FromClause {

		String ID = Namespace.PREFIX + COLON + "unaryFromClause";

		boolean IS_ABSTRACT = false;

		/**
		 * GROUP Reference
		 */
		String GROUP_REF_NAME = Namespace.PREFIX + COLON + "group";

		Class<?> GROUP_REF_TYPE =  GroupSymbol.class;

		boolean GROUP_REF_MULTIPLE = false;

		/**
		 * EXPANDED_COMMAND Reference
		 */
		String EXPANDED_COMMAND_REF_NAME = Namespace.PREFIX + COLON + "expandedCommand";

		Class<?> EXPANDED_COMMAND_REF_TYPE =  Command.class;

		boolean EXPANDED_COMMAND_REF_MULTIPLE = false;

	}

	/**
	 * tsql:from
	 */
	public interface From extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "from";

		boolean IS_ABSTRACT = false;

		/**
		 * CLAUSES Reference
		 */
		String CLAUSES_REF_NAME = Namespace.PREFIX + COLON + "clauses";

		Class<?> CLAUSES_REF_TYPE =  FromClause.class;

		boolean CLAUSES_REF_MULTIPLE = true;

	}

	/**
	 * tsql:groupBy
	 */
	public interface GroupBy extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "groupBy";

		boolean IS_ABSTRACT = false;

		/**
		 * ROLLUP Property
		 */
		String ROLLUP_PROP_NAME = Namespace.PREFIX + COLON + "rollup";

		Class<?> ROLLUP_PROP_TYPE =  Boolean.class;

		boolean ROLLUP_PROP_MULTIPLE = false;

		Version ROLLUP_PROP_SINCE_VERSION = Version.TEIID_8_5;

		/**
		 * SYMBOLS Reference
		 */
		String SYMBOLS_REF_NAME = Namespace.PREFIX + COLON + "symbols";

		Class<?> SYMBOLS_REF_TYPE =  Expression.class;

		boolean SYMBOLS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:into
	 */
	public interface Into extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "into";

		boolean IS_ABSTRACT = false;

		/**
		 * GROUP Reference
		 */
		String GROUP_REF_NAME = Namespace.PREFIX + COLON + "group";

		Class<?> GROUP_REF_TYPE =  GroupSymbol.class;

		boolean GROUP_REF_MULTIPLE = false;

	}

	/**
	 * tsql:joinType
	 */
	public interface JoinType extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "joinType";

		boolean IS_ABSTRACT = false;

		/**
		 * KIND Property
		 */
		String KIND_PROP_NAME = Namespace.PREFIX + COLON + "kind";

		Class<?> KIND_PROP_TYPE =  String.class;

		boolean KIND_PROP_MULTIPLE = false;

		String[] KIND_PROP_CONSTRAINTS = { "JOIN_INNER", "JOIN_RIGHT_OUTER", "JOIN_LEFT_OUTER", "JOIN_FULL_OUTER", "JOIN_CROSS", "JOIN_UNION", "JOIN_SEMI", "JOIN_ANTI_SEMI" };

	}

	/**
	 * tsql:limit
	 */
	public interface Limit extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "limit";

		boolean IS_ABSTRACT = false;

		/**
		 * Tokens and Constants
		 */
		String NON_STRICT = "NON_STRICT";

		/**
		 * IMPLICIT Property
		 */
		String IMPLICIT_PROP_NAME = Namespace.PREFIX + COLON + "implicit";

		Class<?> IMPLICIT_PROP_TYPE =  Boolean.class;

		boolean IMPLICIT_PROP_MULTIPLE = false;

		/**
		 * STRICT Property
		 */
		String STRICT_PROP_NAME = Namespace.PREFIX + COLON + "strict";

		Class<?> STRICT_PROP_TYPE =  Boolean.class;

		boolean STRICT_PROP_MULTIPLE = false;

		/**
		 * OFFSET Reference
		 */
		String OFFSET_REF_NAME = Namespace.PREFIX + COLON + "offset";

		Class<?> OFFSET_REF_TYPE =  Expression.class;

		boolean OFFSET_REF_MULTIPLE = false;

		/**
		 * ROW_LIMIT Reference
		 */
		String ROW_LIMIT_REF_NAME = Namespace.PREFIX + COLON + "rowLimit";

		Class<?> ROW_LIMIT_REF_TYPE =  Expression.class;

		boolean ROW_LIMIT_REF_MULTIPLE = false;

	}

	/**
	 * tsql:makeDep
	 */
	public interface MakeDep extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "makeDep";

		boolean IS_ABSTRACT = false;

		/**
		 * JOIN Property
		 */
		String JOIN_PROP_NAME = Namespace.PREFIX + COLON + "join";

		Class<?> JOIN_PROP_TYPE =  Boolean.class;

		boolean JOIN_PROP_MULTIPLE = false;

		/**
		 * MAX Property
		 */
		String MAX_PROP_NAME = Namespace.PREFIX + COLON + "max";

		Class<?> MAX_PROP_TYPE =  Long.class;

		boolean MAX_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:namespaceItem
	 */
	public interface NamespaceItem extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "namespaceItem";

		boolean IS_ABSTRACT = false;

		/**
		 * PREFIX Property
		 */
		String PREFIX_PROP_NAME = Namespace.PREFIX + COLON + "prefix";

		Class<?> PREFIX_PROP_TYPE =  String.class;

		boolean PREFIX_PROP_MULTIPLE = false;

		/**
		 * URI Property
		 */
		String URI_PROP_NAME = Namespace.PREFIX + COLON + "uri";

		Class<?> URI_PROP_TYPE =  String.class;

		boolean URI_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:nullNode
	 */
	public interface NullNode extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "nullNode";

		boolean IS_ABSTRACT = false;

	}

	/**
	 * tsql:projectedColumn
	 */
	public interface ProjectedColumn extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "projectedColumn";

		boolean IS_ABSTRACT = false;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * TYPE Property
		 */
		String TYPE_PROP_NAME = Namespace.PREFIX + COLON + "type";

		Class<?> TYPE_PROP_TYPE =  String.class;

		boolean TYPE_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:objectColumn
	 */
	public interface ObjectColumn extends ProjectedColumn {

		String ID = Namespace.PREFIX + COLON + "objectColumn";

		boolean IS_ABSTRACT = false;

		/**
		 * PATH Property
		 */
		String PATH_PROP_NAME = Namespace.PREFIX + COLON + "path";

		Class<?> PATH_PROP_TYPE =  String.class;

		boolean PATH_PROP_MULTIPLE = false;

		/**
		 * DEFAULT_EXPRESSION Reference
		 */
		String DEFAULT_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "defaultExpression";

		Class<?> DEFAULT_EXPRESSION_REF_TYPE =  Expression.class;

		boolean DEFAULT_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:textColumn
	 */
	public interface TextColumn extends ProjectedColumn {

		String ID = Namespace.PREFIX + COLON + "textColumn";

		boolean IS_ABSTRACT = false;

		/**
		 * NO_TRIM Property
		 */
		String NO_TRIM_PROP_NAME = Namespace.PREFIX + COLON + "noTrim";

		Class<?> NO_TRIM_PROP_TYPE =  Boolean.class;

		boolean NO_TRIM_PROP_MULTIPLE = false;

		/**
		 * ORDINAL Property
		 */
		String ORDINAL_PROP_NAME = Namespace.PREFIX + COLON + "ordinal";

		Class<?> ORDINAL_PROP_TYPE =  Boolean.class;

		boolean ORDINAL_PROP_MULTIPLE = false;

		Version ORDINAL_PROP_SINCE_VERSION = Version.TEIID_8_7;

		/**
		 * SELECTOR Property
		 */
		String SELECTOR_PROP_NAME = Namespace.PREFIX + COLON + "selector";

		Class<?> SELECTOR_PROP_TYPE =  String.class;

		boolean SELECTOR_PROP_MULTIPLE = false;

		/**
		 * WIDTH Property
		 */
		String WIDTH_PROP_NAME = Namespace.PREFIX + COLON + "width";

		Class<?> WIDTH_PROP_TYPE =  Long.class;

		boolean WIDTH_PROP_MULTIPLE = false;

		/**
		 * POSITION Property
		 */
		String POSITION_PROP_NAME = Namespace.PREFIX + COLON + "position";

		Class<?> POSITION_PROP_TYPE =  Long.class;

		boolean POSITION_PROP_MULTIPLE = false;

		/**
		 * HEADER Property
		 */
		String HEADER_PROP_NAME = Namespace.PREFIX + COLON + "header";

		Class<?> HEADER_PROP_TYPE =  String.class;

		boolean HEADER_PROP_MULTIPLE = false;
	}

	/**
	 * tsql:xmlColumn
	 */
	public interface XMLColumn extends ProjectedColumn {

		String ID = Namespace.PREFIX + COLON + "xmlColumn";

		boolean IS_ABSTRACT = false;

		/**
		 * ORDINAL Property
		 */
		String ORDINAL_PROP_NAME = Namespace.PREFIX + COLON + "ordinal";

		Class<?> ORDINAL_PROP_TYPE =  Boolean.class;

		boolean ORDINAL_PROP_MULTIPLE = false;

		/**
		 * PATH Property
		 */
		String PATH_PROP_NAME = Namespace.PREFIX + COLON + "path";

		Class<?> PATH_PROP_TYPE =  String.class;

		boolean PATH_PROP_MULTIPLE = false;

		/**
		 * DEFAULT_EXPRESSION Reference
		 */
		String DEFAULT_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "defaultExpression";

		Class<?> DEFAULT_EXPRESSION_REF_TYPE =  Expression.class;

		boolean DEFAULT_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:option
	 */
	public interface Option extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "option";

		boolean IS_ABSTRACT = false;

		/**
		 * NO_CACHE_GROUPS Property
		 */
		String NO_CACHE_GROUPS_PROP_NAME = Namespace.PREFIX + COLON + "noCacheGroups";

		Class<?> NO_CACHE_GROUPS_PROP_TYPE =  String.class;

		boolean NO_CACHE_GROUPS_PROP_MULTIPLE = true;

		/**
		 * DEPENDENT_GROUPS Property
		 */
		String DEPENDENT_GROUPS_PROP_NAME = Namespace.PREFIX + COLON + "dependentGroups";

		Class<?> DEPENDENT_GROUPS_PROP_TYPE =  String.class;

		boolean DEPENDENT_GROUPS_PROP_MULTIPLE = true;

		/**
		 * NOT_DEPENDENT_GROUPS Property
		 */
		String NOT_DEPENDENT_GROUPS_PROP_NAME = Namespace.PREFIX + COLON + "notDependentGroups";

		Class<?> NOT_DEPENDENT_GROUPS_PROP_TYPE =  String.class;

		boolean NOT_DEPENDENT_GROUPS_PROP_MULTIPLE = true;

		/**
		 * NO_CACHE Property
		 */
		String NO_CACHE_PROP_NAME = Namespace.PREFIX + COLON + "noCache";

		Class<?> NO_CACHE_PROP_TYPE =  Boolean.class;

		boolean NO_CACHE_PROP_MULTIPLE = false;

		/**
		 * DEPENDENT_GROUP_OPTIONS Reference
		 */
		String DEPENDENT_GROUP_OPTIONS_REF_NAME = Namespace.PREFIX + COLON + "dependentGroupOptions";

		Class<?> DEPENDENT_GROUP_OPTIONS_REF_TYPE =  MakeDep.class;

		boolean DEPENDENT_GROUP_OPTIONS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:orderBy
	 */
	public interface OrderBy extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "orderBy";

		boolean IS_ABSTRACT = false;

		/**
		 * ORDER_BY_ITEMS Reference
		 */
		String ORDER_BY_ITEMS_REF_NAME = Namespace.PREFIX + COLON + "orderByItems";

		Class<?> ORDER_BY_ITEMS_REF_TYPE =  OrderByItem.class;

		boolean ORDER_BY_ITEMS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:orderByItem
	 */
	public interface OrderByItem extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "orderByItem";

		boolean IS_ABSTRACT = false;

		/**
		 * NULL_ORDERING Property
		 */
		String NULL_ORDERING_PROP_NAME = Namespace.PREFIX + COLON + "nullOrdering";

		Class<?> NULL_ORDERING_PROP_TYPE =  String.class;

		boolean NULL_ORDERING_PROP_MULTIPLE = false;

		String[] NULL_ORDERING_PROP_CONSTRAINTS = { "FIRST", "LAST" };

		/**
		 * ASCENDING Property
		 */
		String ASCENDING_PROP_NAME = Namespace.PREFIX + COLON + "ascending";

		Class<?> ASCENDING_PROP_TYPE =  Boolean.class;

		boolean ASCENDING_PROP_MULTIPLE = false;

		/**
		 * SYMBOL Reference
		 */
		String SYMBOL_REF_NAME = Namespace.PREFIX + COLON + "symbol";

		Class<?> SYMBOL_REF_TYPE =  Expression.class;

		boolean SYMBOL_REF_MULTIPLE = false;

	}

	/**
	 * tsql:spParameter
	 */
	public interface SPParameter extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "spParameter";

		boolean IS_ABSTRACT = false;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * VAR_ARG Property
		 */
		String VAR_ARG_PROP_NAME = Namespace.PREFIX + COLON + "varArg";

		Class<?> VAR_ARG_PROP_TYPE =  Boolean.class;

		boolean VAR_ARG_PROP_MULTIPLE = false;

		/**
		 * INDEX Property
		 */
		String INDEX_PROP_NAME = Namespace.PREFIX + COLON + "index";

		Class<?> INDEX_PROP_TYPE =  Long.class;

		boolean INDEX_PROP_MULTIPLE = false;

		/**
		 * PARAMETER_TYPE Property
		 */
		String PARAMETER_TYPE_PROP_NAME = Namespace.PREFIX + COLON + "parameterType";

		Class<?> PARAMETER_TYPE_PROP_TYPE =  Long.class;

		boolean PARAMETER_TYPE_PROP_MULTIPLE = false;

		/**
		 * USING_DEFAULT Property
		 */
		String USING_DEFAULT_PROP_NAME = Namespace.PREFIX + COLON + "usingDefault";

		Class<?> USING_DEFAULT_PROP_TYPE =  Boolean.class;

		boolean USING_DEFAULT_PROP_MULTIPLE = false;

		/**
		 * METADATAID Property
		 */
		String METADATAID_PROP_NAME = Namespace.PREFIX + COLON + "metadataiD";

		Class<?> METADATAID_PROP_TYPE =  String.class;

		boolean METADATAID_PROP_MULTIPLE = false;

		/**
		 * CLASS_TYPE_CLASS Property
		 */
		String CLASS_TYPE_CLASS_PROP_NAME = Namespace.PREFIX + COLON + "classTypeClass";

		Class<?> CLASS_TYPE_CLASS_PROP_TYPE =  String.class;

		boolean CLASS_TYPE_CLASS_PROP_MULTIPLE = false;

		/**
		 * RESULT_SET_COLUMN Reference
		 */
		String RESULT_SET_COLUMN_REF_NAME = Namespace.PREFIX + COLON + "resultSetColumn";

		Class<?> RESULT_SET_COLUMN_REF_TYPE =  ElementSymbol.class;

		boolean RESULT_SET_COLUMN_REF_MULTIPLE = true;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:select
	 */
	public interface Select extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "select";

		boolean IS_ABSTRACT = false;

		/**
		 * DISTINCT Property
		 */
		String DISTINCT_PROP_NAME = Namespace.PREFIX + COLON + "distinct";

		Class<?> DISTINCT_PROP_TYPE =  Boolean.class;

		boolean DISTINCT_PROP_MULTIPLE = false;

		/**
		 * SYMBOLS Reference
		 */
		String SYMBOLS_REF_NAME = Namespace.PREFIX + COLON + "symbols";

		Class<?> SYMBOLS_REF_TYPE =  Expression.class;

		boolean SYMBOLS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:setClause
	 */
	public interface SetClause extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "setClause";

		boolean IS_ABSTRACT = false;

		/**
		 * VALUE Reference
		 */
		String VALUE_REF_NAME = Namespace.PREFIX + COLON + "value";

		Class<?> VALUE_REF_TYPE =  Expression.class;

		boolean VALUE_REF_MULTIPLE = false;

		/**
		 * SYMBOL Reference
		 */
		String SYMBOL_REF_NAME = Namespace.PREFIX + COLON + "symbol";

		Class<?> SYMBOL_REF_TYPE =  ElementSymbol.class;

		boolean SYMBOL_REF_MULTIPLE = false;

	}

	/**
	 * tsql:setClauseList
	 */
	public interface SetClauseList extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "setClauseList";

		boolean IS_ABSTRACT = false;

		/**
		 * SET_CLAUSES Reference
		 */
		String SET_CLAUSES_REF_NAME = Namespace.PREFIX + COLON + "setClauses";

		Class<?> SET_CLAUSES_REF_TYPE =  SetClause.class;

		boolean SET_CLAUSES_REF_MULTIPLE = true;

	}

    /**
      * tsql:cacheHint
      */
    public interface CacheHint extends LanguageObject {

        String ID = Namespace.PREFIX + COLON + "cacheHint";

        boolean IS_ABSTRACT = false;

        /**
         * MIN_ROWS Property
         */
        String MIN_ROWS_PROP_NAME = Namespace.PREFIX + COLON + "minRows";

        Class<?> MIN_ROWS_PROP_TYPE = Long.class;

        boolean MIN_ROWS_PROP_MULTIPLE = false;

        /**
         * UPDATEABLE Property
         */
        String UPDATEABLE_PROP_NAME = Namespace.PREFIX + COLON + "updateable";

        Class<?> UPDATEABLE_PROP_TYPE = Boolean.class;

        boolean UPDATEABLE_PROP_MULTIPLE = false;

        /**
         * TTL Property
         */
        String TTL_PROP_NAME = Namespace.PREFIX + COLON + "ttl";

        Class<?> TTL_PROP_TYPE = String.class;

        boolean TTL_PROP_MULTIPLE = false;

        /**
         * SCOPE Property
         */
        String SCOPE_PROP_NAME = Namespace.PREFIX + COLON + "scope";

        Class<?> SCOPE_PROP_TYPE = String.class;

        boolean SCOPE_PROP_MULTIPLE = false;

        /**
         * INVALIDATION Property
         */
        String INVALIDATION_PROP_NAME = Namespace.PREFIX + COLON + "invalidation";

        Class<?> INVALIDATION_PROP_TYPE = String.class;

        boolean INVALIDATION_PROP_MULTIPLE = false;

        /**
         * PREFERS_MEMORY Property
         */
        String PREFERS_MEMORY_PROP_NAME = Namespace.PREFIX + COLON + "prefersMemory";

        Class<?> PREFERS_MEMORY_PROP_TYPE = Boolean.class;

        boolean PREFERS_MEMORY_PROP_MULTIPLE = false;

        /**
         * READ_ALL Property
         */
        String READ_ALL_PROP_NAME = Namespace.PREFIX + COLON + "readAll";

        Class<?> READ_ALL_PROP_TYPE = Boolean.class;

        boolean READ_ALL_PROP_MULTIPLE = false;
    }

	/**
	 * tsql:sourceHint
	 */
	public interface SourceHint extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "sourceHint";

		boolean IS_ABSTRACT = false;

		/**
		 * GENERAL_HINT Property
		 */
		String GENERAL_HINT_PROP_NAME = Namespace.PREFIX + COLON + "generalHint";

		Class<?> GENERAL_HINT_PROP_TYPE =  String.class;

		boolean GENERAL_HINT_PROP_MULTIPLE = false;

		/**
		 * USE_ALIASES Property
		 */
		String USE_ALIASES_PROP_NAME = Namespace.PREFIX + COLON + "useAliases";

		Class<?> USE_ALIASES_PROP_TYPE =  Boolean.class;

		boolean USE_ALIASES_PROP_MULTIPLE = false;

		/**
		 * SOURCE_HINTS Reference
		 */
		String SOURCE_HINTS_REF_NAME = Namespace.PREFIX + COLON + "sourceHints";

		Class<?> SOURCE_HINTS_REF_TYPE =  SpecificHint.class;

		boolean SOURCE_HINTS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:specificHint
	 */
	public interface SpecificHint extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "specificHint";

		boolean IS_ABSTRACT = false;

		/**
		 * HINT Property
		 */
		String HINT_PROP_NAME = Namespace.PREFIX + COLON + "hint";

		Class<?> HINT_PROP_TYPE =  String.class;

		boolean HINT_PROP_MULTIPLE = false;

		/**
		 * TRANSLATOR_NAME Property
		 */
		String TRANSLATOR_NAME_PROP_NAME = Namespace.PREFIX + COLON + "translatorName";

		Class<?> TRANSLATOR_NAME_PROP_TYPE =  String.class;

		boolean TRANSLATOR_NAME_PROP_MULTIPLE = false;

		/**
		 * USE_ALIASES Property
		 */
		String USE_ALIASES_PROP_NAME = Namespace.PREFIX + COLON + "useAliases";

		Class<?> USE_ALIASES_PROP_TYPE =  Boolean.class;

		boolean USE_ALIASES_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:subqueryHint
	 */
	public interface SubqueryHint extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "subqueryHint";

		boolean IS_ABSTRACT = false;

        /**
         * Tokens and Constants
         */
        String DJ = "DJ";

        String MJ = "MJ";

		/**
		 * NO_UNNEST Property
		 */
		String NO_UNNEST_PROP_NAME = Namespace.PREFIX + COLON + "noUnnest";

		Class<?> NO_UNNEST_PROP_TYPE =  Boolean.class;

		boolean NO_UNNEST_PROP_MULTIPLE = false;

		/**
		 * DEP_JOIN Property
		 */
		String DEP_JOIN_PROP_NAME = Namespace.PREFIX + COLON + "depJoin";

		Class<?> DEP_JOIN_PROP_TYPE =  Boolean.class;

		boolean DEP_JOIN_PROP_MULTIPLE = false;

		/**
		 * MERGE_JOIN Property
		 */
		String MERGE_JOIN_PROP_NAME = Namespace.PREFIX + COLON + "mergeJoin";

		Class<?> MERGE_JOIN_PROP_TYPE =  Boolean.class;

		boolean MERGE_JOIN_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:withQueryCommand
	 */
	public interface WithQueryCommand extends SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "withQueryCommand";

		boolean IS_ABSTRACT = false;

		/**
		 * COLUMNS Reference
		 */
		String COLUMNS_REF_NAME = Namespace.PREFIX + COLON + "columns";

		Class<?> COLUMNS_REF_TYPE =  ElementSymbol.class;

		boolean COLUMNS_REF_MULTIPLE = true;

		/**
		 * GROUP_SYMBOL Reference
		 */
		String GROUP_SYMBOL_REF_NAME = Namespace.PREFIX + COLON + "groupSymbol";

		Class<?> GROUP_SYMBOL_REF_TYPE =  GroupSymbol.class;

		boolean GROUP_SYMBOL_REF_MULTIPLE = false;

		/**
		 * QUERY_EXPRESSION Reference
		 */
		String QUERY_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "queryExpression";

		Class<?> QUERY_EXPRESSION_REF_TYPE =  QueryCommand.class;

		boolean QUERY_EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:statement
	 */
	public interface Statement extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "statement";

		boolean IS_ABSTRACT = true;

		/**
		 * TYPE Property
		 */
		String TYPE_PROP_NAME = Namespace.PREFIX + COLON + "type";

		Class<?> TYPE_PROP_TYPE =  String.class;

		boolean TYPE_PROP_MULTIPLE = false;

		String[] TYPE_PROP_CONSTRAINTS = { "TYPE_UNKNOWN", "TYPE_IF", "TYPE_COMMAND", "TYPE_DECLARE", "TYPE_ERROR", "TYPE_ASSIGNMENT", "TYPE_LOOP", "TYPE_WHILE", "TYPE_CONTINUE", "TYPE_BREAK", "TYPE_UPDATE", "TYPE_COMPOUND", "TYPE_LEAVE", "TYPE_RETURN" };

	}

	/**
	 * tsql:assignmentStatement
	 */
	public interface AssignmentStatement extends Statement, ExpressionStatement {

		String ID = Namespace.PREFIX + COLON + "assignmentStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * VALUE Reference
		 */
		String VALUE_REF_NAME = Namespace.PREFIX + COLON + "value";

		Class<?> VALUE_REF_TYPE =  Expression.class;

		boolean VALUE_REF_MULTIPLE = false;

		/**
		 * VARIABLE Reference
		 */
		String VARIABLE_REF_NAME = Namespace.PREFIX + COLON + "variable";

		Class<?> VARIABLE_REF_TYPE =  ElementSymbol.class;

		boolean VARIABLE_REF_MULTIPLE = false;

		/**
		 * COMMAND Reference
		 */
		String COMMAND_REF_NAME = Namespace.PREFIX + COLON + "command";

		Class<?> COMMAND_REF_TYPE =  Command.class;

		boolean COMMAND_REF_MULTIPLE = false;

	}

	/**
	 * tsql:declareStatement
	 */
	public interface DeclareStatement extends AssignmentStatement {

		String ID = Namespace.PREFIX + COLON + "declareStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * VARIABLE_TYPE Property
		 */
		String VARIABLE_TYPE_PROP_NAME = Namespace.PREFIX + COLON + "variableType";

		Class<?> VARIABLE_TYPE_PROP_TYPE =  String.class;

		boolean VARIABLE_TYPE_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:returnStatement
	 */
	public interface ReturnStatement extends AssignmentStatement {

		String ID = Namespace.PREFIX + COLON + "returnStatement";

		boolean IS_ABSTRACT = false;

	}

	/**
	 * tsql:block
	 */
	public interface Block extends Statement, Labeled {

		String ID = Namespace.PREFIX + COLON + "block";

		boolean IS_ABSTRACT = false;

		/**
		 * ATOMIC Property
		 */
		String ATOMIC_PROP_NAME = Namespace.PREFIX + COLON + "atomic";

		Class<?> ATOMIC_PROP_TYPE =  Boolean.class;

		boolean ATOMIC_PROP_MULTIPLE = false;

		/**
		 * EXCEPTION_GROUP Property
		 */
		String EXCEPTION_GROUP_PROP_NAME = Namespace.PREFIX + COLON + "exceptionGroup";

		Class<?> EXCEPTION_GROUP_PROP_TYPE =  String.class;

		boolean EXCEPTION_GROUP_PROP_MULTIPLE = false;

		/**
		 * EXCEPTION_STATEMENTS Reference
		 */
		String EXCEPTION_STATEMENTS_REF_NAME = Namespace.PREFIX + COLON + "exceptionStatements";

		Class<?> EXCEPTION_STATEMENTS_REF_TYPE =  Statement.class;

		boolean EXCEPTION_STATEMENTS_REF_MULTIPLE = true;

		/**
		 * STATEMENTS Reference
		 */
		String STATEMENTS_REF_NAME = Namespace.PREFIX + COLON + "statements";

		Class<?> STATEMENTS_REF_TYPE =  Statement.class;

		boolean STATEMENTS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:branchingStatement
	 */
	public interface BranchingStatement extends Statement {

		String ID = Namespace.PREFIX + COLON + "branchingStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * MODE Property
		 */
		String MODE_PROP_NAME = Namespace.PREFIX + COLON + "mode";

		Class<?> MODE_PROP_TYPE =  String.class;

		boolean MODE_PROP_MULTIPLE = false;

		String[] MODE_PROP_CONSTRAINTS = { "BREAK", "CONTINUE", "LEAVE" };

		/**
		 * LABEL Property
		 */
		String LABEL_PROP_NAME = Namespace.PREFIX + COLON + "label";

		Class<?> LABEL_PROP_TYPE =  String.class;

		boolean LABEL_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:commandStatement
	 */
	public interface CommandStatement extends Statement, SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "commandStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * RETURNABLE Property
		 */
		String RETURNABLE_PROP_NAME = Namespace.PREFIX + COLON + "returnable";

		Class<?> RETURNABLE_PROP_TYPE =  Boolean.class;

		boolean RETURNABLE_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:ifStatement
	 */
	public interface IfStatement extends Statement {

		String ID = Namespace.PREFIX + COLON + "ifStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * IF_BLOCK Reference
		 */
		String IF_BLOCK_REF_NAME = Namespace.PREFIX + COLON + "ifBlock";

		Class<?> IF_BLOCK_REF_TYPE =  Block.class;

		boolean IF_BLOCK_REF_MULTIPLE = false;

		/**
		 * CONDITION Reference
		 */
		String CONDITION_REF_NAME = Namespace.PREFIX + COLON + "condition";

		Class<?> CONDITION_REF_TYPE =  Criteria.class;

		boolean CONDITION_REF_MULTIPLE = false;

		/**
		 * ELSE_BLOCK Reference
		 */
		String ELSE_BLOCK_REF_NAME = Namespace.PREFIX + COLON + "elseBlock";

		Class<?> ELSE_BLOCK_REF_TYPE =  Block.class;

		boolean ELSE_BLOCK_REF_MULTIPLE = false;

	}

	/**
	 * tsql:loopStatement
	 */
	public interface LoopStatement extends Statement, Labeled, SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "loopStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * CURSOR_NAME Property
		 */
		String CURSOR_NAME_PROP_NAME = Namespace.PREFIX + COLON + "cursorName";

		Class<?> CURSOR_NAME_PROP_TYPE =  String.class;

		boolean CURSOR_NAME_PROP_MULTIPLE = false;

		/**
		 * BLOCK Reference
		 */
		String BLOCK_REF_NAME = Namespace.PREFIX + COLON + "block";

		Class<?> BLOCK_REF_TYPE =  Block.class;

		boolean BLOCK_REF_MULTIPLE = false;

	}

	/**
	 * tsql:raiseStatement
	 */
	public interface RaiseStatement extends Statement, ExpressionStatement {

		String ID = Namespace.PREFIX + COLON + "raiseStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * WARNING Property
		 */
		String WARNING_PROP_NAME = Namespace.PREFIX + COLON + "warning";

		Class<?> WARNING_PROP_TYPE =  Boolean.class;

		boolean WARNING_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:whileStatement
	 */
	public interface WhileStatement extends Statement, Labeled {

		String ID = Namespace.PREFIX + COLON + "whileStatement";

		boolean IS_ABSTRACT = false;

		/**
		 * CONDITION Reference
		 */
		String CONDITION_REF_NAME = Namespace.PREFIX + COLON + "condition";

		Class<?> CONDITION_REF_TYPE =  Criteria.class;

		boolean CONDITION_REF_MULTIPLE = false;

		/**
		 * BLOCK Reference
		 */
		String BLOCK_REF_NAME = Namespace.PREFIX + COLON + "block";

		Class<?> BLOCK_REF_TYPE =  Block.class;

		boolean BLOCK_REF_MULTIPLE = false;

	}

	/**
	 * tsql:exceptionExpression
	 */
	public interface ExceptionExpression extends Expression {

		String ID = Namespace.PREFIX + COLON + "exceptionExpression";

		boolean IS_ABSTRACT = false;

		/**
		 * PARENT_EXPRESSION Reference
		 */
		String PARENT_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "parentExpression";

		Class<?> PARENT_EXPRESSION_REF_TYPE =  Expression.class;

		boolean PARENT_EXPRESSION_REF_MULTIPLE = false;

		/**
		 * SQL_STATE Reference
		 */
		String SQL_STATE_REF_NAME = Namespace.PREFIX + COLON + "sqlState";

		Class<?> SQL_STATE_REF_TYPE =  Expression.class;

		boolean SQL_STATE_REF_MULTIPLE = false;

		/**
		 * MESSAGE Reference
		 */
		String MESSAGE_REF_NAME = Namespace.PREFIX + COLON + "message";

		Class<?> MESSAGE_REF_TYPE =  Expression.class;

		boolean MESSAGE_REF_MULTIPLE = false;

		/**
		 * ERROR_CODE Reference
		 */
		String ERROR_CODE_REF_NAME = Namespace.PREFIX + COLON + "errorCode";

		Class<?> ERROR_CODE_REF_TYPE =  Expression.class;

		boolean ERROR_CODE_REF_MULTIPLE = false;

	}

	/**
	 * tsql:function
	 */
	public interface Function extends Expression {

		String ID = Namespace.PREFIX + COLON + "function";

		boolean IS_ABSTRACT = false;

		/**
		 * IMPLICIT Property
		 */
		String IMPLICIT_PROP_NAME = Namespace.PREFIX + COLON + "implicit";

		Class<?> IMPLICIT_PROP_TYPE =  Boolean.class;

		boolean IMPLICIT_PROP_MULTIPLE = false;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * ARRAY_TYPE Property
		 */
		String ARRAY_TYPE_PROP_NAME = Namespace.PREFIX + COLON + "arrayType";

		Class<?> ARRAY_TYPE_PROP_TYPE =  Boolean.class;

		boolean ARRAY_TYPE_PROP_MULTIPLE = false;

		/**
		 * ARGS Reference
		 */
		String ARGS_REF_NAME = Namespace.PREFIX + COLON + "args";

		Class<?> ARGS_REF_TYPE =  Expression.class;

		boolean ARGS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:aggregateSymbol
	 */
	public interface AggregateSymbol extends Function {

		String ID = Namespace.PREFIX + COLON + "aggregateSymbol";

		boolean IS_ABSTRACT = false;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * AGGREGATE_FUNCTION Property
		 */
		String AGGREGATE_FUNCTION_PROP_NAME = Namespace.PREFIX + COLON + "aggregateFunction";

		Class<?> AGGREGATE_FUNCTION_PROP_TYPE =  String.class;

		boolean AGGREGATE_FUNCTION_PROP_MULTIPLE = false;

		/**
		 * WINDOWED Property
		 */
		String WINDOWED_PROP_NAME = Namespace.PREFIX + COLON + "windowed";

		Class<?> WINDOWED_PROP_TYPE =  Boolean.class;

		boolean WINDOWED_PROP_MULTIPLE = false;

		/**
		 * DISTINCT Property
		 */
		String DISTINCT_PROP_NAME = Namespace.PREFIX + COLON + "distinct";

		Class<?> DISTINCT_PROP_TYPE =  Boolean.class;

		boolean DISTINCT_PROP_MULTIPLE = false;

		/**
		 * CONDITION Reference
		 */
		String CONDITION_REF_NAME = Namespace.PREFIX + COLON + "condition";

		Class<?> CONDITION_REF_TYPE =  Expression.class;

		boolean CONDITION_REF_MULTIPLE = false;

		/**
		 * ARGS Reference
		 */
		String ARGS_REF_NAME = Namespace.PREFIX + COLON + "args";

		Class<?> ARGS_REF_TYPE =  Expression.class;

		boolean ARGS_REF_MULTIPLE = true;

		/**
		 * ORDER_BY Reference
		 */
		String ORDER_BY_REF_NAME = Namespace.PREFIX + COLON + "orderBy";

		Class<?> ORDER_BY_REF_TYPE =  OrderBy.class;

		boolean ORDER_BY_REF_MULTIPLE = false;

	}

	/**
	 * tsql:symbol
	 */
	public interface Symbol extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "symbol";

		boolean IS_ABSTRACT = true;

		/**
		 * Tokens and Constants
		 */

		/**
		 * Character used to delimit name components in a symbol
		 */
		String SEPARATOR = DOT;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * SHORT_NAME Property
		 */
		String SHORT_NAME_PROP_NAME = Namespace.PREFIX + COLON + "shortName";

		Class<?> SHORT_NAME_PROP_TYPE =  String.class;

		boolean SHORT_NAME_PROP_MULTIPLE = false;

		/**
		 * OUTPUT_NAME Property
		 */
		String OUTPUT_NAME_PROP_NAME = Namespace.PREFIX + COLON + "outputName";

		Class<?> OUTPUT_NAME_PROP_TYPE =  String.class;

		boolean OUTPUT_NAME_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:aliasSymbol
	 */
	public interface AliasSymbol extends Symbol, Expression {

		String ID = Namespace.PREFIX + COLON + "aliasSymbol";

		boolean IS_ABSTRACT = false;

		/**
		 * SYMBOL Reference
		 */
		String SYMBOL_REF_NAME = Namespace.PREFIX + COLON + "symbol";

		Class<?> SYMBOL_REF_TYPE =  Expression.class;

		boolean SYMBOL_REF_MULTIPLE = false;

	}

	/**
	 * tsql:elementSymbol
	 */
	public interface ElementSymbol extends Symbol, Expression {

		String ID = Namespace.PREFIX + COLON + "elementSymbol";

		boolean IS_ABSTRACT = false;

		/**
		 * DISPLAY_MODE Property
		 */
		String DISPLAY_MODE_PROP_NAME = Namespace.PREFIX + COLON + "displayMode";

		Class<?> DISPLAY_MODE_PROP_TYPE =  String.class;

		boolean DISPLAY_MODE_PROP_MULTIPLE = false;

		String[] DISPLAY_MODE_PROP_CONSTRAINTS = { "FULLY_QUALIFIED", "OUTPUT_NAME", "SHORT_OUTPUT_NAME" };

		/**
		 * EXTERNAL_REFERENCE Property
		 */
		String EXTERNAL_REFERENCE_PROP_NAME = Namespace.PREFIX + COLON + "externalReference";

		Class<?> EXTERNAL_REFERENCE_PROP_TYPE =  Boolean.class;

		boolean EXTERNAL_REFERENCE_PROP_MULTIPLE = false;

		/**
		 * DISPLAY_FULLY_QUALIFIED Property
		 */
		String DISPLAY_FULLY_QUALIFIED_PROP_NAME = Namespace.PREFIX + COLON + "displayFullyQualified";

		Class<?> DISPLAY_FULLY_QUALIFIED_PROP_TYPE =  Boolean.class;

		boolean DISPLAY_FULLY_QUALIFIED_PROP_MULTIPLE = false;

		/**
		 * METADATAID Property
		 */
		String METADATAID_PROP_NAME = Namespace.PREFIX + COLON + "metadataiD";

		Class<?> METADATAID_PROP_TYPE =  String.class;

		boolean METADATAID_PROP_MULTIPLE = false;

		/**
		 * GROUP_SYMBOL Reference
		 */
		String GROUP_SYMBOL_REF_NAME = Namespace.PREFIX + COLON + "groupSymbol";

		Class<?> GROUP_SYMBOL_REF_TYPE =  GroupSymbol.class;

		boolean GROUP_SYMBOL_REF_MULTIPLE = false;

	}

	/**
	 * tsql:expressionSymbol
	 */
	public interface ExpressionSymbol extends Symbol, Expression {

		String ID = Namespace.PREFIX + COLON + "expressionSymbol";

		boolean IS_ABSTRACT = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:groupSymbol
	 */
	public interface GroupSymbol extends Symbol {

		String ID = Namespace.PREFIX + COLON + "groupSymbol";

		boolean IS_ABSTRACT = false;

		/**
		 * PROCEDURE Property
		 */
		String PROCEDURE_PROP_NAME = Namespace.PREFIX + COLON + "procedure";

		Class<?> PROCEDURE_PROP_TYPE =  Boolean.class;

		boolean PROCEDURE_PROP_MULTIPLE = false;

		/**
		 * DEFINITION Property
		 */
		String DEFINITION_PROP_NAME = Namespace.PREFIX + COLON + "definition";

		Class<?> DEFINITION_PROP_TYPE =  String.class;

		boolean DEFINITION_PROP_MULTIPLE = false;

		/**
		 * METADATAID Property
		 */
		String METADATAID_PROP_NAME = Namespace.PREFIX + COLON + "metadataiD";

		Class<?> METADATAID_PROP_TYPE =  String.class;

		boolean METADATAID_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:arraySymbol
	 */
	public interface ArraySymbol extends Expression {

		String ID = Namespace.PREFIX + COLON + "arraySymbol";

		boolean IS_ABSTRACT = false;

		/**
		 * IMPLICIT Property
		 */
		String IMPLICIT_PROP_NAME = Namespace.PREFIX + COLON + "implicit";

		Class<?> IMPLICIT_PROP_TYPE =  Boolean.class;

		boolean IMPLICIT_PROP_MULTIPLE = false;

		/**
		 * EXPRESSIONS Reference
		 */
		String EXPRESSIONS_REF_NAME = Namespace.PREFIX + COLON + "expressions";

		Class<?> EXPRESSIONS_REF_TYPE =  Expression.class;

		boolean EXPRESSIONS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:caseExpression
	 */
	public interface CaseExpression extends Expression {

		String ID = Namespace.PREFIX + COLON + "caseExpression";

		boolean IS_ABSTRACT = false;

		/**
		 * ELSE_EXPRESSION Reference
		 */
		String ELSE_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "elseExpression";

		Class<?> ELSE_EXPRESSION_REF_TYPE =  Expression.class;

		boolean ELSE_EXPRESSION_REF_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

		/**
		 * WHEN Reference
		 */
		String WHEN_REF_NAME = Namespace.PREFIX + COLON + "when";

		Class<?> WHEN_REF_TYPE =  Expression.class;

		boolean WHEN_REF_MULTIPLE = true;

		/**
		 * THEN Reference
		 */
		String THEN_REF_NAME = Namespace.PREFIX + COLON + "then";

		Class<?> THEN_REF_TYPE =  Expression.class;

		boolean THEN_REF_MULTIPLE = true;

	}

	/**
	 * tsql:constant
	 */
	public interface Constant extends Expression {

		String ID = Namespace.PREFIX + COLON + "constant";

		boolean IS_ABSTRACT = false;

		/**
		 * MULTI_VALUED Property
		 */
		String MULTI_VALUED_PROP_NAME = Namespace.PREFIX + COLON + "multiValued";

		Class<?> MULTI_VALUED_PROP_TYPE =  Boolean.class;

		boolean MULTI_VALUED_PROP_MULTIPLE = false;

		/**
		 * VALUE Property
		 */
		String VALUE_PROP_NAME = Namespace.PREFIX + COLON + "value";

		Class<?> VALUE_PROP_TYPE =  String.class;

		boolean VALUE_PROP_MULTIPLE = false;

	}

	/**
	 * tsql:derivedColumn
	 */
	public interface DerivedColumn extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "derivedColumn";

		boolean IS_ABSTRACT = false;

		/**
		 * ALIAS Property
		 */
		String ALIAS_PROP_NAME = Namespace.PREFIX + COLON + "alias";

		Class<?> ALIAS_PROP_TYPE =  String.class;

		boolean ALIAS_PROP_MULTIPLE = false;

		/**
		 * PROPAGATE_NAME Property
		 */
		String PROPAGATE_NAME_PROP_NAME = Namespace.PREFIX + COLON + "propagateName";

		Class<?> PROPAGATE_NAME_PROP_TYPE =  Boolean.class;

		boolean PROPAGATE_NAME_PROP_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:jsonObject
	 */
	public interface JSONObject extends Expression {

		String ID = Namespace.PREFIX + COLON + "jsonObject";

		boolean IS_ABSTRACT = false;

		/**
		 * ARGS Reference
		 */
		String ARGS_REF_NAME = Namespace.PREFIX + COLON + "args";

		Class<?> ARGS_REF_TYPE =  DerivedColumn.class;

		boolean ARGS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:multipleElementSymbol
	 */
	public interface MultipleElementSymbol extends Expression {

		String ID = Namespace.PREFIX + COLON + "multipleElementSymbol";

		boolean IS_ABSTRACT = false;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * GROUP Reference
		 */
		String GROUP_REF_NAME = Namespace.PREFIX + COLON + "group";

		Class<?> GROUP_REF_TYPE =  GroupSymbol.class;

		boolean GROUP_REF_MULTIPLE = false;

		/**
		 * ELEMENT_SYMBOLS Reference
		 */
		String ELEMENT_SYMBOLS_REF_NAME = Namespace.PREFIX + COLON + "elementSymbols";

		Class<?> ELEMENT_SYMBOLS_REF_TYPE =  ElementSymbol.class;

		boolean ELEMENT_SYMBOLS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:queryString
	 */
	public interface QueryString extends Expression {

		String ID = Namespace.PREFIX + COLON + "queryString";

		boolean IS_ABSTRACT = false;

		/**
		 * ARGS Reference
		 */
		String ARGS_REF_NAME = Namespace.PREFIX + COLON + "args";

		Class<?> ARGS_REF_TYPE =  DerivedColumn.class;

		boolean ARGS_REF_MULTIPLE = true;

		/**
		 * PATH Reference
		 */
		String PATH_REF_NAME = Namespace.PREFIX + COLON + "path";

		Class<?> PATH_REF_TYPE =  Expression.class;

		boolean PATH_REF_MULTIPLE = false;

	}

	/**
	 * tsql:reference
	 */
	public interface Reference extends Expression {

		String ID = Namespace.PREFIX + COLON + "reference";

		boolean IS_ABSTRACT = false;

		/**
		 * POSITIONAL Property
		 */
		String POSITIONAL_PROP_NAME = Namespace.PREFIX + COLON + "positional";

		Class<?> POSITIONAL_PROP_TYPE =  Boolean.class;

		boolean POSITIONAL_PROP_MULTIPLE = false;

		/**
		 * INDEX Property
		 */
		String INDEX_PROP_NAME = Namespace.PREFIX + COLON + "index";

		Class<?> INDEX_PROP_TYPE =  Long.class;

		boolean INDEX_PROP_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  ElementSymbol.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:scalarSubquery
	 */
	public interface ScalarSubquery extends Expression, SubqueryContainer {

		String ID = Namespace.PREFIX + COLON + "scalarSubquery";

		boolean IS_ABSTRACT = false;

	}

	/**
	 * tsql:searchedCaseExpression
	 */
	public interface SearchedCaseExpression extends Expression {

		String ID = Namespace.PREFIX + COLON + "searchedCaseExpression";

		boolean IS_ABSTRACT = false;

		/**
		 * ELSE_EXPRESSION Reference
		 */
		String ELSE_EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "elseExpression";

		Class<?> ELSE_EXPRESSION_REF_TYPE =  Expression.class;

		boolean ELSE_EXPRESSION_REF_MULTIPLE = false;

		/**
		 * WHEN Reference
		 */
		String WHEN_REF_NAME = Namespace.PREFIX + COLON + "when";

		Class<?> WHEN_REF_TYPE =  Criteria.class;

		boolean WHEN_REF_MULTIPLE = true;

		/**
		 * THEN Reference
		 */
		String THEN_REF_NAME = Namespace.PREFIX + COLON + "then";

		Class<?> THEN_REF_TYPE =  Expression.class;

		boolean THEN_REF_MULTIPLE = true;

	}

	/**
	 * tsql:textLine
	 */
	public interface TextLine extends Expression {

		String ID = Namespace.PREFIX + COLON + "textLine";

		boolean IS_ABSTRACT = false;

		/**
		 * QUOTE Property
		 */
		String QUOTE_PROP_NAME = Namespace.PREFIX + COLON + "quote";

		Class<?> QUOTE_PROP_TYPE =  String.class;

		boolean QUOTE_PROP_MULTIPLE = false;

		/**
		 * INCLUDE_HEADER Property
		 */
		String INCLUDE_HEADER_PROP_NAME = Namespace.PREFIX + COLON + "includeHeader";

		Class<?> INCLUDE_HEADER_PROP_TYPE =  Boolean.class;

		boolean INCLUDE_HEADER_PROP_MULTIPLE = false;

		/**
		 * ENCODING Property
		 */
		String ENCODING_PROP_NAME = Namespace.PREFIX + COLON + "encoding";

		Class<?> ENCODING_PROP_TYPE =  String.class;

		boolean ENCODING_PROP_MULTIPLE = false;

		/**
		 * DELIMITER Property
		 */
		String DELIMITER_PROP_NAME = Namespace.PREFIX + COLON + "delimiter";

		Class<?> DELIMITER_PROP_TYPE =  String.class;

		boolean DELIMITER_PROP_MULTIPLE = false;

		/**
		 * EXPRESSIONS Reference
		 */
		String EXPRESSIONS_REF_NAME = Namespace.PREFIX + COLON + "expressions";

		Class<?> EXPRESSIONS_REF_TYPE =  DerivedColumn.class;

		boolean EXPRESSIONS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:windowFunction
	 */
	public interface WindowFunction extends Expression {

		String ID = Namespace.PREFIX + COLON + "windowFunction";

		boolean IS_ABSTRACT = false;

		/**
		 * FUNCTION Reference
		 */
		String FUNCTION_REF_NAME = Namespace.PREFIX + COLON + "function";

		Class<?> FUNCTION_REF_TYPE =  AggregateSymbol.class;

		boolean FUNCTION_REF_MULTIPLE = false;

		/**
		 * WINDOW_SPECIFICATION Reference
		 */
		String WINDOW_SPECIFICATION_REF_NAME = Namespace.PREFIX + COLON + "windowSpecification";

		Class<?> WINDOW_SPECIFICATION_REF_TYPE =  WindowSpecification.class;

		boolean WINDOW_SPECIFICATION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:windowSpecification
	 */
	public interface WindowSpecification extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "windowSpecification";

		boolean IS_ABSTRACT = false;

		/**
		 * PARTITION Reference
		 */
		String PARTITION_REF_NAME = Namespace.PREFIX + COLON + "partition";

		Class<?> PARTITION_REF_TYPE =  Expression.class;

		boolean PARTITION_REF_MULTIPLE = true;

		/**
		 * ORDER_BY Reference
		 */
		String ORDER_BY_REF_NAME = Namespace.PREFIX + COLON + "orderBy";

		Class<?> ORDER_BY_REF_TYPE =  OrderBy.class;

		boolean ORDER_BY_REF_MULTIPLE = false;

	}

	/**
	 * tsql:xmlAttributes
	 */
	public interface XMLAttributes extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "xmlAttributes";

		boolean IS_ABSTRACT = false;

		/**
		 * ARGS Reference
		 */
		String ARGS_REF_NAME = Namespace.PREFIX + COLON + "args";

		Class<?> ARGS_REF_TYPE =  DerivedColumn.class;

		boolean ARGS_REF_MULTIPLE = true;

	}

    /**
     * tsql:xmlCast
     */
    public interface XMLCast extends Expression {

        String ID = Namespace.PREFIX + COLON + "xmlCast";

        boolean IS_ABSTRACT = false;

        /**
         * EXPRESSION Property
         */
        String EXPRESSION_PROP_NAME = Namespace.PREFIX + COLON + "expression";

        Class<?> EXPRESSION_PROP_TYPE = Expression.class;

        boolean EXPRESSION_PROP_MULTIPLE = false;
    }

	/**
	 * tsql:xmlElement
	 */
	public interface XMLElement extends Expression {

		String ID = Namespace.PREFIX + COLON + "xmlElement";

		boolean IS_ABSTRACT = false;

		/**
		 * NAME Property
		 */
		String NAME_PROP_NAME = Namespace.PREFIX + COLON + "name";

		Class<?> NAME_PROP_TYPE =  String.class;

		boolean NAME_PROP_MULTIPLE = false;

		/**
		 * CONTENT Reference
		 */
		String CONTENT_REF_NAME = Namespace.PREFIX + COLON + "content";

		Class<?> CONTENT_REF_TYPE =  Expression.class;

		boolean CONTENT_REF_MULTIPLE = true;

		/**
		 * NAMESPACES Reference
		 */
		String NAMESPACES_REF_NAME = Namespace.PREFIX + COLON + "namespaces";

		Class<?> NAMESPACES_REF_TYPE =  XMLNamespaces.class;

		boolean NAMESPACES_REF_MULTIPLE = false;

		/**
		 * ATTRIBUTES Reference
		 */
		String ATTRIBUTES_REF_NAME = Namespace.PREFIX + COLON + "attributes";

		Class<?> ATTRIBUTES_REF_TYPE =  XMLAttributes.class;

		boolean ATTRIBUTES_REF_MULTIPLE = false;

	}

    /**
     * tsql:xmlExists
     */
    public interface XMLExists extends Criteria {

        String ID = Namespace.PREFIX + COLON + "xmlExists";

        boolean IS_ABSTRACT = false;

        /**
         * XML_QUERY Reference
         */
        String XML_QUERY_REF_NAME = Namespace.PREFIX + COLON + "xmlQuery";

        Class<?> XML_QUERY_REF_TYPE = XMLQuery.class;

        boolean XML_QUERY_REF_MULTIPLE = false;
    }

	/**
	 * tsql:xmlForest
	 */
	public interface XMLForest extends Expression {

		String ID = Namespace.PREFIX + COLON + "xmlForest";

		boolean IS_ABSTRACT = false;

		/**
		 * NAMESPACES Reference
		 */
		String NAMESPACES_REF_NAME = Namespace.PREFIX + COLON + "namespaces";

		Class<?> NAMESPACES_REF_TYPE =  XMLNamespaces.class;

		boolean NAMESPACES_REF_MULTIPLE = false;

		/**
		 * ARGUMENTS Reference
		 */
		String ARGUMENTS_REF_NAME = Namespace.PREFIX + COLON + "arguments";

		Class<?> ARGUMENTS_REF_TYPE =  DerivedColumn.class;

		boolean ARGUMENTS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:xmlNamespaces
	 */
	public interface XMLNamespaces extends LanguageObject {

		String ID = Namespace.PREFIX + COLON + "xmlNamespaces";

		boolean IS_ABSTRACT = false;

		/**
		 * Tokens and Constants
		 */
		String NO_DEFAULT = "NO DEFAULT";

		/**
		 * NAMESPACE_ITEMS Reference
		 */
		String NAMESPACE_ITEMS_REF_NAME = Namespace.PREFIX + COLON + "namespaceItems";

		Class<?> NAMESPACE_ITEMS_REF_TYPE =  NamespaceItem.class;

		boolean NAMESPACE_ITEMS_REF_MULTIPLE = true;

	}

	/**
	 * tsql:xmlParse
	 */
	public interface XMLParse extends Expression {

		String ID = Namespace.PREFIX + COLON + "xmlParse";

		boolean IS_ABSTRACT = false;

		/**
		 * DOCUMENT Property
		 */
		String DOCUMENT_PROP_NAME = Namespace.PREFIX + COLON + "document";

		Class<?> DOCUMENT_PROP_TYPE =  Boolean.class;

		boolean DOCUMENT_PROP_MULTIPLE = false;

		/**
		 * WELL_FORMED Property
		 */
		String WELL_FORMED_PROP_NAME = Namespace.PREFIX + COLON + "wellFormed";

		Class<?> WELL_FORMED_PROP_TYPE =  Boolean.class;

		boolean WELL_FORMED_PROP_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}

	/**
	 * tsql:xmlQuery
	 */
	public interface XMLQuery extends Expression {

		String ID = Namespace.PREFIX + COLON + "xmlQuery";

		boolean IS_ABSTRACT = false;

		/**
		 * XQUERY Property
		 */
		String XQUERY_PROP_NAME = Namespace.PREFIX + COLON + "xquery";

		Class<?> XQUERY_PROP_TYPE =  String.class;

		boolean XQUERY_PROP_MULTIPLE = false;

		/**
		 * EMPTY_ON_EMPTY Property
		 */
		String EMPTY_ON_EMPTY_PROP_NAME = Namespace.PREFIX + COLON + "emptyOnEmpty";

		Class<?> EMPTY_ON_EMPTY_PROP_TYPE =  Boolean.class;

		boolean EMPTY_ON_EMPTY_PROP_MULTIPLE = false;

		/**
		 * NAMESPACES Reference
		 */
		String NAMESPACES_REF_NAME = Namespace.PREFIX + COLON + "namespaces";

		Class<?> NAMESPACES_REF_TYPE =  XMLNamespaces.class;

		boolean NAMESPACES_REF_MULTIPLE = false;

		/**
		 * PASSING Reference
		 */
		String PASSING_REF_NAME = Namespace.PREFIX + COLON + "passing";

		Class<?> PASSING_REF_TYPE =  DerivedColumn.class;

		boolean PASSING_REF_MULTIPLE = true;

	}

	/**
	 * tsql:xmlSerialize
	 */
	public interface XMLSerialize extends Expression {

		String ID = Namespace.PREFIX + COLON + "xmlSerialize";

		boolean IS_ABSTRACT = false;

		/**
		 * DOCUMENT Property
		 */
		String DOCUMENT_PROP_NAME = Namespace.PREFIX + COLON + "document";

		Class<?> DOCUMENT_PROP_TYPE =  Boolean.class;

		boolean DOCUMENT_PROP_MULTIPLE = false;

		/**
		 * ENCODING Property
		 */
		String ENCODING_PROP_NAME = Namespace.PREFIX + COLON + "encoding";

		Class<?> ENCODING_PROP_TYPE =  String.class;

		boolean ENCODING_PROP_MULTIPLE = false;

		/**
		 * TYPE_STRING Property
		 */
		String TYPE_STRING_PROP_NAME = Namespace.PREFIX + COLON + "typeString";

		Class<?> TYPE_STRING_PROP_TYPE =  String.class;

		boolean TYPE_STRING_PROP_MULTIPLE = false;

		/**
		 * VERSION Property
		 */
		String VERSION_PROP_NAME = Namespace.PREFIX + COLON + "version";

		Class<?> VERSION_PROP_TYPE =  String.class;

		boolean VERSION_PROP_MULTIPLE = false;

		/**
		 * DECLARATION Property
		 */
		String DECLARATION_PROP_NAME = Namespace.PREFIX + COLON + "declaration";

		Class<?> DECLARATION_PROP_TYPE =  Boolean.class;

		boolean DECLARATION_PROP_MULTIPLE = false;

		/**
		 * EXPRESSION Reference
		 */
		String EXPRESSION_REF_NAME = Namespace.PREFIX + COLON + "expression";

		Class<?> EXPRESSION_REF_TYPE =  Expression.class;

		boolean EXPRESSION_REF_MULTIPLE = false;

	}


	/**
	 * Enumeration of lexicon classes and their identifiers
	 */
	public enum LexTokens {
		EXPRESSION_STATEMENT (ExpressionStatement.ID, ExpressionStatement.class),

		LANGUAGE_OBJECT (LanguageObject.ID, LanguageObject.class),

		EXPRESSION (Expression.ID, Expression.class),

		PREDICATE_CRITERIA (PredicateCriteria.ID, PredicateCriteria.class),

		SUBQUERY_CONTAINER (SubqueryContainer.ID, SubqueryContainer.class),

		TARGETED_COMMAND (TargetedCommand.ID, TargetedCommand.class),

		LABELED (Labeled.ID, Labeled.class),

		CRITERIA (Criteria.ID, Criteria.class),

		CREATE (Create.ID,Create.class),

		ABSTRACT_COMPARE_CRITERIA (AbstractCompareCriteria.ID, AbstractCompareCriteria.class),

		COMPARE_CRITERIA (CompareCriteria.ID, CompareCriteria.class),

		SUBQUERY_COMPARE_CRITERIA (SubqueryCompareCriteria.ID, SubqueryCompareCriteria.class),

		ABSTRACT_SET_CRITERIA (AbstractSetCriteria.ID, AbstractSetCriteria.class),

		SET_CRITERIA (SetCriteria.ID, SetCriteria.class),

		SUBQUERY_SET_CRITERIA (SubquerySetCriteria.ID, SubquerySetCriteria.class),

		BETWEEN_CRITERIA (BetweenCriteria.ID, BetweenCriteria.class),

		COMPOUND_CRITERIA (CompoundCriteria.ID, CompoundCriteria.class),

		EXISTS_CRITERIA (ExistsCriteria.ID, ExistsCriteria.class),

		EXPRESSION_CRITERIA (ExpressionCriteria.ID, ExpressionCriteria.class),

		IS_NULL_CRITERIA (IsNullCriteria.ID, IsNullCriteria.class),

		MATCH_CRITERIA (MatchCriteria.ID, MatchCriteria.class),

		NOT_CRITERIA (NotCriteria.ID, NotCriteria.class),

		COMMAND (Command.ID, Command.class),

		ALTER (Alter.ID, Alter.class),

		ALTER_PROCEDURE (AlterProcedure.ID, AlterProcedure.class),

		ALTER_TRIGGER (AlterTrigger.ID, AlterTrigger.class),

		ALTER_VIEW (AlterView.ID, AlterView.class),

		PROCEDURE_CONTAINER (ProcedureContainer.ID, ProcedureContainer.class),

		DELETE (Delete.ID, Delete.class),

		INSERT (Insert.ID, Insert.class),

		STORED_PROCEDURE (StoredProcedure.ID, StoredProcedure.class),

		UPDATE (Update.ID, Update.class),

		DYNAMIC_COMMAND (DynamicCommand.ID, DynamicCommand.class),

		QUERY_COMMAND (QueryCommand.ID, QueryCommand.class),

		QUERY (Query.ID, Query.class),

		SET_QUERY (SetQuery.ID, SetQuery.class),

		CREATE_PROCEDURE_COMMAND (CreateProcedureCommand.ID, CreateProcedureCommand.class),

		TRIGGER_ACTION (TriggerAction.ID, TriggerAction.class),

		FROM_CLAUSE (FromClause.ID, FromClause.class),

		TABLE_FUNCTION_REFERENCE (TableFunctionReference.ID, TableFunctionReference.class),

		ARRAY_TABLE (ArrayTable.ID, ArrayTable.class),

		OBJECT_TABLE (ObjectTable.ID, ObjectTable.class),

		TEXT_TABLE (TextTable.ID, TextTable.class),

		XML_TABLE (XMLTable.ID, XMLTable.class),

		JOIN_PREDICATE (JoinPredicate.ID, JoinPredicate.class),

		SUBQUERY_FROM_CLAUSE (SubqueryFromClause.ID, SubqueryFromClause.class),

		UNARY_FROM_CLAUSE (UnaryFromClause.ID, UnaryFromClause.class),

		FROM (From.ID, From.class),

		GROUP_BY (GroupBy.ID, GroupBy.class),

		INTO (Into.ID, Into.class),

		JOIN_TYPE (JoinType.ID, JoinType.class),

		LIMIT (Limit.ID, Limit.class),

		MAKE_DEP (MakeDep.ID, MakeDep.class),

		NAMESPACE_ITEM (NamespaceItem.ID, NamespaceItem.class),

		NULL_NODE (NullNode.ID, NullNode.class),

		PROJECTED_COLUMN (ProjectedColumn.ID, ProjectedColumn.class),

		OBJECT_COLUMN (ObjectColumn.ID, ObjectColumn.class),

		TEXT_COLUMN (TextColumn.ID, TextColumn.class),

		XML_COLUMN (XMLColumn.ID, XMLColumn.class),

		OPTION (Option.ID, Option.class),

		ORDER_BY (OrderBy.ID, OrderBy.class),

		ORDER_BY_ITEM (OrderByItem.ID, OrderByItem.class),

		SP_PARAMETER (SPParameter.ID, SPParameter.class),

		SELECT (Select.ID, Select.class),

		SET_CLAUSE (SetClause.ID, SetClause.class),

		SET_CLAUSE_LIST (SetClauseList.ID, SetClauseList.class),

		SOURCE_HINT (SourceHint.ID, SourceHint.class),

		SPECIFIC_HINT (SpecificHint.ID, SpecificHint.class),

		SUBQUERY_HINT (SubqueryHint.ID, SubqueryHint.class),

		WITH_QUERY_COMMAND (WithQueryCommand.ID, WithQueryCommand.class),

		STATEMENT (Statement.ID, Statement.class),

		ASSIGNMENT_STATEMENT (AssignmentStatement.ID, AssignmentStatement.class),

		DECLARE_STATEMENT (DeclareStatement.ID, DeclareStatement.class),

		RETURN_STATEMENT (ReturnStatement.ID, ReturnStatement.class),

		BLOCK (Block.ID, Block.class),

		BRANCHING_STATEMENT (BranchingStatement.ID, BranchingStatement.class),

		COMMAND_STATEMENT (CommandStatement.ID, CommandStatement.class),

		IF_STATEMENT (IfStatement.ID, IfStatement.class),

		LOOP_STATEMENT (LoopStatement.ID, LoopStatement.class),

		RAISE_STATEMENT (RaiseStatement.ID, RaiseStatement.class),

		WHILE_STATEMENT (WhileStatement.ID, WhileStatement.class),

		EXCEPTION_EXPRESSION (ExceptionExpression.ID, ExceptionExpression.class),

		FUNCTION (Function.ID, Function.class),

		AGGREGATE_SYMBOL (AggregateSymbol.ID, AggregateSymbol.class),

		SYMBOL (Symbol.ID, Symbol.class),

		ALIAS_SYMBOL (AliasSymbol.ID, AliasSymbol.class),

		ELEMENT_SYMBOL (ElementSymbol.ID, ElementSymbol.class),

		EXPRESSION_SYMBOL (ExpressionSymbol.ID, ExpressionSymbol.class),

		GROUP_SYMBOL (GroupSymbol.ID, GroupSymbol.class),

		ARRAY_SYMBOL (ArraySymbol.ID, ArraySymbol.class),

		CASE_EXPRESSION (CaseExpression.ID, CaseExpression.class),

		CONSTANT (Constant.ID, Constant.class),

		DERIVED_COLUMN (DerivedColumn.ID, DerivedColumn.class),

		JSON_OBJECT (JSONObject.ID, JSONObject.class),

		MULTIPLE_ELEMENT_SYMBOL (MultipleElementSymbol.ID, MultipleElementSymbol.class),

		QUERY_STRING (QueryString.ID, QueryString.class),

		REFERENCE (Reference.ID, Reference.class),

		SCALAR_SUBQUERY (ScalarSubquery.ID, ScalarSubquery.class),

		SEARCHED_CASE_EXPRESSION (SearchedCaseExpression.ID, SearchedCaseExpression.class),

		TEXT_LINE (TextLine.ID, TextLine.class),

		WINDOW_FUNCTION (WindowFunction.ID, WindowFunction.class),

		WINDOW_SPECIFICATION (WindowSpecification.ID, WindowSpecification.class),

		XML_ATTRIBUTES (XMLAttributes.ID, XMLAttributes.class),

		XML_CAST (XMLCast.ID, XMLCast.class),

		XML_ELEMENT (XMLElement.ID, XMLElement.class),

		XML_EXISTS (XMLExists.ID, XMLExists.class),

		XML_FOREST (XMLForest.ID, XMLForest.class),

		XML_NAMESPACES (XMLNamespaces.ID, XMLNamespaces.class),

		XML_PARSE (XMLParse.ID, XMLParse.class),

		XML_QUERY (XMLQuery.ID, XMLQuery.class),

		XML_SERIALIZE (XMLSerialize.ID, XMLSerialize.class),

	    CACHE_HINT (CacheHint.ID, CacheHint.class),

	    IS_DISTINCT_CRITERIA(IsDistinctCriteria.ID, IsDistinctCriteria.class);

		private String id;

		private Class<?> klazz;

		private LexTokens(String id, Class<?> klazz){

			this.id = id;
			this.klazz = klazz;
		}

		public String getId() {
			return this.id;
		}

		public Class<?> getTSqlClass() {
			return this.klazz;
		}

		public static LexTokens findClass(String id) {
			for (LexTokens tSqlClass : LexTokens.values()) {
				if (tSqlClass.getId().equals(id))
					return tSqlClass;
			}
			return null;
		}
	}
}
