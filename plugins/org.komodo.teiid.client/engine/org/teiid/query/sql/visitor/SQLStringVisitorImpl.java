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

package org.teiid.query.sql.visitor;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.sql.SQLStringVisitor;
import org.komodo.spi.query.sql.symbol.AggregateSymbol.Type;
import org.komodo.spi.query.sql.symbol.ElementSymbol.DisplayMode;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.core.types.ArrayImpl;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.core.util.StringUtil;
import org.teiid.language.SQLConstants;
import org.teiid.language.SQLConstants.NonReserved;
import org.teiid.language.SQLConstants.Tokens;
import org.teiid.metadata.AbstractMetadataRecord;
import org.teiid.metadata.BaseColumn;
import org.teiid.metadata.BaseColumn.NullType;
import org.teiid.metadata.Column;
import org.teiid.metadata.ForeignKey;
import org.teiid.metadata.KeyRecord;
import org.teiid.metadata.MetadataFactory;
import org.teiid.metadata.Table;
import org.teiid.query.metadata.DDLConstants;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.AlterProcedureImpl;
import org.teiid.query.sql.lang.AlterTriggerImpl;
import org.teiid.query.sql.lang.AlterViewImpl;
import org.teiid.query.sql.lang.ArrayTableImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CompoundCriteriaImpl;
import org.teiid.query.sql.lang.CreateImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.CriteriaSelectorImpl;
import org.teiid.query.sql.lang.DeleteImpl;
import org.teiid.query.sql.lang.DropImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.ExpressionCriteriaImpl;
import org.teiid.query.sql.lang.FromClauseImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.HasCriteriaImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IntoImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.JoinPredicateImpl;
import org.teiid.query.sql.lang.JoinTypeImpl;
import org.teiid.query.sql.lang.Labeled;
import org.teiid.query.sql.lang.LimitImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NamespaceItem;
import org.teiid.query.sql.lang.NotCriteriaImpl;
import org.teiid.query.sql.lang.ObjectColumnImpl;
import org.teiid.query.sql.lang.ObjectTableImpl;
import org.teiid.query.sql.lang.OptionImpl;
import org.teiid.query.sql.lang.OptionImpl.MakeDep;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.OrderByItemImpl;
import org.teiid.query.sql.lang.PredicateCriteria;
import org.teiid.query.sql.lang.ProjectedColumnImpl;
import org.teiid.query.sql.lang.QueryCommandImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.SetClauseListImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.SourceHintImpl;
import org.teiid.query.sql.lang.SourceHintImpl.SpecificHint;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubqueryHint;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.TextColumnImpl;
import org.teiid.query.sql.lang.TextTableImpl;
import org.teiid.query.sql.lang.TranslateCriteriaImpl;
import org.teiid.query.sql.lang.UnaryFromClauseImpl;
import org.teiid.query.sql.lang.UpdateImpl;
import org.teiid.query.sql.lang.WithQueryCommandImpl;
import org.teiid.query.sql.lang.XMLColumnImpl;
import org.teiid.query.sql.lang.XMLTableImpl;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateProcedureCommandImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.DeclareStatementImpl;
import org.teiid.query.sql.proc.ExceptionExpressionImpl;
import org.teiid.query.sql.proc.IfStatementImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.proc.RaiseErrorStatementImpl;
import org.teiid.query.sql.proc.RaiseStatementImpl;
import org.teiid.query.sql.proc.ReturnStatementImpl;
import org.teiid.query.sql.proc.StatementImpl;
import org.teiid.query.sql.proc.TriggerActionImpl;
import org.teiid.query.sql.proc.WhileStatementImpl;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.ArraySymbolImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.BaseWindowFunction;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.JSONObjectImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.QueryStringImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.symbol.TextLineImpl;
import org.teiid.query.sql.symbol.WindowSpecificationImpl;
import org.teiid.query.sql.symbol.XMLAttributesImpl;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLForestImpl;
import org.teiid.query.sql.symbol.XMLNamespacesImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;
import org.teiid.translator.SourceSystemFunctions;

/**
 * <p>
 * The SQLStringVisitor will visit a set of ast nodes and return the corresponding SQL string representation.
 * </p>
 */
public class SQLStringVisitorImpl extends TCLanguageVisitorImpl
    implements SQLConstants.Reserved, SQLConstants.NonReserved, SQLConstants.Tokens, DDLConstants, SQLStringVisitor<BaseLanguageObject> {

    @Since(Version.TEIID_8_0)
    private final static Map<String, String> BUILTIN_PREFIXES = new HashMap<String, String>();
    static {
        for (Map.Entry<String, String> entry : MetadataFactory.BUILTIN_NAMESPACES.entrySet()) {
            BUILTIN_PREFIXES.put(entry.getValue(), entry.getKey());
        }
    }

    @Since(Version.TEIID_8_0)
    private static final HashSet<String> LENGTH_DATATYPES = new HashSet<String>(
        Arrays.asList(
            DefaultDataTypeManager.DefaultDataTypes.CHAR.getId(),
            DefaultDataTypeManager.DefaultDataTypes.CLOB.getId(),
            DefaultDataTypeManager.DefaultDataTypes.BLOB.getId(),
            DefaultDataTypeManager.DefaultDataTypes.OBJECT.getId(),
            DefaultDataTypeManager.DefaultDataTypes.XML.getId(),
            DefaultDataTypeManager.DefaultDataTypes.STRING.getId(),
            DefaultDataTypeManager.DefaultDataTypes.VARBINARY.getId(),
            DefaultDataTypeManager.DefaultDataTypes.BIG_INTEGER.getId()));

    @Since(Version.TEIID_8_0)
    private static final HashSet<String> PRECISION_DATATYPES = new HashSet<String>(
        Arrays.asList(DefaultDataTypeManager.DefaultDataTypes.BIG_DECIMAL.getId()));

    /**
     * Undefined
     */
    public static final String UNDEFINED = "<undefined>"; //$NON-NLS-1$

    private static final String BEGIN_HINT = "/*+"; //$NON-NLS-1$

    private static final String END_HINT = "*/"; //$NON-NLS-1$

    private static final char ID_ESCAPE_CHAR = '\"';

    protected StringBuilder parts = new StringBuilder();

    private boolean shortNameOnly = false;

    /**
     * @param teiidVersion
     */
    public SQLStringVisitorImpl(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    /**
     * Helper to quickly get the parser string for an object using the visitor.
     *
     * @param obj Language object
     *
     * @return String SQL String for obj
     */
    public static final String getSQLString(BaseLanguageObject obj) {
        if (obj == null) {
            return UNDEFINED;
        }

        SQLStringVisitorImpl visitor = new SQLStringVisitorImpl(obj.getTeiidVersion());
        return visitor.returnSQLString(obj);
    }

    /**
     * @param languageObject
     * @return sql representation of {@link BaseLanguageObject}
     */
    @Override
    public String returnSQLString(BaseLanguageObject languageObject) {
        if (languageObject == null) {
            return UNDEFINED;
        }

        isApplicable(languageObject);
        languageObject.acceptVisitor(this);
        return getSQLString();
    }

    /**
     * Retrieve completed string from the visitor.
     *
     * @return Complete SQL string for the visited nodes
     */
    public String getSQLString() {
        return this.parts.toString();
    }

    /**
     * @return the shortNameOnly
     */
    public boolean isShortNameOnly() {
        if (!isTeiid8OrGreater())
            return false; // Not applicable for teiid 7

        return this.shortNameOnly;
    }

    /**
     * @param shortNameOnly the shortNameOnly to set
     */
    private void setShortNameOnly(boolean shortNameOnly) {
        if (!isTeiid8OrGreater())
            return; // Not applicable for teiid 7

        this.shortNameOnly = shortNameOnly;
    }

    protected void visitNode(BaseLanguageObject obj) {
        if (obj == null) {
            append(UNDEFINED);
            return;
        }
        isApplicable(obj);
        obj.acceptVisitor(this);
    }

    protected void append(Object value) {
        this.parts.append(value);
    }

    protected void beginClause(int level) {
        append(SPACE);
    }

    private ConstantImpl newConstant(Object value) {
        ConstantImpl constant = createNode(ASTNodes.CONSTANT);
        constant.setValue(value);
        return constant;
    }

    // ############ Visitor methods for language objects ####################

    @Override
    public void visit(BetweenCriteriaImpl obj) {
        visitNode(obj.getExpression());
        append(SPACE);

        if (obj.isNegated()) {
            append(NOT);
            append(SPACE);
        }
        append(BETWEEN);
        append(SPACE);
        visitNode(obj.getLowerExpression());

        append(SPACE);
        append(AND);
        append(SPACE);
        visitNode(obj.getUpperExpression());
    }

    @Override
    public void visit(CaseExpressionImpl obj) {
        append(CASE);
        append(SPACE);
        visitNode(obj.getExpression());
        append(SPACE);

        for (int i = 0; i < obj.getWhenCount(); i++) {
            append(WHEN);
            append(SPACE);
            visitNode(obj.getWhenExpression(i));
            append(SPACE);
            append(THEN);
            append(SPACE);
            visitNode(obj.getThenExpression(i));
            append(SPACE);
        }

        if (obj.getElseExpression() != null) {
            append(ELSE);
            append(SPACE);
            visitNode(obj.getElseExpression());
            append(SPACE);
        }
        append(END);
    }

    @Override
    public void visit(CompareCriteriaImpl obj) {
        BaseExpression leftExpression = obj.getLeftExpression();
        visitNode(leftExpression);
        append(SPACE);
        append(obj.getOperatorAsString());
        append(SPACE);
        BaseExpression rightExpression = obj.getRightExpression();
        visitNode(rightExpression);
    }

    @Override
    public void visit(CompoundCriteriaImpl obj) {
        // Get operator string
        int operator = obj.getOperator();
        String operatorStr = ""; //$NON-NLS-1$
        if (operator == CompoundCriteriaImpl.AND) {
            operatorStr = AND;
        } else if (operator == CompoundCriteriaImpl.OR) {
            operatorStr = OR;
        }

        // Get criteria
        List<CriteriaImpl> subCriteria = obj.getCriteria();

        // Build parts
        if (subCriteria.size() == 1) {
            // Special case - should really never happen, but we are tolerant
            CriteriaImpl firstChild = subCriteria.get(0);
            visitNode(firstChild);
        } else {
            // Add first criteria
            Iterator<CriteriaImpl> iter = subCriteria.iterator();

            while (iter.hasNext()) {
                // Add criteria
                CriteriaImpl crit = iter.next();
                append(Tokens.LPAREN);
                visitNode(crit);
                append(Tokens.RPAREN);

                if (iter.hasNext()) {
                    // Add connector
                    append(SPACE);
                    append(operatorStr);
                    append(SPACE);
                }
            }
        }
    }

    @Override
    public void visit(DeleteImpl obj) {
        // add delete clause
        append(DELETE);
        addSourceHint(obj.getSourceHint());
        append(SPACE);
        // add from clause
        append(FROM);
        append(SPACE);
        visitNode(obj.getGroup());

        // add where clause
        if (obj.getCriteria() != null) {
            beginClause(0);
            visitCriteria(WHERE, obj.getCriteria());
        }

        // Option clause
        if (obj.getOption() != null) {
            beginClause(0);
            visitNode(obj.getOption());
        }
    }

    @Override
    public void visit(FromImpl obj) {
        append(FROM);
        beginClause(1);
        registerNodes(obj.getClauses(), 0);
    }

    @Override
    public void visit(GroupByImpl obj) {
        append(GROUP);
        append(SPACE);
        append(BY);
        append(SPACE);
        if (isTeiidVersionOrGreater(Version.TEIID_8_5) && obj.isRollup()) {
        	append(ROLLUP);
        	append(Tokens.LPAREN);
        }
        registerNodes(obj.getSymbols(), 0);
        if (isTeiidVersionOrGreater(Version.TEIID_8_5) && obj.isRollup()) {
        	append(Tokens.RPAREN);
        }
    }

    @Override
    public void visit(InsertImpl obj) {
        if (isTeiid8OrGreater() && obj.isMerge()) {
            append(MERGE);
        } else {
            append(INSERT);
        }
        addSourceHint(obj.getSourceHint());
        append(SPACE);
        append(INTO);
        append(SPACE);
        visitNode(obj.getGroup());

        if (!obj.getVariables().isEmpty()) {
            beginClause(2);

            // Columns clause
            List<ElementSymbolImpl> vars = obj.getVariables();
            if (vars != null) {
                append("("); //$NON-NLS-1$
                setShortNameOnly(true);
                registerNodes(vars, 0);
                setShortNameOnly(false);
                append(")"); //$NON-NLS-1$
            }
        }
        beginClause(1);
        if (obj.getQueryExpression() != null) {
            visitNode(obj.getQueryExpression());
            //         } else if (obj.getTupleSource() != null) {
            //             append(VALUES);
            //             append(" (...)"); //$NON-NLS-1$
        } else if (obj.getValues() != null) {
            append(VALUES);
            beginClause(2);
            append("("); //$NON-NLS-1$
            registerNodes(obj.getValues(), 0);
            append(")"); //$NON-NLS-1$
        }

        // Option clause
        if (obj.getOption() != null) {
            beginClause(1);
            visitNode(obj.getOption());
        }
    }

    private void visit7( CreateImpl obj ) {
        append(CREATE);
        append(SPACE);
        append(LOCAL);
        append(SPACE);
        append(TEMPORARY);
        append(SPACE);
        append(TABLE);
        append(SPACE);
        visitNode(obj.getTable());
        append(SPACE);

        // Columns clause
        List<Column> columns = obj.getColumns();
        append("("); //$NON-NLS-1$
        Iterator<Column> iter = columns.iterator();
        while (iter.hasNext()) {
            Column element = iter.next();
            outputDisplayName(element.getName());
            append(SPACE);
            if (element.isAutoIncremented()) {
                append(NonReserved.SERIAL);
            } else {
                append(element.getRuntimeType());
                if (element.getNullType() == NullType.No_Nulls) {
                    append(NOT);
                    append(SPACE);
                    append(NULL);
                }
            }
            if (iter.hasNext()) {
                append(", "); //$NON-NLS-1$
            }
        }
        if (!obj.getPrimaryKey().isEmpty()) {
            append(", "); //$NON-NLS-1$
            append(PRIMARY);
            append(" "); //$NON-NLS-1$
            append(NonReserved.KEY);
            append(Tokens.LPAREN);
            Iterator<ElementSymbolImpl> pkiter = obj.getPrimaryKey().iterator();
            while (pkiter.hasNext()) {
                outputShortName(pkiter.next());
                if (pkiter.hasNext()) {
                    append(", "); //$NON-NLS-1$
                }
            }
            append(Tokens.RPAREN);
        }
        append(")"); //$NON-NLS-1$
    }

    private String buildTableOptions(Table table) {
        StringBuilder options = new StringBuilder();
        addCommonOptions(options, table);
        
        if (table.isMaterialized()) {
            addOption(options, MATERIALIZED, table.isMaterialized());
            if (table.getMaterializedTable() != null) {
                addOption(options, MATERIALIZED_TABLE, table.getMaterializedTable().getName());
            }
        }
        if (table.supportsUpdate()) {
            addOption(options, UPDATABLE, table.supportsUpdate());
        }
        if (table.getCardinality() != -1) {
            addOption(options, CARDINALITY, table.getCardinality());
        }
        if (!table.getProperties().isEmpty()) {
            for (String key:table.getProperties().keySet()) {
                addOption(options, key, table.getProperty(key, false));
            }
        }
        return options.toString();
    }

    private void addCommonOptions(StringBuilder sb, AbstractMetadataRecord record) {
        if (record.getUUID() != null && !record.getUUID().startsWith("tid:")) { //$NON-NLS-1$
            addOption(sb, UUID, record.getUUID());
        }
        if (record.getAnnotation() != null) {
            addOption(sb, ANNOTATION, record.getAnnotation());
        }
        if (record.getNameInSource() != null && !record.getNameInSource().equals(record.getName())) {
            addOption(sb, NAMEINSOURCE, record.getNameInSource());
        }
    }
    
    private void buildContraints(Table table) {
        addConstraints(table.getAccessPatterns(), "AP", ACCESSPATTERN); //$NON-NLS-1$
        
        KeyRecord pk = table.getPrimaryKey();
        if (pk != null) {
            addConstraint("PK", PRIMARY_KEY, pk, true); //$NON-NLS-1$
        }

        addConstraints(table.getUniqueKeys(), UNIQUE, UNIQUE);
        addConstraints(table.getIndexes(), INDEX, INDEX);
        addConstraints(table.getFunctionBasedIndexes(), INDEX, INDEX);

        for (int i = 0; i < table.getForeignKeys().size(); i++) {
            ForeignKey key = table.getForeignKeys().get(i);
            addConstraint("FK" + i, FOREIGN_KEY, key, false); //$NON-NLS-1$
            append(SPACE);
            append(REFERENCES);
            if (key.getReferenceTableName() != null) {
                append(SPACE);
                GroupSymbolImpl gs = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
                gs.setName(key.getReferenceTableName());
                append(gs.getName());
            }
            append(SPACE);
            addNames(key.getReferenceColumns());
            appendOptions(key);
        }
    }

    private void addConstraints(List<KeyRecord> constraints, String defaultName, String type) {
        for (int i = 0; i < constraints.size(); i++) {
            KeyRecord constraint = constraints.get(i);
            addConstraint(defaultName + i, type, constraint, true);
        }
    }

    private void addConstraint(String defaultName, String type,
            KeyRecord constraint, boolean addOptions) {
        append(COMMA);
        append(NEWLINE);
        append(TAB);
        boolean nameMatches = defaultName.equals(constraint.getName());
        if (!nameMatches) {
            append(CONSTRAINT);
            append(SPACE);
            append(escapeSinglePart(constraint.getName()));
            append(SPACE); 
        }
        append(type);
        addColumns(constraint.getColumns(), false);
        if (addOptions) {
            appendOptions(constraint);
        }
    }

    private void addColumns(List<Column> columns, boolean includeType) {
        append(LPAREN);
        boolean first = true;
        for (Column c:columns) {
            if (first) {
                first = false;
            }
            else {
                append(COMMA);
                append(SPACE);
            }
            if (includeType) {
                appendColumn(c, true, includeType);
                appendColumnOptions(c);
            } else if (c.getParent() instanceof KeyRecord) {
                //function based column
                append(c.getNameInSource());
            } else {
                append(escapeSinglePart(c.getName()));
            }
        }
        append(RPAREN);
    }

    private void addNames(List<String> columns) {
        if (columns != null) {
            append(LPAREN);
            boolean first = true;
            for (String c:columns) {
                if (first) {
                    first = false;
                }
                else {
                    append(COMMA);
                    append(SPACE);
                }
                append(escapeSinglePart(c));
            }
            append(RPAREN);
        }
    }   
    
    private void visit(Column column) {
        append(NEWLINE);
        append(TAB);
        appendColumn(column, true, true);
        
        if (column.isAutoIncremented()) {
            append(SPACE);
            append(DDLConstants.AUTO_INCREMENT);
        }
        
        appendDefault(column);
        
        // options
        appendColumnOptions(column);
    }

    private void appendDefault(BaseColumn column) {
        if (column.getDefaultValue() != null) {
            append(SPACE);
            append(DEFAULT);
            append(SPACE);
            append(TICK);
            append(StringUtil.replaceAll(column.getDefaultValue(), TICK, TICK + TICK));
            append(TICK);
        }
    }

    private void appendColumn(BaseColumn column, boolean includeName, boolean includeType) {
        if (includeName) {
            append(escapeSinglePart(column.getName()));
        }
        if (includeType) {
            String runtimeTypeName = column.getDatatype().getRuntimeTypeName();
            if (includeName) {
                append(SPACE);
            }
            append(runtimeTypeName);
            if (LENGTH_DATATYPES.contains(runtimeTypeName)) {
                if (column.getLength() != 0 && column.getLength() != column.getDatatype().getLength()) {
                    append(LPAREN);
                    append(column.getLength());
                    append(RPAREN);
                }
            } else if (PRECISION_DATATYPES.contains(runtimeTypeName) 
                    && (column.getPrecision() != column.getDatatype().getPrecision() || column.getScale() != column.getDatatype().getScale())) {
                append(LPAREN);
                append(column.getPrecision());
                if (column.getScale() != 0) {
                    append(COMMA);
                    append(column.getScale());
                }
                append(RPAREN);
            }
            if (column.getNullType() == NullType.No_Nulls) {
                append(SPACE);
                append(NOT_NULL);
            }
        }
    }   
    
    private void appendColumnOptions(BaseColumn column) {
        StringBuilder options = new StringBuilder();
        addCommonOptions(options, column);
        
        if (!column.getDatatype().isBuiltin()) {
            addOption(options, UDT, column.getDatatype().getName() + "("+column.getLength()+ ", " +column.getPrecision()+", " + column.getScale()+ ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        }
        
        if (column.getDatatype().getRadix() != 0 && column.getRadix() != column.getDatatype().getRadix()) {
            addOption(options, RADIX, column.getRadix());
        }
        
        if (column instanceof Column) {
            buildColumnOptions((Column)column, options);
        }
        if (options.length() != 0) {
            append(SPACE);
            append(OPTIONS);
            append(SPACE);
            append(LPAREN);
            append(options);
            append(RPAREN);
        }
    }

    private void buildColumnOptions(Column column, 
            StringBuilder options) {
        if (!column.isSelectable()) {
            addOption(options, SELECTABLE, column.isSelectable());
        }       

        // if table is already updatable, then columns are implicitly updatable.
        if (!column.isUpdatable() && column.getParent() instanceof Table && ((Table)column.getParent()).supportsUpdate()) {
            addOption(options, UPDATABLE, column.isUpdatable());
        }
        
        if (column.isCurrency()) {
            addOption(options, CURRENCY, column.isCurrency());
        }
            
        // only record if not default
        if (!column.isCaseSensitive() && column.getDatatype().isCaseSensitive()) {
            addOption(options, CASE_SENSITIVE, column.isCaseSensitive());
        }
        
        if (!column.isSigned() && column.getDatatype().isSigned()) {
            addOption(options, SIGNED, column.isSigned());
        }         
        if (column.isFixedLength()) {
            addOption(options, FIXED_LENGTH, column.isFixedLength());
        }
        // length and octet length should be same. so this should be never be true.
        //TODO - this is not quite valid since we are dealing with length representing chars in UTF-16, then there should be twice the bytes
        if (column.getCharOctetLength() != 0 && column.getLength() != column.getCharOctetLength()) {
            addOption(options, CHAR_OCTET_LENGTH, column.getCharOctetLength());
        }   
        
        // by default the search type is default data type search, so avoid it.
        if (column.getSearchType() != null && (!column.getSearchType().equals(column.getDatatype().getSearchType()) || column.isSearchTypeSet())) {
            addOption(options, SEARCHABLE, column.getSearchType().name());
        }
        
        if (column.getMinimumValue() != null) {
            addOption(options, MIN_VALUE, column.getMinimumValue());
        }
        
        if (column.getMaximumValue() != null) {
            addOption(options, MAX_VALUE, column.getMaximumValue());
        }
        
        if (column.getNativeType() != null) {
            addOption(options, NATIVE_TYPE, column.getNativeType());
        }
        
        if (column.getNullValues() != -1) {
            addOption(options, NULL_VALUE_COUNT, column.getNullValues());
        }
        
        if (column.getDistinctValues() != -1) {
            addOption(options, DISTINCT_VALUES, column.getDistinctValues());
        }       
        
        buildOptions(column, options);
    }
    
    private void appendOptions(AbstractMetadataRecord record) {
        StringBuilder options = new StringBuilder();
        buildOptions(record, options);
        if (options.length() != 0) {
            append(SPACE);
            append(OPTIONS);
            append(SPACE);
            append(LPAREN);
            append(options);
            append(RPAREN);
        }
    }

    private void buildOptions(AbstractMetadataRecord record, StringBuilder options) {
        if (!record.getProperties().isEmpty()) {
            for (Map.Entry<String, String> entry:record.getProperties().entrySet()) {
                addOption(options, entry.getKey(), entry.getValue());
            }
        }
    }   
    
    private void addOption(StringBuilder sb, String key, Object value) {
        if (sb.length() != 0) {
            sb.append(COMMA).append(SPACE);
        }

        ConstantImpl c = getTeiidParser().createASTNode(ASTNodes.CONSTANT);
        c.setValue(value);
        value = c;

        if (key != null && key.length() > 2 && key.charAt(0) == '{') { 
            String origKey = key;
            int index = key.indexOf('}');
            if (index > 1) {
                String uri = key.substring(1, index);
                key = key.substring(index + 1, key.length());
                String prefix = BUILTIN_PREFIXES.get(uri);
                if (prefix != null) {
                    key = prefix + ":" + key; //$NON-NLS-1$
                } else {
                    key = origKey;
                }
            }
        }
        sb.append(escapeSinglePart(key));
        append(SPACE);
        append(value);
    }

    private String addTableBody(Table table) {
        String name = escapeSinglePart(table.getName());
        append(name);
        
        if (table.getColumns() != null) {
            append(SPACE);
            append(LPAREN);
            boolean first = true; 
            for (Column c:table.getColumns()) {
                if (first) {
                    first = false;
                }
                else {
                    append(COMMA);
                }
                visit(c);
            }
            buildContraints(table);
            append(NEWLINE);
            append(RPAREN);         
        }
        
        // options
        String options = buildTableOptions(table);      
        if (!options.isEmpty()) {
            append(SPACE);
            append(OPTIONS);
            append(SPACE);
            append(LPAREN);
            append(options);
            append(RPAREN);
        }
        return name;
    }

    private void visit8( CreateImpl obj ) {
        append(CREATE);
        append(SPACE);
        if (obj.getTableMetadata() != null) {
            append(FOREIGN);
            append(SPACE);
            append(TEMPORARY);
            append(SPACE);
            append(TABLE);
            append(SPACE);
            
            addTableBody(obj.getTableMetadata());

            append(SPACE);
            append(ON);
            append(SPACE);
            outputLiteral(String.class, false, obj.getOn());
            return;
        }
        append(LOCAL);
        append(SPACE);
        append(TEMPORARY);
        append(SPACE);
        append(TABLE);
        append(SPACE);
        visitNode(obj.getTable());
        append(SPACE);

        // Columns clause
        List<Column> columns = obj.getColumns();
        append("("); //$NON-NLS-1$
        Iterator<Column> iter = columns.iterator();
        while (iter.hasNext()) {
            Column element = iter.next();
            outputDisplayName(element.getName());
            append(SPACE);
            if (element.isAutoIncremented()) {
                append(NonReserved.SERIAL);
            } else {
                append(element.getRuntimeType());
                if (element.getNullType() == NullType.No_Nulls) {
                    append(SPACE);
                    append(NOT);
                    append(SPACE);
                    append(NULL);
                }
            }
            if (iter.hasNext()) {
                append(", "); //$NON-NLS-1$
            }
        }
        if (!obj.getPrimaryKey().isEmpty()) {
            append(", "); //$NON-NLS-1$
            append(PRIMARY);
            append(" "); //$NON-NLS-1$
            append(NonReserved.KEY);
            append(Tokens.LPAREN);
            Iterator<ElementSymbolImpl> pkiter = obj.getPrimaryKey().iterator();
            while (pkiter.hasNext()) {
                outputShortName(pkiter.next());
                if (pkiter.hasNext()) {
                    append(", "); //$NON-NLS-1$
                }
            }
            append(Tokens.RPAREN);
        }
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(CreateImpl obj) {
        if (isTeiid8OrGreater())
            visit8(obj);
        else
            visit7(obj);
    }

    @Override
    public void visit(DropImpl obj) {
        append(DROP);
        append(SPACE);
        append(TABLE);
        append(SPACE);
        visitNode(obj.getTable());
    }

    @Override
    public void visit(IsNullCriteriaImpl obj) {
        BaseExpression expr = obj.getExpression();
        if (isTeiid8OrGreater())
            appendNested(expr);
        else
            visitNode(expr);

        append(SPACE);
        append(IS);
        append(SPACE);
        if (obj.isNegated()) {
            append(NOT);
            append(SPACE);
        }
        append(NULL);
    }

    @Override
    public void visit(JoinPredicateImpl obj) {
        addHintComment(obj);

        if (obj.hasHint()) {
            append("(");//$NON-NLS-1$
        }

        // left clause
        FromClauseImpl leftClause = obj.getLeftClause();
        if (leftClause instanceof JoinPredicateImpl && !((JoinPredicateImpl)leftClause).hasHint()) {
            append("("); //$NON-NLS-1$
            visitNode(leftClause);
            append(")"); //$NON-NLS-1$
        } else {
            visitNode(leftClause);
        }

        // join type
        append(SPACE);
        visitNode(obj.getJoinType());
        append(SPACE);

        // right clause
        FromClauseImpl rightClause = obj.getRightClause();
        if (rightClause instanceof JoinPredicateImpl && !((JoinPredicateImpl)rightClause).hasHint()) {
            append("("); //$NON-NLS-1$
            visitNode(rightClause);
            append(")"); //$NON-NLS-1$
        } else {
            visitNode(rightClause);
        }

        // join criteria
        List joinCriteria = obj.getJoinCriteria();
        if (joinCriteria != null && joinCriteria.size() > 0) {
            append(SPACE);
            append(ON);
            append(SPACE);
            Iterator critIter = joinCriteria.iterator();
            while (critIter.hasNext()) {
                CriteriaImpl crit = (CriteriaImpl)critIter.next();
                if (crit instanceof PredicateCriteria || crit instanceof NotCriteriaImpl) {
                    visitNode(crit);
                } else {
                    append("("); //$NON-NLS-1$
                    visitNode(crit);
                    append(")"); //$NON-NLS-1$
                }

                if (critIter.hasNext()) {
                    append(SPACE);
                    append(AND);
                    append(SPACE);
                }
            }
        }

        if (obj.hasHint()) {
            append(")"); //$NON-NLS-1$
        }

        addMakeDep(obj);
    }

    private void addHintComment(FromClauseImpl obj) {
        if (obj.hasHint()) {
            append(BEGIN_HINT);
            append(SPACE);
            if (obj.isOptional()) {
                append(OptionImpl.OPTIONAL);
                append(SPACE);
            }
            if (obj.getMakeDep() != null && obj.getMakeDep().isSimple()) {
                append(OptionImpl.MAKEDEP);
                append(SPACE);
            }
            if (obj.isMakeNotDep()) {
                append(OptionImpl.MAKENOTDEP);
                append(SPACE);
            }
            if (obj.isMakeInd()) {
                append(FromClauseImpl.MAKEIND);
                append(SPACE);
            }
            if (obj.isNoUnnest()) {
                append(SubqueryHint.NOUNNEST);
                append(SPACE);
            }

            if (isTeiid8OrGreater() && obj.isPreserve()) {
                append(FromClauseImpl.PRESERVE);
                append(SPACE);
            }
            append(END_HINT);
            append(SPACE);
        }
    }

    @Override
    public void visit(JoinTypeImpl obj) {
        String[] output = null;
        switch (obj.getKind()) {
            case JOIN_ANTI_SEMI:
                output = new String[] {"ANTI SEMI", SPACE, JOIN}; //$NON-NLS-1$
                break;
            case JOIN_CROSS:
                output = new String[] {CROSS, SPACE, JOIN};
                break;
            case JOIN_FULL_OUTER:
                output = new String[] {FULL, SPACE, OUTER, SPACE, JOIN};
                break;
            case JOIN_INNER:
                output = new String[] {INNER, SPACE, JOIN};
                break;
            case JOIN_LEFT_OUTER:
                output = new String[] {LEFT, SPACE, OUTER, SPACE, JOIN};
                break;
            case JOIN_RIGHT_OUTER:
                output = new String[] {RIGHT, SPACE, OUTER, SPACE, JOIN};
                break;
            case JOIN_SEMI:
                output = new String[] {"SEMI", SPACE, JOIN}; //$NON-NLS-1$
                break;
            case JOIN_UNION:
                output = new String[] {UNION, SPACE, JOIN};
                break;
            default:
                throw new AssertionError();
        }

        for (String part : output) {
            append(part);
        }
    }

    @Override
    public void visit(MatchCriteriaImpl obj) {
        visitNode(obj.getLeftExpression());

        append(SPACE);
        if (obj.isNegated()) {
            append(NOT);
            append(SPACE);
        }
        switch (obj.getMode()) {
            case SIMILAR:
                append(SIMILAR);
                append(SPACE);
                append(TO);
                break;
            case LIKE:
                append(LIKE);
                break;
            case REGEX:
                append(LIKE_REGEX);
                break;
        }
        append(SPACE);

        visitNode(obj.getRightExpression());

        if (obj.getEscapeChar() != MatchCriteriaImpl.NULL_ESCAPE_CHAR) {
            append(SPACE);
            append(ESCAPE);
            if (isTeiid8OrGreater()) {
                append(SPACE);
                outputLiteral(String.class, false, obj.getEscapeChar());
            } else {
                append(" '"); //$NON-NLS-1$
                append(String.valueOf(obj.getEscapeChar()));
                append("'"); //$NON-NLS-1$
            }
        }
    }

    @Override
    public void visit(NotCriteriaImpl obj) {
        append(NOT);
        append(" ("); //$NON-NLS-1$
        visitNode(obj.getCriteria());
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(OptionImpl obj) {
        append(OPTION);

        Collection<String> groups = obj.getDependentGroups();
        if (groups != null && groups.size() > 0) {
            append(" "); //$NON-NLS-1$
            append(MAKEDEP);
            append(" "); //$NON-NLS-1$

            Iterator<String> iter = groups.iterator();
            Iterator<MakeDep> iter1 = obj.getMakeDepOptions().iterator();

            while (iter.hasNext()) {
                outputDisplayName(iter.next());
                
                appendMakeDepOptions(iter1.next());

                if (iter.hasNext()) {
                    append(", ");//$NON-NLS-1$
                }
            }
        }

        groups = obj.getNotDependentGroups();
        if (groups != null && groups.size() > 0) {
            append(" "); //$NON-NLS-1$
            append(MAKENOTDEP);
            append(" "); //$NON-NLS-1$

            Iterator<String> iter = groups.iterator();

            while (iter.hasNext()) {
                outputDisplayName(iter.next());

                if (iter.hasNext()) {
                    append(", ");//$NON-NLS-1$
                }
            }
        }

        groups = obj.getNoCacheGroups();
        if (groups != null && groups.size() > 0) {
            append(" "); //$NON-NLS-1$
            append(NOCACHE);
            append(" "); //$NON-NLS-1$

            Iterator<String> iter = groups.iterator();

            while (iter.hasNext()) {
                outputDisplayName(iter.next());

                if (iter.hasNext()) {
                    append(", ");//$NON-NLS-1$
                }
            }
        } else if (obj.isNoCache()) {
            append(" "); //$NON-NLS-1$
            append(NOCACHE);
        }

    }

    /**
     * @param makedep
     * @return this visitor
     */
    public SQLStringVisitorImpl appendMakeDepOptions(MakeDep makedep) {
        if (isLessThanTeiidVersion(Version.TEIID_8_5))
            return this;

        boolean parens = false;
        if (makedep.getMax() != null || makedep.isJoin()) {
            append(Tokens.LPAREN);
            parens = true;
        }
        boolean space = false;
        if (makedep.getMax() != null) {
            if (space) {
                append(SPACE);
            } else {
                space = true;
            }
            append(NonReserved.MAX);
            append(Tokens.COLON);
            append(makedep.getMax());
        }
        if (makedep.isJoin()) {
            if (space) {
                append(SPACE);
            } else {
                space = true;
            }
            append(JOIN);
        }
        if (parens) {
            append(Tokens.RPAREN);
        }
        return this;
    }

    @Override
    public void visit(OrderByImpl obj) {
        append(ORDER);
        append(SPACE);
        append(BY);
        append(SPACE);
        registerNodes(obj.getOrderByItems(), 0);
    }

    @Override
    public void visit(OrderByItemImpl obj) {
        BaseExpression ses = obj.getSymbol();
        if (ses instanceof AliasSymbolImpl) {
            AliasSymbolImpl as = (AliasSymbolImpl)ses;
            outputDisplayName(as.getOutputName());
        } else {
            visitNode(ses);
        }
        if (!obj.isAscending()) {
            append(SPACE);
            append(DESC);
        } // Don't print default "ASC"
        if (obj.getNullOrdering() != null) {
            append(SPACE);
            append(NonReserved.NULLS);
            append(SPACE);
            append(obj.getNullOrdering().name());
        }
    }

    @Override
    public void visit(DynamicCommandImpl obj) {
        append(EXECUTE);
        append(SPACE);
        append(IMMEDIATE);
        append(SPACE);
        visitNode(obj.getSql());

        if (obj.isAsClauseSet()) {
            beginClause(1);
            append(AS);
            append(SPACE);
            for (int i = 0; i < obj.getAsColumns().size(); i++) {
                ElementSymbolImpl symbol = (ElementSymbolImpl)obj.getAsColumns().get(i);
                outputShortName(symbol);
                append(SPACE);
                append(getDataTypeManager().getDataTypeName(symbol.getType()));
                if (i < obj.getAsColumns().size() - 1) {
                    append(", "); //$NON-NLS-1$
                }
            }
        }

        if (obj.getIntoGroup() != null) {
            beginClause(1);
            append(INTO);
            append(SPACE);
            visitNode(obj.getIntoGroup());
        }

        if (obj.getUsing() != null && !obj.getUsing().isEmpty()) {
            beginClause(1);
            append(USING);
            append(SPACE);
            visitNode(obj.getUsing());
        }

        if (obj.getUpdatingModelCount() > 0) {
            beginClause(1);
            append(UPDATE);
            append(SPACE);
            if (obj.getUpdatingModelCount() > 1) {
                append("*"); //$NON-NLS-1$
            } else {
                append("1"); //$NON-NLS-1$
            }
        }
    }

    @Override
    public void visit(SetClauseListImpl obj) {
        for (Iterator<SetClauseImpl> iterator = obj.getClauses().iterator(); iterator.hasNext();) {
            SetClauseImpl clause = iterator.next();
            visitNode(clause);
            if (iterator.hasNext()) {
                append(", "); //$NON-NLS-1$
            }
        }
    }

    @Override
    public void visit(SetClauseImpl obj) {
        ElementSymbolImpl symbol = obj.getSymbol();
        outputShortName(symbol);
        append(" = "); //$NON-NLS-1$
        visitNode(obj.getValue());
    }

    @Override
    public void visit(WithQueryCommandImpl obj) {
        visitNode(obj.getGroupSymbol());
        append(SPACE);
        if (obj.getColumns() != null && !obj.getColumns().isEmpty()) {
            append(Tokens.LPAREN);
            setShortNameOnly(true);
            registerNodes(obj.getColumns(), 0);
            setShortNameOnly(false);
            append(Tokens.RPAREN);
            append(SPACE);
        }
        append(AS);
        append(SPACE);
        append(Tokens.LPAREN);
        if (isTeiidVersionOrGreater(Version.TEIID_8_5) && obj.getCommand() == null) {
            append("<dependent values>"); //$NON-NLS-1$
        } else {
            visitNode(obj.getCommand());
        }
        append(Tokens.RPAREN);
    }

    @Override
    public void visit(QueryImpl obj) {
        addWithClause(obj);
        append(SELECT);

        SourceHintImpl sh = obj.getSourceHint();
        addSourceHint(sh);
        if (obj.getSelect() != null) {
            visitNode(obj.getSelect());
        }

        if (obj.getInto() != null) {
            beginClause(1);
            visitNode(obj.getInto());
        }

        if (obj.getFrom() != null) {
            beginClause(1);
            visitNode(obj.getFrom());
        }

        // Where clause
        if (obj.getCriteria() != null) {
            beginClause(1);
            visitCriteria(WHERE, obj.getCriteria());
        }

        // Group by clause
        if (obj.getGroupBy() != null) {
            beginClause(1);
            visitNode(obj.getGroupBy());
        }

        // Having clause
        if (obj.getHaving() != null) {
            beginClause(1);
            visitCriteria(HAVING, obj.getHaving());
        }

        // Order by clause
        if (obj.getOrderBy() != null) {
            beginClause(1);
            visitNode(obj.getOrderBy());
        }

        if (obj.getLimit() != null) {
            beginClause(1);
            visitNode(obj.getLimit());
        }

        // Option clause
        if (obj.getOption() != null) {
            beginClause(1);
            visitNode(obj.getOption());
        }
    }

    private void addSourceHint(SourceHintImpl sh) {
        if (sh != null) {
            append(SPACE);
            append(BEGIN_HINT);
            append("sh"); //$NON-NLS-1$

            if (isTeiid8OrGreater() && sh.isUseAliases()) {
                append(SPACE);
                append("KEEP ALIASES"); //$NON-NLS-1$
            }

            if (sh.getGeneralHint() != null) {
                appendSourceHintValue(sh.getGeneralHint());
        	} else {
        		append(SPACE);
            }
            if (sh.getSpecificHints() != null) {
                for (Map.Entry<String, SpecificHint> entry : sh.getSpecificHints().entrySet()) {
                    append(entry.getKey());
                    if (isTeiid8OrGreater() && entry.getValue().isUseAliases()) {
                        append(SPACE);
                        append("KEEP ALIASES"); //$NON-NLS-1$
                    }

                    appendSourceHintValue(entry.getValue().getHint());
                }
            }
            append(END_HINT);
        }
    }

    private void addWithClause(QueryCommandImpl obj) {
        if (obj.getWith() != null) {
            append(WITH);
            append(SPACE);
            registerNodes(obj.getWith(), 0);
            beginClause(0);
        }
    }

    protected void visitCriteria(String keyWord, CriteriaImpl crit) {
        append(keyWord);
        append(SPACE);
        visitNode(crit);
    }

    @Override
    public void visit(SearchedCaseExpressionImpl obj) {
        append(CASE);
        for (int i = 0; i < obj.getWhenCount(); i++) {
            append(SPACE);
            append(WHEN);
            append(SPACE);
            visitNode(obj.getWhenCriteria(i));
            append(SPACE);
            append(THEN);
            append(SPACE);
            visitNode(obj.getThenExpression(i));
        }
        append(SPACE);
        if (obj.getElseExpression() != null) {
            append(ELSE);
            append(SPACE);
            visitNode(obj.getElseExpression());
            append(SPACE);
        }
        append(END);
    }

    @Override
    public void visit(SelectImpl obj) {
        if (obj.isDistinct()) {
            append(SPACE);
            append(DISTINCT);
        }
        beginClause(2);

        Iterator<BaseExpression> iter = obj.getSymbols().iterator();
        while (iter.hasNext()) {
            BaseExpression symbol = iter.next();
            visitNode(symbol);
            if (iter.hasNext()) {
                append(", "); //$NON-NLS-1$
            }
        }
    }

    private void appendSourceHintValue(String sh) {
        append(Tokens.COLON);
        append('\'');
        append(escapeStringValue(sh, "'")); //$NON-NLS-1$
        append('\'');
        append(SPACE);
    }

    @Override
    public void visit(SetCriteriaImpl obj) {
        // variable

        if (isTeiid8OrGreater())
            appendNested(obj.getExpression());
        else
            visitNode(obj.getExpression());

        // operator and beginning of list
        append(SPACE);
        if (obj.isNegated()) {
            append(NOT);
            append(SPACE);
        }
        append(IN);
        append(" ("); //$NON-NLS-1$

        // value list
        Collection vals = obj.getValues();
        int size = vals.size();
        if (size == 1) {
            Iterator iter = vals.iterator();
            BaseExpression expr = (BaseExpression)iter.next();
            visitNode(expr);
        } else if (size > 1) {
            Iterator iter = vals.iterator();
            BaseExpression expr = (BaseExpression)iter.next();
            visitNode(expr);
            while (iter.hasNext()) {
                expr = (BaseExpression)iter.next();
                append(", "); //$NON-NLS-1$
                visitNode(expr);
            }
        }
        append(")"); //$NON-NLS-1$
    }

    /**
     * Condition operators have lower precedence than LIKE/SIMILAR/IS
     * @param ex
     */
    @Since(Version.TEIID_8_0)
    private void appendNested(BaseExpression ex) {
        boolean useParens = ex instanceof CriteriaImpl;
        if (useParens) {
            append(Tokens.LPAREN);
        }
        visitNode(ex);
        if (useParens) {
            append(Tokens.RPAREN);
        }
    }

    @Override
    public void visit(SetQueryImpl obj) {
        addWithClause(obj);
        QueryCommandImpl query = obj.getLeftQuery();
        appendSetQuery(obj, query, false);

        beginClause(0);
        append(obj.getOperation());

        if (obj.isAll()) {
            append(SPACE);
            append(ALL);
        }
        beginClause(0);
        query = obj.getRightQuery();
        appendSetQuery(obj, query, true);

        if (obj.getOrderBy() != null) {
            beginClause(0);
            visitNode(obj.getOrderBy());
        }

        if (obj.getLimit() != null) {
            beginClause(0);
            visitNode(obj.getLimit());
        }

        if (obj.getOption() != null) {
            beginClause(0);
            visitNode(obj.getOption());
        }
    }

    protected void appendSetQuery(SetQueryImpl parent, QueryCommandImpl obj, boolean right) {
        if (obj.getLimit() != null
            || obj.getOrderBy() != null
            || (right && ((obj instanceof SetQueryImpl && ((parent.isAll() && !((SetQueryImpl)obj).isAll()) || parent.getOperation() != ((SetQueryImpl)obj).getOperation()))))) {
            append(Tokens.LPAREN);
            visitNode(obj);
            append(Tokens.RPAREN);
        } else {
            visitNode(obj);
        }
    }

    @Override
    public void visit(StoredProcedureImpl obj) {
        if (obj.isCalledWithReturn()) {
            for (SPParameterImpl param : obj.getParameters()) {
                if (param.getParameterType() == SPParameterImpl.RETURN_VALUE) {
                    if (param.getExpression() == null) {
                        append("?"); //$NON-NLS-1$
                    } else {
                        visitNode(param.getExpression());
                    }
                }
            }
            append(SPACE);
            append(Tokens.EQ);
            append(SPACE);
        }
        // exec clause
        append(EXEC);
        append(SPACE);
        append(obj.getProcedureName());
        append("("); //$NON-NLS-1$
        boolean first = true;
        for (SPParameterImpl param : obj.getParameters()) {
            if (param.isUsingDefault() || param.getParameterType() == SPParameterImpl.RETURN_VALUE
                || param.getParameterType() == SPParameterImpl.RESULT_SET || param.getExpression() == null) {
                continue;
            }
            if (first) {
                first = false;
            } else {
                append(", "); //$NON-NLS-1$
            }
            if (obj.isDisplayNamedParameters()) {
                append(escapeSinglePart(SymbolImpl.getShortName(param.getParameterSymbol().getOutputName())));
                append(" => "); //$NON-NLS-1$
            }

            boolean addParens = !obj.isDisplayNamedParameters() && param.getExpression() instanceof CompareCriteriaImpl;
            if (addParens) {
                append(Tokens.LPAREN);
            }
            visitNode(param.getExpression());
            if (addParens) {
                append(Tokens.RPAREN);
            }
        }
        append(")"); //$NON-NLS-1$

        // Option clause
        if (obj.getOption() != null) {
            beginClause(1);
            visitNode(obj.getOption());
        }
    }

    @Override
    public void visit(SubqueryFromClauseImpl obj) {
        addHintComment(obj);
        if (obj.isTable()) {
            append(TABLE);
        }
        append("(");//$NON-NLS-1$
        visitNode(obj.getCommand());
        append(")");//$NON-NLS-1$
        append(" AS ");//$NON-NLS-1$

        GroupSymbolImpl groupSymbol = obj.getGroupSymbol();
        if (isTeiid8OrGreater())
            append(escapeSinglePart(groupSymbol.getOutputName()));
        else
            append(groupSymbol.getOutputName());

        addMakeDep(obj);
    }

    @Override
    public void visit(SubquerySetCriteriaImpl obj) {
        // variable
        visitNode(obj.getExpression());

        // operator and beginning of list
        append(SPACE);
        if (obj.isNegated()) {
            append(NOT);
            append(SPACE);
        }
        append(IN);
        addSubqueryHint(obj.getSubqueryHint());
        append(" ("); //$NON-NLS-1$
        visitNode(obj.getCommand());
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(UnaryFromClauseImpl obj) {
        addHintComment(obj);
        visitNode(obj.getGroup());
        addMakeDep(obj);
    }

    @Override
    public void visit(UpdateImpl obj) {
        // Update clause
        append(UPDATE);
        addSourceHint(obj.getSourceHint());
        append(SPACE);
        visitNode(obj.getGroup());
        beginClause(1);
        // Set clause
        append(SET);
        beginClause(2);
        visitNode(obj.getChangeList());

        // Where clause
        if (obj.getCriteria() != null) {
            beginClause(1);
            visitCriteria(WHERE, obj.getCriteria());
        }

        // Option clause
        if (obj.getOption() != null) {
            beginClause(1);
            visitNode(obj.getOption());
        }
    }

    @Override
    public void visit(IntoImpl obj) {
        append(INTO);
        append(SPACE);
        visitNode(obj.getGroup());
    }

    // ############ Visitor methods for symbol objects ####################

    @Override
    public void visit(BaseAggregateSymbol obj) {
        if (isTeiid8OrGreater())
            append(obj.getName());
        else
            append(obj.getAggregateFunction().name());
        append("("); //$NON-NLS-1$

        if (obj.isDistinct()) {
            append(DISTINCT);
            append(" "); //$NON-NLS-1$
        } else if (isTeiid8OrGreater() && obj.getAggregateFunction() == Type.USER_DEFINED) {
            append(ALL);
            append(" "); //$NON-NLS-1$
        }

        if ((!isTeiid8OrGreater() && obj.getExpression() == null) ||
             (isTeiid8OrGreater() && (obj.getArgs() == null ||  obj.getArgs().length == 0))) {
            if (obj.getAggregateFunction() == Type.COUNT) {
                append(Tokens.ALL_COLS);
            }
        } else if (isTeiid8OrGreater()) {
            registerNodes(obj.getArgs(), 0);
        } else {
            visitNode(obj.getExpression());
        }

        if (obj.getOrderBy() != null) {
            append(SPACE);
            visitNode(obj.getOrderBy());
        }
        append(")"); //$NON-NLS-1$

        if (obj.getCondition() != null) {
            append(SPACE);
            append(FILTER);
            append(Tokens.LPAREN);
            append(WHERE);
            append(SPACE);
            append(obj.getCondition());
            append(Tokens.RPAREN);
        }
    }

    @Override
    public void visit(AliasSymbolImpl obj) {
        visitNode(obj.getSymbol());
        append(SPACE);
        append(AS);
        append(SPACE);
        append(escapeSinglePart(obj.getOutputName()));
    }

    @Override
    public void visit(MultipleElementSymbolImpl obj) {
        if (obj.getGroup() == null) {
            append(Tokens.ALL_COLS);
        } else {
            visitNode(obj.getGroup());
            append(Tokens.DOT);
            append(Tokens.ALL_COLS);
        }
    }

    private void visit7(ConstantImpl obj) {
        Class<?> type = obj.getType();
        String[] constantParts = null;
        if (obj.isMultiValued()) {
            constantParts = new String[] {"?"}; //$NON-NLS-1$
        } else if (obj.getValue() == null) {
            if (type.equals(DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass())) {
                constantParts = new String[] {UNKNOWN};
            } else {
                constantParts = new String[] {"null"}; //$NON-NLS-1$
            }
        } else {
            if (Number.class.isAssignableFrom(type)) {
                constantParts = new String[] {obj.getValue().toString()};
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass())) {
                constantParts = new String[] {obj.getValue().equals(Boolean.TRUE) ? TRUE : FALSE};
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.TIMESTAMP.getTypeClass())) {
                constantParts = new String[] {"{ts'", obj.getValue().toString(), "'}"}; //$NON-NLS-1$ //$NON-NLS-2$
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.TIME.getTypeClass())) {
                constantParts = new String[] {"{t'", obj.getValue().toString(), "'}"}; //$NON-NLS-1$ //$NON-NLS-2$
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.DATE.getTypeClass())) {
                constantParts = new String[] {"{d'", obj.getValue().toString(), "'}"}; //$NON-NLS-1$ //$NON-NLS-2$
            }
            if (constantParts == null) {
                String strValue = obj.getValue().toString();
                strValue = escapeStringValue(strValue, "'"); //$NON-NLS-1$
                constantParts = new String[] {"'", strValue, "'"}; //$NON-NLS-1$ //$NON-NLS-2$
            }
        }

        for (String string : constantParts) {
            append(string);
        }
    }

    private void outputLiteral(Class<?> type, boolean multiValued, Object value) throws AssertionError {
        String[] constantParts = null;
        if (multiValued) {
            constantParts = new String[] {"?"}; //$NON-NLS-1$
        } else if (value == null) {
            if (type.equals(DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass())) {
                constantParts = new String[] {UNKNOWN};
            } else {
                constantParts = new String[] {"null"}; //$NON-NLS-1$
            }
        } else {
			if (isTeiid87OrGreater() && value.getClass() == ArrayImpl.class) {
				ArrayImpl av = (ArrayImpl)value;
				append(Tokens.LPAREN);
				for (int i = 0; i < av.getValues().length; i++) {
					if (i > 0) {
						append(Tokens.COMMA);
						append(SPACE);
					}
					Object value2 = av.getValues()[i];
					outputLiteral(value2!=null?value2.getClass():av.getValues().getClass().getComponentType(), multiValued, value2);
				}
				append(Tokens.RPAREN);
				return;
			}
            if (Number.class.isAssignableFrom(type)) {
                constantParts = new String[] {value.toString()};
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass())) {
                constantParts = new String[] {value.equals(Boolean.TRUE) ? TRUE : FALSE};
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.TIMESTAMP.getTypeClass())) {
                constantParts = new String[] {"{ts'", value.toString(), "'}"}; //$NON-NLS-1$ //$NON-NLS-2$
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.TIME.getTypeClass())) {
                constantParts = new String[] {"{t'", value.toString(), "'}"}; //$NON-NLS-1$ //$NON-NLS-2$
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.DATE.getTypeClass())) {
                constantParts = new String[] {"{d'", value.toString(), "'}"}; //$NON-NLS-1$ //$NON-NLS-2$
            } else if (type.equals(DefaultDataTypeManager.DefaultDataTypes.VARBINARY.getTypeClass())) {
                constantParts = new String[] {"X'", value.toString(), "'"}; //$NON-NLS-1$ //$NON-NLS-2$
            }
            if (constantParts == null) {
                if (isTeiid8OrGreater() && DefaultDataTypeManager.DefaultDataTypes.isLOB(type)) {
                    constantParts = new String[] {"?"}; //$NON-NLS-1$
                } else {
                    String strValue = value.toString();
                    strValue = escapeStringValue(strValue, "'"); //$NON-NLS-1$
                    constantParts = new String[] {"'", strValue, "'"}; //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
        }

        for (String string : constantParts) {
            append(string);
        }
    }

    private void visit8(ConstantImpl obj) {
        Class<?> type = obj.getType();
        boolean multiValued = obj.isMultiValued();
        Object value = obj.getValue();
        outputLiteral(type, multiValued, value);
    }

    @Override
    public void visit(ConstantImpl obj) {
        if (isTeiid8OrGreater())
            visit8(obj);
        else
            visit7(obj);
    }

    /**
     * Take a string literal and escape it as necessary. By default, this converts ' to ''.
     * 
     * @param str String literal value (unquoted), never null
     * @return Escaped string literal value
     */
    static String escapeStringValue(String str, String tick) {
        return StringUtil.replaceAll(str, tick, tick + tick);
    }

    @Override
    public void visit(ElementSymbolImpl obj) {
        if (obj.getDisplayMode().equals(DisplayMode.SHORT_OUTPUT_NAME) ||isShortNameOnly()) {
            outputShortName(obj);
            return;
        }
        String name = obj.getOutputName();
        if (obj.getDisplayMode().equals(DisplayMode.FULLY_QUALIFIED)) {
            name = obj.getName();
        }
        outputDisplayName(name);
    }

    private void outputShortName(ElementSymbolImpl obj) {
        outputDisplayName(SymbolImpl.getShortName(obj.getOutputName()));
    }

    private void outputDisplayName(String name) {
        String[] pathParts = name.split("\\."); //$NON-NLS-1$
        for (int i = 0; i < pathParts.length; i++) {
            if (i > 0) {
                append(SymbolImpl.SEPARATOR);
            }
            append(escapeSinglePart(pathParts[i]));
        }
    }

    @Override
    public void visit(ExpressionSymbolImpl obj) {
        visitNode(obj.getExpression());
    }

    @Override
    public void visit(FunctionImpl obj) {
        String name = obj.getName();
        BaseExpression[] args = obj.getArgs();
        if (obj.isImplicit()) {
            // Hide this function, which is implicit
            visitNode(args[0]);

        } else if (name.equalsIgnoreCase(CONVERT) || name.equalsIgnoreCase(CAST)) {
            append(name);
            append("("); //$NON-NLS-1$

            if (args != null && args.length > 0) {
                visitNode(args[0]);

                if (name.equalsIgnoreCase(CONVERT)) {
                    append(", "); //$NON-NLS-1$
                } else {
                    append(" "); //$NON-NLS-1$
                    append(AS);
                    append(" "); //$NON-NLS-1$
                }

                if (args.length < 2 || args[1] == null || !(args[1] instanceof ConstantImpl)) {
                    append(UNDEFINED);
                } else {
                    append(((ConstantImpl)args[1]).getValue());
                }
            }
            append(")"); //$NON-NLS-1$

        } else if (name.equals("+") || name.equals("-") || name.equals("*") || name.equals("/") || name.equals("||")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            append("("); //$NON-NLS-1$

            if (args != null) {
                for (int i = 0; i < args.length; i++) {
                    visitNode(args[i]);
                    if (i < (args.length - 1)) {
                        append(SPACE);
                        append(name);
                        append(SPACE);
                    }
                }
            }
            append(")"); //$NON-NLS-1$

        } else if (name.equalsIgnoreCase(NonReserved.TIMESTAMPADD) || name.equalsIgnoreCase(NonReserved.TIMESTAMPDIFF)) {
            append(name);
            append("("); //$NON-NLS-1$

            if (args != null && args.length > 0) {
                append(((ConstantImpl)args[0]).getValue());
                registerNodes(args, 1);
            }
            append(")"); //$NON-NLS-1$

        } else if (name.equalsIgnoreCase(SourceSystemFunctions.XMLPI)) {
            append(name);
            append("(NAME "); //$NON-NLS-1$
            outputDisplayName((String)((ConstantImpl)args[0]).getValue());
            registerNodes(args, 1);
            append(")"); //$NON-NLS-1$
        } else if (name.equalsIgnoreCase(SourceSystemFunctions.TRIM)) {
            append(name);
            append(SQLConstants.Tokens.LPAREN);
            String value = (String)((ConstantImpl)args[0]).getValue();
            if (!value.equalsIgnoreCase(BOTH)) {
                append(((ConstantImpl)args[0]).getValue());
                append(" "); //$NON-NLS-1$
            }
            append(args[1]);
            append(" "); //$NON-NLS-1$
            append(FROM);
            append(" "); //$NON-NLS-1$
            append(args[2]);
            append(")"); //$NON-NLS-1$
        } else {
            append(name);
            append("("); //$NON-NLS-1$
            registerNodes(args, 0);
            append(")"); //$NON-NLS-1$
        }
    }

    private void registerNodes(BaseLanguageObject[] objects, int begin) {
        registerNodes(Arrays.asList(objects), begin);
    }

    private void registerNodes(List<? extends BaseLanguageObject> objects, int begin) {
        for (int i = begin; i < objects.size(); i++) {
            if (i > 0) {
                append(", "); //$NON-NLS-1$
            }
            visitNode(objects.get(i));
        }
    }

    @Override
    public void visit(GroupSymbolImpl obj) {
        String alias = null;
        String fullGroup = obj.getOutputName();
        if (obj.getOutputDefinition() != null) {
            alias = obj.getOutputName();
            fullGroup = obj.getOutputDefinition();
        }

        outputDisplayName(fullGroup);

        if (alias != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            append(escapeSinglePart(alias));
        }
    }

    @Override
    public void visit(ReferenceImpl obj) {
        if (!obj.isPositional() && obj.getExpression() != null) {
            visitNode(obj.getExpression());
        } else {
            append("?"); //$NON-NLS-1$
        }
    }

    // ############ Visitor methods for storedprocedure language objects ####################

    private void visit7(BlockImpl obj) {
        addLabel(obj);
        List<StatementImpl> statements = obj.getStatements();
        // Add first clause
        append(BEGIN);
        if (obj.isAtomic()) {
            append(SPACE);
            append(ATOMIC);
        }
        append("\n"); //$NON-NLS-1$
        Iterator<StatementImpl> stmtIter = statements.iterator();
        while (stmtIter.hasNext()) {
            // Add each statement
            addTabs(1);
            visitNode(stmtIter.next());
            append("\n"); //$NON-NLS-1$
        }
        addTabs(0);
        append(END);
    }

    private void addStatements(List<StatementImpl> statements) {
        Iterator<StatementImpl> stmtIter = statements.iterator();
        while (stmtIter.hasNext()) {
            // Add each statement
            addTabs(1);
            visitNode(stmtIter.next());
            append("\n"); //$NON-NLS-1$
        }
        addTabs(0);
    }

    private void visit8(BlockImpl obj) {
        addLabel(obj);
        List<StatementImpl> statements = obj.getStatements();
        // Add first clause
        append(BEGIN);
        if (obj.isAtomic()) {
            append(SPACE);
            append(ATOMIC);
        }
        append("\n"); //$NON-NLS-1$
        addStatements(statements);
        if (obj.getExceptionGroup() != null) {
            append(NonReserved.EXCEPTION);
            append(SPACE);
            outputDisplayName(obj.getExceptionGroup());
            append("\n"); //$NON-NLS-1$
            if (obj.getExceptionStatements() != null) {
                addStatements(obj.getExceptionStatements());
            }
        }
        append(END);
    }

    @Override
    public void visit(BlockImpl block) {
        if (isTeiid8OrGreater())
            visit8(block);
        else
            visit7(block);
    }

    private void addLabel(Labeled obj) {
        if (obj.getLabel() != null) {
            outputDisplayName(obj.getLabel());
            append(SPACE);
            append(Tokens.COLON);
            append(SPACE);
        }
    }

    /**
    * @param level  
    */
    protected void addTabs(int level) {
    }

    @Override
    public void visit(CommandStatementImpl obj) {
        visitNode(obj.getCommand());
        if (isTeiid8OrGreater() && !obj.isReturnable()) {
            append(SPACE);
            append(WITHOUT);
            append(SPACE);
            append(RETURN);
        }
        append(";"); //$NON-NLS-1$
    }

    @Override
    @Removed(Version.TEIID_8_0)
    public void visit(CreateUpdateProcedureCommandImpl obj) {
        append(CREATE);
        append(SPACE);
        if (!obj.isUpdateProcedure()) {
            append(VIRTUAL);
            append(SPACE);
        }
        append(PROCEDURE);
        append("\n"); //$NON-NLS-1$
        addTabs(0);
        visitNode(obj.getBlock());
    }

    @Override
    public void visit(CreateProcedureCommandImpl obj) {
        if (isLessThanTeiidVersion(Version.TEIID_8_4)) {
            append(CREATE);
            append(SPACE);
            append(VIRTUAL);
            append(SPACE);
            append(PROCEDURE);
            append("\n"); //$NON-NLS-1$
            addTabs(0);
        }
        visitNode(obj.getBlock());
    }

    @Override
    public void visit(DeclareStatementImpl obj) {
        append(DECLARE);
        append(SPACE);
        append(obj.getVariableType());
        append(SPACE);
        createAssignment(obj);
    }

    /**
     * @param obj
     * @param parts
     */
    private void createAssignment(AssignmentStatementImpl obj) {
        visitNode(obj.getVariable());
        if (obj.getExpression() != null) {
            append(" = "); //$NON-NLS-1$
            visitNode(obj.getExpression());
        }
        append(";"); //$NON-NLS-1$
    }

    @Override
    public void visit(IfStatementImpl obj) {
        append(IF);
        append("("); //$NON-NLS-1$
        visitNode(obj.getCondition());
        append(")\n"); //$NON-NLS-1$
        addTabs(0);
        visitNode(obj.getIfBlock());
        if (obj.hasElseBlock()) {
            append("\n"); //$NON-NLS-1$
            addTabs(0);
            append(ELSE);
            append("\n"); //$NON-NLS-1$
            addTabs(0);
            visitNode(obj.getElseBlock());
        }
    }

    @Override
    public void visit(AssignmentStatementImpl obj) {
        createAssignment(obj);
    }

    @Override
    public void visit(RaiseStatementImpl obj) {
        append(NonReserved.RAISE);
        append(SPACE);
        if (obj.isWarning()) {
            append(SQLWARNING);
            append(SPACE);
        }
        visitNode(obj.getExpression());
        append(";"); //$NON-NLS-1$
    }

    @Override
    public void visit(HasCriteriaImpl obj) {
        append(HAS);
        append(SPACE);
        visitNode(obj.getSelector());
    }

    @Override
    public void visit(TranslateCriteriaImpl obj) {
        append(TRANSLATE);
        append(SPACE);
        visitNode(obj.getSelector());

        if (obj.hasTranslations()) {
            append(SPACE);
            append(WITH);
            append(SPACE);
            append("("); //$NON-NLS-1$
            Iterator critIter = obj.getTranslations().iterator();

            while (critIter.hasNext()) {
                visitNode((CriteriaImpl)critIter.next());
                if (critIter.hasNext()) {
                    append(", "); //$NON-NLS-1$
                }
                if (!critIter.hasNext()) {
                    append(")"); //$NON-NLS-1$
                }
            }
        }
    }

    @Override
    public void visit(CriteriaSelectorImpl obj) {
        switch (obj.getSelectorType()) {
            case EQ:
                append("= "); //$NON-NLS-1$
                break;
            case GE:
                append(">= "); //$NON-NLS-1$
                break;
            case GT:
                append("> "); //$NON-NLS-1$
                break;
            case LE:
                append("<= "); //$NON-NLS-1$
                break;
            case LT:
                append("< "); //$NON-NLS-1$
                break;
            case NE:
                append("<> "); //$NON-NLS-1$
                break;
            case IN:
                append(IN);
                append(SPACE);
                break;
            case IS_NULL:
                append(IS);
                append(SPACE);
                append(NULL);
                append(SPACE);
                break;
            case LIKE:
                append(LIKE);
                append(SPACE);
                break;
            case BETWEEN:
                append(BETWEEN);
                append(SPACE);
                break;
            case NO_TYPE:
            default:
                // Append nothing
                break;
        }

        append(CRITERIA);
        if (obj.hasElements()) {
            append(SPACE);
            append(ON);
            append(SPACE);
            append("("); //$NON-NLS-1$

            Iterator elmtIter = obj.getElements().iterator();
            while (elmtIter.hasNext()) {
                visitNode((ElementSymbolImpl)elmtIter.next());
                if (elmtIter.hasNext()) {
                    append(", "); //$NON-NLS-1$
                }
            }
            append(")"); //$NON-NLS-1$
        }
    }

    @Override
    public void visit(RaiseErrorStatementImpl obj) {
        append(ERROR);
        append(SPACE);
        visitNode(obj.getExpression());
        append(";"); //$NON-NLS-1$
    }

    @Override
    public void visit(ExceptionExpressionImpl exceptionExpression) {
        append(SQLEXCEPTION);
        append(SPACE);
        visitNode(exceptionExpression.getMessage());
        if (exceptionExpression.getSqlState() != null) {
            append(SPACE);
            append(SQLSTATE);
            append(SPACE);
            append(exceptionExpression.getSqlState());
            if (exceptionExpression.getErrorCode() != null) {
                append(Tokens.COMMA);
                append(SPACE);
                append(exceptionExpression.getErrorCode());
            }
        }
        if (exceptionExpression.getParent() != null) {
            append(SPACE);
            append(NonReserved.CHAIN);
            append(SPACE);
            append(exceptionExpression.getParent());
        }
    }

    @Override
    public void visit(ReturnStatementImpl obj) {
        append(RETURN);
        if (obj.getExpression() != null) {
            append(SPACE);
            visitNode(obj.getExpression());
        }
        append(Tokens.SEMICOLON);
    }

    @Override
    public void visit(BranchingStatementImpl obj) {
        switch (obj.getMode()) {
            case CONTINUE:
                append(CONTINUE);
                break;
            case BREAK:
                append(BREAK);
                break;
            case LEAVE:
                append(LEAVE);
                break;
        }
        if (obj.getLabel() != null) {
            append(SPACE);
            outputDisplayName(obj.getLabel());
        }
        append(";"); //$NON-NLS-1$
    }

    @Override
    public void visit(LoopStatementImpl obj) {
        addLabel(obj);
        append(LOOP);
        append(" "); //$NON-NLS-1$
        append(ON);
        append(" ("); //$NON-NLS-1$
        visitNode(obj.getCommand());
        append(") "); //$NON-NLS-1$
        append(AS);
        append(" "); //$NON-NLS-1$
        if (isTeiid8OrGreater())
            outputDisplayName(obj.getCursorName());
        else
            append(obj.getCursorName());

        append("\n"); //$NON-NLS-1$
        addTabs(0);
        visitNode(obj.getBlock());
    }

    @Override
    public void visit(WhileStatementImpl obj) {
        addLabel(obj);
        append(WHILE);
        append("("); //$NON-NLS-1$
        visitNode(obj.getCondition());
        append(")\n"); //$NON-NLS-1$
        addTabs(0);
        visitNode(obj.getBlock());
    }

    @Override
    public void visit(ExistsCriteriaImpl obj) {
        if (obj.isNegated()) {
            append(NOT);
            append(SPACE);
        }
        append(EXISTS);
        addSubqueryHint(obj.getSubqueryHint());
        append(" ("); //$NON-NLS-1$
        visitNode(obj.getCommand());
        append(")"); //$NON-NLS-1$
    }

    private void addSubqueryHint(SubqueryHint hint) {
        if (hint.isNoUnnest()) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(SubqueryHint.NOUNNEST);
            append(SPACE);
            append(END_HINT);
        } else if (hint.isDepJoin()) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(SubqueryHint.DJ);
            append(SPACE);
            append(END_HINT);
        } else if (hint.isMergeJoin()) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(SubqueryHint.MJ);
            append(SPACE);
            append(END_HINT);
        }
    }

    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
        BaseExpression leftExpression = obj.getLeftExpression();
        visitNode(leftExpression);

        String operator = obj.getOperatorAsString();
        String quantifier = obj.getPredicateQuantifierAsString();

        // operator and beginning of list
        append(SPACE);
        append(operator);
        append(SPACE);
        append(quantifier);
        append("("); //$NON-NLS-1$
        visitNode(obj.getCommand());
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(ScalarSubqueryImpl obj) {
        // operator and beginning of list
        append("("); //$NON-NLS-1$
        visitNode(obj.getCommand());
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(XMLAttributesImpl obj) {
        append(XMLATTRIBUTES);
        append("("); //$NON-NLS-1$
        registerNodes(obj.getArgs(), 0);
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(XMLElementImpl obj) {
        append(XMLELEMENT);
        append("(NAME "); //$NON-NLS-1$
        outputDisplayName(obj.getName());
        if (obj.getNamespaces() != null) {
            append(", "); //$NON-NLS-1$
            visitNode(obj.getNamespaces());
        }
        if (obj.getAttributes() != null) {
            append(", "); //$NON-NLS-1$
            visitNode(obj.getAttributes());
        }
        if (!obj.getContent().isEmpty()) {
            append(", "); //$NON-NLS-1$
        }
        registerNodes(obj.getContent(), 0);
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(XMLForestImpl obj) {
        append(XMLFOREST);
        append("("); //$NON-NLS-1$
        if (obj.getNamespaces() != null) {
            visitNode(obj.getNamespaces());
            append(", "); //$NON-NLS-1$
        }
        registerNodes(obj.getArgs(), 0);
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(JSONObjectImpl obj) {
        append(NonReserved.JSONOBJECT);
        append("("); //$NON-NLS-1$
        registerNodes(obj.getArgs(), 0);
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(TextLineImpl obj) {
        append(FOR);
        append(SPACE);
        registerNodes(obj.getExpressions(), 0);

        if (obj.getDelimiter() != null) {
            append(SPACE);
            append(NonReserved.DELIMITER);
            append(SPACE);
            visitNode(newConstant(obj.getDelimiter()));
        }
        if (obj.getQuote() != null) {
            append(SPACE);
            append(NonReserved.QUOTE);
            append(SPACE);
            visitNode(newConstant(obj.getQuote()));
        }
        if (obj.isIncludeHeader()) {
            append(SPACE);
            append(NonReserved.HEADER);
        }
        if (obj.getEncoding() != null) {
            append(SPACE);
            append(NonReserved.ENCODING);
            append(SPACE);
            outputDisplayName(obj.getEncoding());
        }
    }

    @Override
    public void visit(XMLNamespacesImpl obj) {
        append(XMLNAMESPACES);
        append("("); //$NON-NLS-1$
        for (Iterator<NamespaceItem> items = obj.getNamespaceItems().iterator(); items.hasNext();) {
            NamespaceItem item = items.next();
            if (item.getPrefix() == null) {
                if (item.getUri() == null) {
                    append("NO DEFAULT"); //$NON-NLS-1$
                } else {
                    append("DEFAULT "); //$NON-NLS-1$
                    visitNode(newConstant(item.getUri()));
                }
            } else {
                visitNode(newConstant(item.getUri()));
                append(" AS "); //$NON-NLS-1$
                outputDisplayName(item.getPrefix());
            }
            if (items.hasNext()) {
                append(", "); //$NON-NLS-1$
            }
        }
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(LimitImpl obj) {
        if (!obj.isStrict()) {
            append(BEGIN_HINT);
            append(SPACE);
            append(LimitImpl.NON_STRICT);
            append(SPACE);
            append(END_HINT);
            append(SPACE);
        }
        if (obj.getRowLimit() == null) {
            append(OFFSET);
            append(SPACE);
            visitNode(obj.getOffset());
            append(SPACE);
            append(ROWS);
            return;
        }
        append(LIMIT);
        if (obj.getOffset() != null) {
            append(SPACE);
            visitNode(obj.getOffset());
            append(","); //$NON-NLS-1$
        }
        append(SPACE);
        visitNode(obj.getRowLimit());
    }

    @Override
    public void visit(TextTableImpl obj) {
        addHintComment(obj);
        append("TEXTTABLE("); //$NON-NLS-1$
        visitNode(obj.getFile());
        if (isTeiid8OrGreater() && obj.getSelector() != null) {
            append(SPACE);
            append(NonReserved.SELECTOR);
            append(SPACE);
            append(escapeSinglePart(obj.getSelector()));
        }
        append(SPACE);
        append(NonReserved.COLUMNS);

        for (Iterator<TextColumnImpl> cols = obj.getColumns().iterator(); cols.hasNext();) {
            TextColumnImpl col = cols.next();
            append(SPACE);
            outputDisplayName(col.getName());
            append(SPACE);
            if (col.isOrdinal()) {
                // Will only ever come in here is Teiid 8.7 or greater
                append(FOR);
                append(SPACE);
                append(NonReserved.ORDINALITY);
            } else {
                append(col.getType());
                if (col.getWidth() != null) {
                    append(SPACE);
                    append(NonReserved.WIDTH);
                    append(SPACE);
                    append(col.getWidth());
                }
                if (col.isNoTrim()) {
                    append(SPACE);
                    append(NO);
                    append(SPACE);
                    append(NonReserved.TRIM);
                }
                if (isTeiid8OrGreater() && col.getSelector() != null) {
                    append(SPACE);
                    append(NonReserved.SELECTOR);
                    append(SPACE);
                    append(escapeSinglePart(col.getSelector()));
                    append(SPACE);
                    append(col.getPosition());
                }
            }
            if (cols.hasNext()) {
                append(","); //$NON-NLS-1$
            }
        }
        if (!obj.isUsingRowDelimiter()) {
            append(SPACE);
            append(NO);
            append(SPACE);
            append(ROW);
            append(SPACE);
            append(NonReserved.DELIMITER);
        }
        if (obj.getDelimiter() != null) {
            append(SPACE);
            append(NonReserved.DELIMITER);
            append(SPACE);
            visitNode(newConstant(obj.getDelimiter()));
        }
        if (obj.getQuote() != null) {
            append(SPACE);
            if (obj.isEscape()) {
                append(ESCAPE);
            } else {
                append(NonReserved.QUOTE);
            }
            append(SPACE);
            visitNode(newConstant(obj.getQuote()));
        }
        if (obj.getHeader() != null) {
            append(SPACE);
            append(NonReserved.HEADER);
            if (1 != obj.getHeader()) {
                append(SPACE);
                append(obj.getHeader());
            }
        }
        if (obj.getSkip() != null) {
            append(SPACE);
            append(NonReserved.SKIP);
            append(SPACE);
            append(obj.getSkip());
        }
        append(")");//$NON-NLS-1$
        append(SPACE);
        append(AS);
        append(SPACE);
        outputDisplayName(obj.getName());
        addMakeDep(obj);
    }

    @Override
    public void visit(XMLTableImpl obj) {
        addHintComment(obj);
        append("XMLTABLE("); //$NON-NLS-1$
        if (obj.getNamespaces() != null) {
            visitNode(obj.getNamespaces());
            append(","); //$NON-NLS-1$
            append(SPACE);
        }
        visitNode(newConstant(obj.getXquery()));
        if (!obj.getPassing().isEmpty()) {
            append(SPACE);
            append(NonReserved.PASSING);
            append(SPACE);
            registerNodes(obj.getPassing(), 0);
        }

        if ((isTeiid8OrGreater() && !obj.getColumns().isEmpty() && !obj.isUsingDefaultColumn())
            || (!isTeiid8OrGreater() && !obj.getColumns().isEmpty())) {
            append(SPACE);
            append(NonReserved.COLUMNS);
            for (Iterator<XMLColumnImpl> cols = obj.getColumns().iterator(); cols.hasNext();) {
                XMLColumnImpl col = cols.next();
                append(SPACE);
                outputDisplayName(col.getName());
                append(SPACE);
                if (col.isOrdinal()) {
                    append(FOR);
                    append(SPACE);
                    append(NonReserved.ORDINALITY);
                } else {
                    append(col.getType());
                    if (col.getDefaultExpression() != null) {
                        append(SPACE);
                        append(DEFAULT);
                        append(SPACE);
                        visitNode(col.getDefaultExpression());
                    }
                    if (col.getPath() != null) {
                        append(SPACE);
                        append(NonReserved.PATH);
                        append(SPACE);
                        visitNode(newConstant(col.getPath()));
                    }
                }
                if (cols.hasNext()) {
                    append(","); //$NON-NLS-1$
                }
            }
        }
        append(")");//$NON-NLS-1$
        append(SPACE);
        append(AS);
        append(SPACE);
        outputDisplayName(obj.getName());
        addMakeDep(obj);
    }

    @Override
    public void visit(ObjectTableImpl obj) {
        addHintComment(obj);
        append("OBJECTTABLE("); //$NON-NLS-1$
        if (obj.getScriptingLanguage() != null) {
            append(LANGUAGE);
            append(SPACE);
            visitNode(newConstant(obj.getScriptingLanguage()));
            append(SPACE);
        }
        visitNode(newConstant(obj.getRowScript()));
        if (!obj.getPassing().isEmpty()) {
            append(SPACE);
            append(NonReserved.PASSING);
            append(SPACE);
            registerNodes(obj.getPassing(), 0);
        }
        append(SPACE);
        append(NonReserved.COLUMNS);
        for (Iterator<ObjectColumnImpl> cols = obj.getColumns().iterator(); cols.hasNext();) {
            ObjectColumnImpl col = cols.next();
            append(SPACE);
            outputDisplayName(col.getName());
            append(SPACE);
            append(col.getType());
            append(SPACE);
            visitNode(newConstant(col.getPath()));
            if (col.getDefaultExpression() != null) {
                append(SPACE);
                append(DEFAULT);
                append(SPACE);
                visitNode(col.getDefaultExpression());
            }
            if (cols.hasNext()) {
                append(","); //$NON-NLS-1$
            }
        }
        append(")");//$NON-NLS-1$
        append(SPACE);
        append(AS);
        append(SPACE);
        outputDisplayName(obj.getName());
        addMakeDep(obj);
    }

    @Override
    public void visit(XMLQueryImpl obj) {
        append("XMLQUERY("); //$NON-NLS-1$
        if (obj.getNamespaces() != null) {
            visitNode(obj.getNamespaces());
            append(","); //$NON-NLS-1$
            append(SPACE);
        }
        visitNode(newConstant(obj.getXquery()));
        if (!obj.getPassing().isEmpty()) {
            append(SPACE);
            append(NonReserved.PASSING);
            append(SPACE);
            registerNodes(obj.getPassing(), 0);
        }
        if (obj.getEmptyOnEmpty() != null) {
            append(SPACE);
            if (obj.getEmptyOnEmpty()) {
                append(NonReserved.EMPTY);
            } else {
                append(NULL);
            }
            append(SPACE);
            append(ON);
            append(SPACE);
            append(NonReserved.EMPTY);
        }
        append(")");//$NON-NLS-1$
    }

    @Override
    public void visit(DerivedColumnImpl obj) {
        visitNode(obj.getExpression());
        if (obj.getAlias() != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            outputDisplayName(obj.getAlias());
        }
    }

    @Override
    public void visit(XMLSerializeImpl obj) {
        append(XMLSERIALIZE);
        append(Tokens.LPAREN);
        if (obj.getDocument() != null) {
            if (obj.getDocument()) {
                append(NonReserved.DOCUMENT);
            } else {
                append(NonReserved.CONTENT);
            }
            append(SPACE);
        }
        visitNode(obj.getExpression());
        if (obj.getTypeString() != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            append(obj.getTypeString());
        }
        if (isTeiid8OrGreater()) {
            if (obj.getEncoding() != null) {
                append(SPACE);
                append(NonReserved.ENCODING);
                append(SPACE);
                append(escapeSinglePart(obj.getEncoding()));
            }
            if (obj.getVersion() != null) {
                append(SPACE);
                append(NonReserved.VERSION);
                append(SPACE);
                append(newConstant(obj.getVersion()));
            }
            if (obj.getDeclaration() != null) {
                append(SPACE);
                if (obj.getDeclaration()) {
                    append(NonReserved.INCLUDING);
                } else {
                    append(NonReserved.EXCLUDING);
                }
                append(SPACE);
                append(NonReserved.XMLDECLARATION);
            }
        }
        append(Tokens.RPAREN);
    }

    @Override
    public void visit(QueryStringImpl obj) {
        append(NonReserved.QUERYSTRING);
        append("("); //$NON-NLS-1$
        visitNode(obj.getPath());
        if (!obj.getArgs().isEmpty()) {
            append(","); //$NON-NLS-1$
            append(SPACE);
            registerNodes(obj.getArgs(), 0);
        }
        append(")"); //$NON-NLS-1$
    }

    @Override
    public void visit(XMLParseImpl obj) {
        append(XMLPARSE);
        append(Tokens.LPAREN);
        if (obj.isDocument()) {
            append(NonReserved.DOCUMENT);
        } else {
            append(NonReserved.CONTENT);
        }
        append(SPACE);
        visitNode(obj.getExpression());
        if (obj.isWellFormed()) {
            append(SPACE);
            append(NonReserved.WELLFORMED);
        }
        append(Tokens.RPAREN);
    }

    @Override
    public void visit(ExpressionCriteriaImpl obj) {
        visitNode(obj.getExpression());
    }

    @Override
    public void visit(TriggerActionImpl obj) {
        append(FOR);
        append(SPACE);
        append(EACH);
        append(SPACE);
        append(ROW);
        append("\n"); //$NON-NLS-1$
        addTabs(0);
        visitNode(obj.getBlock());
    }

    @Override
    public void visit(ArrayTableImpl obj) {
        addHintComment(obj);
        append("ARRAYTABLE("); //$NON-NLS-1$
        visitNode(obj.getArrayValue());
        append(SPACE);
        append(NonReserved.COLUMNS);

        for (Iterator<ProjectedColumnImpl> cols = obj.getColumns().iterator(); cols.hasNext();) {
            ProjectedColumnImpl col = cols.next();
            append(SPACE);
            outputDisplayName(col.getName());
            append(SPACE);
            append(col.getType());
            if (cols.hasNext()) {
                append(","); //$NON-NLS-1$
            }
        }

        append(")");//$NON-NLS-1$
        append(SPACE);
        append(AS);
        append(SPACE);
        outputDisplayName(obj.getName());
        addMakeDep(obj);
    }

    private void addMakeDep(FromClauseImpl obj) {
        if (isLessThanTeiidVersion(Version.TEIID_8_5))
            return;

        MakeDep makeDep = obj.getMakeDep();
        if (makeDep != null && !makeDep.isSimple()) {
            append(SPACE);
            append(MAKEDEP);
            appendMakeDepOptions(makeDep);
        }
    }

    private void visit7(AlterProcedureImpl<CreateUpdateProcedureCommandImpl> obj) {
        append(ALTER);
        append(SPACE);
        append(PROCEDURE);
        append(SPACE);
        append(obj.getTarget());
        beginClause(1);
        append(AS);
        append(obj.getDefinition().getBlock());
    }

    private void visit8(AlterProcedureImpl<CreateProcedureCommandImpl> obj) {
        append(ALTER);
        append(SPACE);
        append(PROCEDURE);
        append(SPACE);
        append(obj.getTarget());
        beginClause(1);
        append(AS);
        append(obj.getDefinition().getBlock());
    }

    @Override
    public void visit(AlterProcedureImpl obj) {
        if (isTeiid8OrGreater())
            visit8(obj);
        else
            visit7(obj);
    }

    @Override
    public void visit(AlterTriggerImpl obj) {
        if (obj.isCreate()) {
            append(CREATE);
        } else {
            append(ALTER);
        }
        append(SPACE);
        append(TRIGGER);
        append(SPACE);
        append(ON);
        append(SPACE);
        append(obj.getTarget());
        beginClause(0);
        append(NonReserved.INSTEAD);
        append(SPACE);
        append(OF);
        append(SPACE);
        append(obj.getEvent());
        if (obj.getDefinition() != null) {
            beginClause(0);
            append(AS);
            append("\n"); //$NON-NLS-1$
            addTabs(0);
            append(obj.getDefinition());
        } else {
            append(SPACE);
            append(obj.getEnabled() ? NonReserved.ENABLED : NonReserved.DISABLED);
        }
    }

    @Override
    public void visit(AlterViewImpl obj) {
        append(ALTER);
        append(SPACE);
        append(NonReserved.VIEW);
        append(SPACE);
        append(obj.getTarget());
        beginClause(0);
        append(AS);
        append("\n"); //$NON-NLS-1$
        addTabs(0);
        append(obj.getDefinition());
    }

    @Override
    public void visit(BaseWindowFunction windowFunction) {
        append(windowFunction.getFunction());
        append(SPACE);
        append(OVER);
        append(SPACE);
        append(windowFunction.getWindowSpecification());
    }

    @Override
    public void visit(WindowSpecificationImpl windowSpecification) {
        append(Tokens.LPAREN);
        boolean needsSpace = false;
        if (windowSpecification.getPartition() != null) {
            append(PARTITION);
            append(SPACE);
            append(BY);
            append(SPACE);
            registerNodes(windowSpecification.getPartition(), 0);
            needsSpace = true;
        }
        if (windowSpecification.getOrderBy() != null) {
            if (needsSpace) {
                append(SPACE);
            }
            append(windowSpecification.getOrderBy());
        }
        append(Tokens.RPAREN);
    }

    @Override
    public void visit(ArraySymbolImpl array) {
        if (!array.isImplicit()) {
            append(Tokens.LPAREN);
        }
        registerNodes(array.getExpressions(), 0);
        if (!array.isImplicit()) {
    		if (array.getExpressions().size() == 1) {
    			append(Tokens.COMMA);
    		}
            append(Tokens.RPAREN);
        }
    }

    private String escapeSinglePart(String part) {
        if (isReservedWord(part)) {
            return ID_ESCAPE_CHAR + part + ID_ESCAPE_CHAR;
        }
        boolean escape = true;
        char start = part.charAt(0);
        if (start == '#' || start == '@' || StringUtil.isLetter(start)) {
            escape = false;
            for (int i = 1; !escape && i < part.length(); i++) {
                char c = part.charAt(i);
                escape = !StringUtil.isLetterOrDigit(c) && c != '_';
            }
        }
        if (escape) {
            return ID_ESCAPE_CHAR + escapeStringValue(part, "\"") + ID_ESCAPE_CHAR; //$NON-NLS-1$
        }
        return part;
    }

    /**
     * Check whether a string is considered a reserved word or not. Subclasses may override to change definition of reserved word.
     *
     * @param string String to check
     * @return True if reserved word
     */
    private boolean isReservedWord(String string) {
        if (string == null) {
            return false;
        }
        return SQLConstants.isReservedWord(getTeiidVersion(), string);
    }

}
