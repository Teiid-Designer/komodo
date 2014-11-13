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
package org.teiid.query.parser;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.komodo.spi.annotation.AnnotationUtils;
import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.AbstractLanguageVisitor;
import org.komodo.spi.query.sql.lang.AlterProcedure;
import org.komodo.spi.query.sql.lang.AlterTrigger;
import org.komodo.spi.query.sql.lang.AlterView;
import org.komodo.spi.query.sql.lang.ArrayTable;
import org.komodo.spi.query.sql.lang.BetweenCriteria;
import org.komodo.spi.query.sql.lang.CompareCriteria;
import org.komodo.spi.query.sql.lang.CompoundCriteria;
import org.komodo.spi.query.sql.lang.Create;
import org.komodo.spi.query.sql.lang.Delete;
import org.komodo.spi.query.sql.lang.DependentSetCriteria;
import org.komodo.spi.query.sql.lang.Drop;
import org.komodo.spi.query.sql.lang.DynamicCommand;
import org.komodo.spi.query.sql.lang.ExistsCriteria;
import org.komodo.spi.query.sql.lang.ExpressionCriteria;
import org.komodo.spi.query.sql.lang.From;
import org.komodo.spi.query.sql.lang.GroupBy;
import org.komodo.spi.query.sql.lang.Insert;
import org.komodo.spi.query.sql.lang.Into;
import org.komodo.spi.query.sql.lang.IsNullCriteria;
import org.komodo.spi.query.sql.lang.JoinPredicate;
import org.komodo.spi.query.sql.lang.JoinType;
import org.komodo.spi.query.sql.lang.LanguageObject;
import org.komodo.spi.query.sql.lang.Limit;
import org.komodo.spi.query.sql.lang.MatchCriteria;
import org.komodo.spi.query.sql.lang.NotCriteria;
import org.komodo.spi.query.sql.lang.ObjectTable;
import org.komodo.spi.query.sql.lang.Option;
import org.komodo.spi.query.sql.lang.OrderBy;
import org.komodo.spi.query.sql.lang.OrderByItem;
import org.komodo.spi.query.sql.lang.Query;
import org.komodo.spi.query.sql.lang.Select;
import org.komodo.spi.query.sql.lang.SetClause;
import org.komodo.spi.query.sql.lang.SetClauseList;
import org.komodo.spi.query.sql.lang.SetCriteria;
import org.komodo.spi.query.sql.lang.SetQuery;
import org.komodo.spi.query.sql.lang.StoredProcedure;
import org.komodo.spi.query.sql.lang.SubqueryCompareCriteria;
import org.komodo.spi.query.sql.lang.SubqueryFromClause;
import org.komodo.spi.query.sql.lang.SubquerySetCriteria;
import org.komodo.spi.query.sql.lang.TextTable;
import org.komodo.spi.query.sql.lang.UnaryFromClause;
import org.komodo.spi.query.sql.lang.Update;
import org.komodo.spi.query.sql.lang.WithQueryCommand;
import org.komodo.spi.query.sql.lang.XMLTable;
import org.komodo.spi.query.sql.proc.AssignmentStatement;
import org.komodo.spi.query.sql.proc.Block;
import org.komodo.spi.query.sql.proc.BranchingStatement;
import org.komodo.spi.query.sql.proc.CommandStatement;
import org.komodo.spi.query.sql.proc.CreateProcedureCommand;
import org.komodo.spi.query.sql.proc.CriteriaSelector;
import org.komodo.spi.query.sql.proc.DeclareStatement;
import org.komodo.spi.query.sql.proc.ExceptionExpression;
import org.komodo.spi.query.sql.proc.HasCriteria;
import org.komodo.spi.query.sql.proc.IfStatement;
import org.komodo.spi.query.sql.proc.LoopStatement;
import org.komodo.spi.query.sql.proc.RaiseStatement;
import org.komodo.spi.query.sql.proc.ReturnStatement;
import org.komodo.spi.query.sql.proc.TranslateCriteria;
import org.komodo.spi.query.sql.proc.TriggerAction;
import org.komodo.spi.query.sql.proc.WhileStatement;
import org.komodo.spi.query.sql.symbol.AggregateSymbol;
import org.komodo.spi.query.sql.symbol.AliasSymbol;
import org.komodo.spi.query.sql.symbol.Array;
import org.komodo.spi.query.sql.symbol.CaseExpression;
import org.komodo.spi.query.sql.symbol.Constant;
import org.komodo.spi.query.sql.symbol.DerivedColumn;
import org.komodo.spi.query.sql.symbol.ElementSymbol;
import org.komodo.spi.query.sql.symbol.ExpressionSymbol;
import org.komodo.spi.query.sql.symbol.Function;
import org.komodo.spi.query.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.symbol.MultipleElementSymbol;
import org.komodo.spi.query.sql.symbol.QueryString;
import org.komodo.spi.query.sql.symbol.Reference;
import org.komodo.spi.query.sql.symbol.ScalarSubquery;
import org.komodo.spi.query.sql.symbol.SearchedCaseExpression;
import org.komodo.spi.query.sql.symbol.TextLine;
import org.komodo.spi.query.sql.symbol.WindowFunction;
import org.komodo.spi.query.sql.symbol.WindowSpecification;
import org.komodo.spi.query.sql.symbol.XMLAttributes;
import org.komodo.spi.query.sql.symbol.XMLElement;
import org.komodo.spi.query.sql.symbol.XMLForest;
import org.komodo.spi.query.sql.symbol.XMLNamespaces;
import org.komodo.spi.query.sql.symbol.XMLParse;
import org.komodo.spi.query.sql.symbol.XMLQuery;
import org.komodo.spi.query.sql.symbol.XMLSerialize;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.AlterProcedureImpl;
import org.teiid.query.sql.lang.AlterTriggerImpl;
import org.teiid.query.sql.lang.AlterViewImpl;
import org.teiid.query.sql.lang.ArrayTableImpl;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CommandImpl;
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
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.FromClauseImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.HasCriteriaImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IntoImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.JoinPredicateImpl;
import org.teiid.query.sql.lang.JoinTypeImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.LimitImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NotCriteriaImpl;
import org.teiid.query.sql.lang.ObjectColumnImpl;
import org.teiid.query.sql.lang.ObjectTableImpl;
import org.teiid.query.sql.lang.OptionImpl;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.OrderByItemImpl;
import org.teiid.query.sql.lang.ProjectedColumnImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.QueryCommandImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.SetClauseListImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
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
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.ArraySymbolImpl;
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
import org.teiid.query.sql.symbol.TextLineImpl;
import org.teiid.query.sql.symbol.BaseWindowFunction;
import org.teiid.query.sql.symbol.WindowSpecificationImpl;
import org.teiid.query.sql.symbol.XMLAttributesImpl;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLForestImpl;
import org.teiid.query.sql.symbol.XMLNamespacesImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;

/**
 *
 */
@SuppressWarnings( "javadoc" )
public abstract class TCLanguageVisitorImpl extends AbstractLanguageVisitor {

    private final TeiidVersion teiidVersion;

    /*
     * Required if nodes are to be created by the visitor
     */
    private final TCQueryParser parser;

    private DefaultDataTypeManager dataTypeManager;

    private boolean abort = false;

    private static final Map<Class<?>, Method> methodCache = new HashMap<Class<?>, Method>();

    static {
        // cache all the methods on this visitor
        Method[] methods = TCLanguageVisitorImpl.class.getMethods();
        for (Method method : methods) {
            if (!method.getName().equals("visit")) //$NON-NLS-1$
                continue;

            Class<?>[] params = method.getParameterTypes();
            if (params == null || params.length == 0)
                continue;

            for (Class<?> param : params) {
                if (BaseLanguageObject.class.isAssignableFrom(param)) {
                    methodCache.put(param, method);
                }
            }
        }
    }

    /**
     * Construct new instance of visitor dependent on
     * teiid version.
     *
     * @param teiidVersion used to check visitor methods are
     *                                      applicable
     */
    public TCLanguageVisitorImpl(TeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
        this.parser = new TCQueryParser(teiidVersion);
    }

    /**
     * @return the teiidVersion
     */
    public TeiidVersion getTeiidVersion() {
        return this.teiidVersion;
    }

    protected boolean isTeiidVersionOrGreater(Version teiidVersion) {
        TeiidVersion minVersion = getTeiidVersion().getMinimumVersion();
        return minVersion.equals(teiidVersion.get()) || minVersion.isGreaterThan(teiidVersion.get());
    }

    protected boolean isLessThanTeiidVersion(Version teiidVersion) {
        TeiidVersion maxVersion = getTeiidVersion().getMaximumVersion();
        return maxVersion.isLessThan(teiidVersion.get());
    }

    protected boolean isTeiid8OrGreater() {
        return isTeiidVersionOrGreater(Version.TEIID_8_0);
    }

    protected boolean isTeiid87OrGreater() {
        return isTeiidVersionOrGreater(Version.TEIID_8_7);
    }

    

    /**
     * @return the parser
     */
    public TCQueryParser getQueryParser() {
        return this.parser;
    }

    protected TeiidClientParser getTeiidParser() {
        return parser.getTeiidParser();
    }

    protected DefaultDataTypeManager getDataTypeManager() {
        if (dataTypeManager == null)
            dataTypeManager = DefaultDataTypeManager.getInstance(getTeiidVersion());

        return dataTypeManager;
    }

    protected <T extends BaseLanguageObject> T createNode(ASTNodes nodeType) {
        return getTeiidParser().createASTNode(nodeType);
    }

    public void setAbort(boolean abort) {
        this.abort = abort;
    }
    
    public final boolean shouldAbort() {
        return abort;
    }

    private Method searchMethodCache(Class<?> methodClass) {
        Method method = methodCache.get(methodClass);
        if (method != null)
            return method;

        // Cannot find any method for this class but it could be a version
        // specific class such as Aggregate8Symbol so the class' interface
        Class<?>[] interfaces = methodClass.getInterfaces();
        if (interfaces == null)
            return null;

        for (Class<?> iface : interfaces) {
            method = methodCache.get(iface);
            if (method != null)
                return method;
        }

        // Cannot find any method of this class or its interfaces but it may be
        // a version specific class, such as Alter8Procedure with an abstract
        // superclass.
        Class<?> superClass = null;
        do {
           superClass = methodClass.getSuperclass();
           if (superClass != null) {
               method = methodCache.get(superClass);
               if (method != null)
                   return method;
           }
        } while (superClass != null);

        return null;
    }

    protected void isApplicable(LanguageObject node) {
        Method method = searchMethodCache(node.getClass());
        if (method == null) {
            throw new RuntimeException("No visit method for " + node.getClass()); //$NON-NLS-1$
        }

        String message = "The visit method " + method.toGenericString() + " is not applicable for teiid version " + teiidVersion; //$NON-NLS-1$ //$NON-NLS-2$
        if (AnnotationUtils.hasAnnotation(method, Removed.class)) {
            Removed removed = AnnotationUtils.getAnnotation(method, Removed.class);
            if (AnnotationUtils.isGreaterThanOrEqualTo(removed, teiidVersion)) {
                throw new RuntimeException(message);
            }
        }

        if (AnnotationUtils.hasAnnotation(method, Since.class)) {
            Since since = AnnotationUtils.getAnnotation(method, Since.class);
            if (!AnnotationUtils.isGreaterThanOrEqualTo(since, teiidVersion)) {
                throw new RuntimeException(message);
            }
        }
    }

    public void visit(BaseLanguageObject node) {
        isApplicable(node);
    }

    public void visit(CommandImpl node) {
        isApplicable(node);
    }

    public void visit(AlterViewImpl node) {
        isApplicable(node);
    }

    public void visit(AlterTriggerImpl node) {
        isApplicable(node);
    }

    public void visit(AlterProcedureImpl node) {
        isApplicable(node);
    }

    public void visit(TriggerActionImpl node) {
        isApplicable(node);
    }

    public void visit(DropImpl node) {
        isApplicable(node);
    }

    public void visit(CreateImpl node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(RaiseErrorStatementImpl node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(RaiseStatementImpl node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(ExceptionExpressionImpl node) {
        isApplicable(node);
    }

    public void visit(StatementImpl node) {
        isApplicable(node);
    }

    public void visit(BranchingStatementImpl node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(ReturnStatementImpl node) {
        isApplicable(node);
    }

    public void visit(WhileStatementImpl node) {
        isApplicable(node);
    }

    public void visit(LoopStatementImpl node) {
        isApplicable(node);
    }

    public void visit(IfStatementImpl node) {
        isApplicable(node);
    }

    public void visit(DeclareStatementImpl node) {
        isApplicable(node);
    }

    public void visit(CommandStatementImpl node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(CreateProcedureCommandImpl node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(CreateUpdateProcedureCommandImpl node) {
        isApplicable(node);
    }

    public void visit(DynamicCommandImpl node) {
        isApplicable(node);
    }

    public void visit(SetClauseListImpl node) {
        isApplicable(node);
    }

    public void visit(SetClauseImpl node) {
        isApplicable(node);
    }

    public void visit(ProjectedColumnImpl node) {
        isApplicable(node);
    }

    public void visit(StoredProcedureImpl node) {
        isApplicable(node);
    }

    public void visit(InsertImpl node) {
        isApplicable(node);
    }

    public void visit(UpdateImpl node) {
        isApplicable(node);
    }

    public void visit(DeleteImpl node) {
        isApplicable(node);
    }

    public void visit(QueryCommandImpl node) {
        isApplicable(node);
    }

    public void visit(WithQueryCommandImpl node) {
        isApplicable(node);
    }

    public void visit(SetQueryImpl node) {
        isApplicable(node);
    }

    public void visit(QueryImpl node) {
        isApplicable(node);
    }

    public void visit(IntoImpl node) {
        isApplicable(node);
    }

    public void visit(SelectImpl node) {
        isApplicable(node);
    }

    public void visit(ExpressionSymbolImpl node) {
        isApplicable(node);
    }

    public void visit(DerivedColumnImpl node) {
        isApplicable(node);
    }

    public void visit(MultipleElementSymbolImpl node) {
        isApplicable(node);
    }

    public void visit(FromImpl node) {
        isApplicable(node);
    }

    public void visit(FromClauseImpl node) {
        isApplicable(node);
    }

    public void visit(JoinPredicateImpl node) {
        isApplicable(node);
    }

    public void visit(JoinTypeImpl node) {
        isApplicable(node);
    }

    public void visit(XMLSerializeImpl node) {
        isApplicable(node);
    }

    public void visit(ArrayTableImpl node) {
        isApplicable(node);
    }

    public void visit(TextTableImpl node) {
        isApplicable(node);
    }

    public void visit(TextColumnImpl node) {
        isApplicable(node);
    }

    public void visit(XMLQueryImpl node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(ObjectTableImpl node) {
        isApplicable(node);
    }

    public void visit(ObjectColumnImpl node) {
        isApplicable(node);
    }

    public void visit(XMLTableImpl node) {
        isApplicable(node);
    }

    public void visit(XMLColumnImpl node) {
        isApplicable(node);
    }

    public void visit(SubqueryFromClauseImpl node) {
        isApplicable(node);
    }

    public void visit(UnaryFromClauseImpl node) {
        isApplicable(node);
    }

    public void visit(CriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(CompoundCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(NotCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(CompareCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(SubqueryCompareCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(MatchCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(BetweenCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(IsNullCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(SubquerySetCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(SetCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(ExistsCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(GroupByImpl node) {
        isApplicable(node);
    }

    public void visit(OrderByImpl node) {
        isApplicable(node);
    }

    public void visit(OrderByItemImpl node) {
        isApplicable(node);
    }

    public void visit(LimitImpl node) {
        isApplicable(node);
    }

    public void visit(OptionImpl node) {
        isApplicable(node);
    }

    public void visit(ReferenceImpl node) {
        isApplicable(node);
    }

    public void visit(CaseExpressionImpl node) {
        isApplicable(node);
    }

    public void visit(SearchedCaseExpressionImpl node) {
        isApplicable(node);
    }

    public void visit(FunctionImpl node) {
        isApplicable(node);
    }

    public void visit(XMLParseImpl node) {
        isApplicable(node);
    }

    public void visit(QueryStringImpl node) {
        isApplicable(node);
    }

    public void visit(XMLElementImpl node) {
        isApplicable(node);
    }

    public void visit(XMLAttributesImpl node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(JSONObjectImpl node) {
        isApplicable(node);
    }

    public void visit(XMLForestImpl node) {
        isApplicable(node);
    }

    public void visit(XMLNamespacesImpl node) {
        isApplicable(node);
    }

    public void visit(AssignmentStatementImpl node) {
        isApplicable(node);
    }

    public void visit(ScalarSubqueryImpl node) {
        isApplicable(node);
    }

    public void visit(GroupSymbolImpl node) {
        isApplicable(node);
    }

    public void visit(ConstantImpl node) {
        isApplicable(node);
    }

    public void visit(ElementSymbolImpl node) {
        isApplicable(node);
    }

    public void visit(BlockImpl node) {
        isApplicable(node);
    }

    public void visit(ExpressionCriteriaImpl node) {
        isApplicable(node);
    }

    public void visit(AliasSymbolImpl node) {
        isApplicable(node);
    }

    public void visit(BaseAggregateSymbol node) {
        isApplicable(node);
    }

    public void visit(BaseWindowFunction node) {
        isApplicable(node);
    }

    public void visit(WindowSpecificationImpl node) {
        isApplicable(node);
    }

    public void visit(TextLineImpl node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(CriteriaSelectorImpl node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(HasCriteriaImpl node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(TranslateCriteriaImpl node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(ArraySymbolImpl node) {
        isApplicable(node);
    }

    // Visitor methods for language objects
    @Override
    public void visit(BetweenCriteria obj) {
        visit((BetweenCriteriaImpl) obj);
    }

    @Override
    public void visit(CaseExpression obj) {
        visit((CaseExpressionImpl) obj);
    }

    @Override
    public void visit(CompareCriteria obj) {
        visit((CompareCriteriaImpl) obj);
    }

    @Override
    public void visit(CompoundCriteria obj) {
        visit((CompoundCriteriaImpl) obj);
    }

    @Override
    public void visit(Delete obj) {
        visit((DeleteImpl) obj);
    }

    @Override
    public void visit(ExistsCriteria obj) {
        visit((ExistsCriteriaImpl) obj);
    }

    @Override
    public void visit(From obj) {
        visit((FromImpl) obj);
    }

    @Override
    public void visit(GroupBy obj) {
        visit((GroupByImpl) obj);
    }

    @Override
    public void visit(Insert obj) {
        visit((InsertImpl) obj);
    }

    @Override
    public void visit(IsNullCriteria obj) {
        visit((IsNullCriteriaImpl) obj);
    }

    @Override
    public void visit(JoinPredicate obj) {
        visit((JoinPredicateImpl) obj);
    }

    @Override
    public void visit(JoinType obj) {
        visit((JoinTypeImpl) obj);
    }

    @Override
    public void visit(Limit obj) {
        visit((LimitImpl) obj);
    }

    @Override
    public void visit(MatchCriteria obj) {
        visit((MatchCriteriaImpl) obj);
    }

    @Override
    public void visit(NotCriteria obj) {
        visit((NotCriteriaImpl) obj);
    }

    @Override
    public void visit(Option obj) {
        visit((OptionImpl) obj);
    }

    @Override
    public void visit(OrderBy obj) {
        visit((OrderByImpl) obj);
    }

    @Override
    public void visit(Query obj) {
        visit((QueryImpl) obj);
    }

    @Override
    public void visit(SearchedCaseExpression obj) {
        visit((SearchedCaseExpressionImpl) obj);
    }

    @Override
    public void visit(Select obj) {
        visit((SelectImpl) obj);
    }

    @Override
    public void visit(SetCriteria obj) {
        visit((SetCriteriaImpl) obj);
    }

    @Override
    public void visit(SetQuery obj) {
        visit((SetQueryImpl) obj);
    }

    @Override
    public void visit(StoredProcedure obj) {
        visit((StoredProcedureImpl) obj);
    }

    @Override
    public void visit(SubqueryCompareCriteria obj) {
        visit((SubqueryCompareCriteriaImpl) obj);
    }

    @Override
    public void visit(SubqueryFromClause obj) {
        visit((SubqueryFromClauseImpl) obj);
    }

    @Override
    public void visit(SubquerySetCriteria obj) {
        visit((SubquerySetCriteriaImpl) obj);
    }

    @Override
    public void visit(UnaryFromClause obj) {
        visit((UnaryFromClauseImpl) obj);
    }

    @Override
    public void visit(Update obj) {
        visit((UpdateImpl) obj);
    }

    @Override
    public void visit(Into obj) {
        visit((IntoImpl) obj);
    }

    public void visit(DependentSetCriteria obj) {
    }

    public void visit(Create obj) {
    }

    @Override
    public void visit(Drop obj) {
        visit((DropImpl) obj);
    }

    // Visitor methods for symbol objects
    @Override
    public void visit(AggregateSymbol obj) {
        visit((BaseAggregateSymbol) obj);
    }

    @Override
    public void visit(AliasSymbol obj) {
        visit((AliasSymbolImpl) obj);
    }

    @Override
    public void visit(MultipleElementSymbol obj) {
        visit((MultipleElementSymbolImpl) obj);
    }

    @Override
    public void visit(Constant obj) {
        visit((ConstantImpl) obj);
    }

    @Override
    public void visit(ElementSymbol obj) {
        visit((ElementSymbolImpl) obj);
    }

    @Override
    public void visit(ExpressionSymbol obj) {
        visit((ExpressionSymbolImpl) obj);
    }

    @Override
    public void visit(Function obj) {
        visit((FunctionImpl) obj);
    }

    @Override
    public void visit(GroupSymbol obj) {
        visit((GroupSymbolImpl) obj);
    }

    @Override
    public void visit(Reference obj) {
        visit((ReferenceImpl) obj);
    }

    @Override
    public void visit(ScalarSubquery obj) {
        visit((ScalarSubqueryImpl) obj);
    }

    // Visitor methods for procedure language objects    
    @Override
    public void visit(AssignmentStatement obj) {
        visit((AssignmentStatementImpl) obj);
    }

    @Override
    public void visit(Block obj) {
        visit((BlockImpl) obj);
    }

    @Override
    public void visit(CommandStatement obj) {
        visit((CommandStatementImpl) obj);
    }

    @Override
    public void visit(CreateProcedureCommand obj) {
        if (obj instanceof CreateProcedureCommandImpl)
            visit((CreateProcedureCommandImpl) obj);
        else
            visit((CreateUpdateProcedureCommandImpl) obj);
    }

    @Override
    public void visit(CriteriaSelector obj) {
        visit((CriteriaSelectorImpl) obj);
    }

    @Override
    public void visit(DeclareStatement obj) {
        visit((AssignmentStatement) obj);
    }

    @Override
    public void visit(HasCriteria obj) {
        visit((HasCriteriaImpl) obj);
    }

    @Override
    public void visit(IfStatement obj) {
        visit((IfStatementImpl) obj);
    }

    @Override
    public void visit(RaiseStatement obj) {
        if (obj instanceof RaiseStatementImpl)
            visit((RaiseStatementImpl) obj);
        else
            visit((RaiseErrorStatementImpl) obj);
    }

    @Override
    public void visit(BranchingStatement obj) {
        visit((BranchingStatementImpl) obj);
    }

    @Override
    public void visit(TranslateCriteria obj) {
        visit((TranslateCriteriaImpl) obj);
    }

    @Override
    public void visit(WhileStatement obj) {
        visit((WhileStatementImpl) obj);
    }

    @Override
    public void visit(LoopStatement obj) {
        visit((LoopStatementImpl) obj);
    }

    @Override
    public void visit(DynamicCommand obj) {
        visit((DynamicCommandImpl) obj);
    }

    @Override
    public void visit(SetClauseList obj) {
        visit((SetClauseListImpl) obj);
    }

    @Override
    public void visit(SetClause obj) {
        visit((SetClauseImpl) obj);
    }

    @Override
    public void visit(OrderByItem obj) {
        visit((OrderByItemImpl) obj);
    }

    @Override
    public void visit(XMLElement obj) {
        visit((XMLElementImpl) obj);
    }

    @Override
    public void visit(XMLAttributes obj) {
        visit((XMLAttributesImpl) obj);
    }

    @Override
    public void visit(XMLForest obj) {
        visit((XMLForestImpl) obj);
    }

    @Override
    public void visit(XMLNamespaces obj) {
        visit((XMLNamespacesImpl) obj);
    }

    @Override
    public void visit(TextTable obj) {
        visit((TextTableImpl) obj);
    }

    @Override
    public void visit(TextLine obj) {
        visit((TextLineImpl) obj);
    }

    @Override
    public void visit(XMLTable obj) {
        visit((XMLTableImpl) obj);
    }

    @Override
    public void visit(DerivedColumn obj) {
        visit((DerivedColumnImpl) obj);
    }

    @Override
    public void visit(XMLSerialize obj) {
        visit((XMLSerializeImpl) obj);
    }

    @Override
    public void visit(XMLQuery obj) {
        visit((XMLQueryImpl) obj);
    }

    @Override
    public void visit(QueryString obj) {
        visit((QueryStringImpl) obj);
    }

    @Override
    public void visit(XMLParse obj) {
        visit((XMLParseImpl) obj);
    }

    @Override
    public void visit(ExpressionCriteria obj) {
        visit((ExpressionCriteriaImpl) obj);
    }

    @Override
    public void visit(WithQueryCommand obj) {
        visit((WithQueryCommandImpl) obj);
    }

    @Override
    public void visit(TriggerAction obj) {
        visit((TriggerActionImpl) obj);
    }

    @Override
    public void visit(ObjectTable obj) {
        visit((ObjectTableImpl) obj);
    }

    @Override
    public void visit(ArrayTable obj) {
        visit((ArrayTableImpl) obj);
    }

    @Override
    public void visit(AlterView obj) {
        visit((AlterViewImpl) obj);
    }

    @Override
    public void visit(AlterProcedure obj) {
        visit((AlterProcedureImpl) obj);
    }

    @Override
    public void visit(AlterTrigger obj) {
        visit((AlterTriggerImpl) obj);
    }

    @Override
    public void visit(WindowFunction obj) {
        visit((BaseWindowFunction) obj);
    }

    @Override
    public void visit(WindowSpecification obj) {
        visit((WindowSpecificationImpl) obj);
    }

    @Override
    public void visit(ExceptionExpression obj) {
        visit((ExceptionExpressionImpl) obj);
    }

    @Override
    public void visit(ReturnStatement obj) {
        visit((ReturnStatementImpl) obj);
    }

    @Override
    public void visit(Array obj) {
        visit((ArraySymbolImpl) obj);
    }
}
