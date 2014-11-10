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
import org.komodo.spi.query.sql.lang.IAlterProcedure;
import org.komodo.spi.query.sql.lang.IAlterTrigger;
import org.komodo.spi.query.sql.lang.IAlterView;
import org.komodo.spi.query.sql.lang.IArrayTable;
import org.komodo.spi.query.sql.lang.IBetweenCriteria;
import org.komodo.spi.query.sql.lang.ICompareCriteria;
import org.komodo.spi.query.sql.lang.ICompoundCriteria;
import org.komodo.spi.query.sql.lang.ICreate;
import org.komodo.spi.query.sql.lang.IDelete;
import org.komodo.spi.query.sql.lang.IDependentSetCriteria;
import org.komodo.spi.query.sql.lang.IDrop;
import org.komodo.spi.query.sql.lang.IDynamicCommand;
import org.komodo.spi.query.sql.lang.IExistsCriteria;
import org.komodo.spi.query.sql.lang.IExpressionCriteria;
import org.komodo.spi.query.sql.lang.IFrom;
import org.komodo.spi.query.sql.lang.IGroupBy;
import org.komodo.spi.query.sql.lang.IInsert;
import org.komodo.spi.query.sql.lang.IInto;
import org.komodo.spi.query.sql.lang.IIsNullCriteria;
import org.komodo.spi.query.sql.lang.IJoinPredicate;
import org.komodo.spi.query.sql.lang.IJoinType;
import org.komodo.spi.query.sql.lang.ILanguageObject;
import org.komodo.spi.query.sql.lang.ILimit;
import org.komodo.spi.query.sql.lang.IMatchCriteria;
import org.komodo.spi.query.sql.lang.INotCriteria;
import org.komodo.spi.query.sql.lang.IObjectTable;
import org.komodo.spi.query.sql.lang.IOption;
import org.komodo.spi.query.sql.lang.IOrderBy;
import org.komodo.spi.query.sql.lang.IOrderByItem;
import org.komodo.spi.query.sql.lang.IQuery;
import org.komodo.spi.query.sql.lang.ISelect;
import org.komodo.spi.query.sql.lang.ISetClause;
import org.komodo.spi.query.sql.lang.ISetClauseList;
import org.komodo.spi.query.sql.lang.ISetCriteria;
import org.komodo.spi.query.sql.lang.ISetQuery;
import org.komodo.spi.query.sql.lang.IStoredProcedure;
import org.komodo.spi.query.sql.lang.ISubqueryCompareCriteria;
import org.komodo.spi.query.sql.lang.ISubqueryFromClause;
import org.komodo.spi.query.sql.lang.ISubquerySetCriteria;
import org.komodo.spi.query.sql.lang.ITextTable;
import org.komodo.spi.query.sql.lang.IUnaryFromClause;
import org.komodo.spi.query.sql.lang.IUpdate;
import org.komodo.spi.query.sql.lang.IWithQueryCommand;
import org.komodo.spi.query.sql.lang.IXMLTable;
import org.komodo.spi.query.sql.proc.IAssignmentStatement;
import org.komodo.spi.query.sql.proc.IBlock;
import org.komodo.spi.query.sql.proc.IBranchingStatement;
import org.komodo.spi.query.sql.proc.ICommandStatement;
import org.komodo.spi.query.sql.proc.ICreateProcedureCommand;
import org.komodo.spi.query.sql.proc.ICriteriaSelector;
import org.komodo.spi.query.sql.proc.IDeclareStatement;
import org.komodo.spi.query.sql.proc.IExceptionExpression;
import org.komodo.spi.query.sql.proc.IHasCriteria;
import org.komodo.spi.query.sql.proc.IIfStatement;
import org.komodo.spi.query.sql.proc.ILoopStatement;
import org.komodo.spi.query.sql.proc.IRaiseStatement;
import org.komodo.spi.query.sql.proc.IReturnStatement;
import org.komodo.spi.query.sql.proc.ITranslateCriteria;
import org.komodo.spi.query.sql.proc.ITriggerAction;
import org.komodo.spi.query.sql.proc.IWhileStatement;
import org.komodo.spi.query.sql.symbol.IAggregateSymbol;
import org.komodo.spi.query.sql.symbol.IAliasSymbol;
import org.komodo.spi.query.sql.symbol.IArray;
import org.komodo.spi.query.sql.symbol.ICaseExpression;
import org.komodo.spi.query.sql.symbol.IConstant;
import org.komodo.spi.query.sql.symbol.IDerivedColumn;
import org.komodo.spi.query.sql.symbol.IElementSymbol;
import org.komodo.spi.query.sql.symbol.IExpressionSymbol;
import org.komodo.spi.query.sql.symbol.IFunction;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;
import org.komodo.spi.query.sql.symbol.IMultipleElementSymbol;
import org.komodo.spi.query.sql.symbol.IQueryString;
import org.komodo.spi.query.sql.symbol.IReference;
import org.komodo.spi.query.sql.symbol.IScalarSubquery;
import org.komodo.spi.query.sql.symbol.ISearchedCaseExpression;
import org.komodo.spi.query.sql.symbol.ITextLine;
import org.komodo.spi.query.sql.symbol.IWindowFunction;
import org.komodo.spi.query.sql.symbol.IWindowSpecification;
import org.komodo.spi.query.sql.symbol.IXMLAttributes;
import org.komodo.spi.query.sql.symbol.IXMLElement;
import org.komodo.spi.query.sql.symbol.IXMLForest;
import org.komodo.spi.query.sql.symbol.IXMLNamespaces;
import org.komodo.spi.query.sql.symbol.IXMLParse;
import org.komodo.spi.query.sql.symbol.IXMLQuery;
import org.komodo.spi.query.sql.symbol.IXMLSerialize;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.AlterProcedure;
import org.teiid.query.sql.lang.AlterTrigger;
import org.teiid.query.sql.lang.AlterView;
import org.teiid.query.sql.lang.ArrayTable;
import org.teiid.query.sql.lang.BetweenCriteria;
import org.teiid.query.sql.lang.Command;
import org.teiid.query.sql.lang.CompareCriteria;
import org.teiid.query.sql.lang.CompoundCriteria;
import org.teiid.query.sql.lang.Create;
import org.teiid.query.sql.lang.Criteria;
import org.teiid.query.sql.lang.CriteriaSelector;
import org.teiid.query.sql.lang.Delete;
import org.teiid.query.sql.lang.Drop;
import org.teiid.query.sql.lang.DynamicCommand;
import org.teiid.query.sql.lang.ExistsCriteria;
import org.teiid.query.sql.lang.ExpressionCriteria;
import org.teiid.query.sql.lang.From;
import org.teiid.query.sql.lang.FromClause;
import org.teiid.query.sql.lang.GroupBy;
import org.teiid.query.sql.lang.HasCriteria;
import org.teiid.query.sql.lang.Insert;
import org.teiid.query.sql.lang.Into;
import org.teiid.query.sql.lang.IsNullCriteria;
import org.teiid.query.sql.lang.JoinPredicate;
import org.teiid.query.sql.lang.JoinType;
import org.teiid.query.sql.lang.LanguageObject;
import org.teiid.query.sql.lang.Limit;
import org.teiid.query.sql.lang.MatchCriteria;
import org.teiid.query.sql.lang.NotCriteria;
import org.teiid.query.sql.lang.ObjectColumn;
import org.teiid.query.sql.lang.ObjectTable;
import org.teiid.query.sql.lang.Option;
import org.teiid.query.sql.lang.OrderBy;
import org.teiid.query.sql.lang.OrderByItem;
import org.teiid.query.sql.lang.ProjectedColumn;
import org.teiid.query.sql.lang.Query;
import org.teiid.query.sql.lang.QueryCommand;
import org.teiid.query.sql.lang.Select;
import org.teiid.query.sql.lang.SetClause;
import org.teiid.query.sql.lang.SetClauseList;
import org.teiid.query.sql.lang.SetCriteria;
import org.teiid.query.sql.lang.SetQuery;
import org.teiid.query.sql.lang.StoredProcedure;
import org.teiid.query.sql.lang.SubqueryCompareCriteria;
import org.teiid.query.sql.lang.SubqueryFromClause;
import org.teiid.query.sql.lang.SubquerySetCriteria;
import org.teiid.query.sql.lang.TextColumn;
import org.teiid.query.sql.lang.TextTable;
import org.teiid.query.sql.lang.TranslateCriteria;
import org.teiid.query.sql.lang.UnaryFromClause;
import org.teiid.query.sql.lang.Update;
import org.teiid.query.sql.lang.WithQueryCommand;
import org.teiid.query.sql.lang.XMLColumn;
import org.teiid.query.sql.lang.XMLTable;
import org.teiid.query.sql.proc.AssignmentStatement;
import org.teiid.query.sql.proc.Block;
import org.teiid.query.sql.proc.BranchingStatement;
import org.teiid.query.sql.proc.CommandStatement;
import org.teiid.query.sql.proc.CreateProcedureCommand;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommand;
import org.teiid.query.sql.proc.DeclareStatement;
import org.teiid.query.sql.proc.ExceptionExpression;
import org.teiid.query.sql.proc.IfStatement;
import org.teiid.query.sql.proc.LoopStatement;
import org.teiid.query.sql.proc.RaiseErrorStatement;
import org.teiid.query.sql.proc.RaiseStatement;
import org.teiid.query.sql.proc.ReturnStatement;
import org.teiid.query.sql.proc.Statement;
import org.teiid.query.sql.proc.TriggerAction;
import org.teiid.query.sql.proc.WhileStatement;
import org.teiid.query.sql.symbol.AggregateSymbol;
import org.teiid.query.sql.symbol.AliasSymbol;
import org.teiid.query.sql.symbol.Array;
import org.teiid.query.sql.symbol.CaseExpression;
import org.teiid.query.sql.symbol.Constant;
import org.teiid.query.sql.symbol.DerivedColumn;
import org.teiid.query.sql.symbol.ElementSymbol;
import org.teiid.query.sql.symbol.ExpressionSymbol;
import org.teiid.query.sql.symbol.Function;
import org.teiid.query.sql.symbol.GroupSymbol;
import org.teiid.query.sql.symbol.JSONObject;
import org.teiid.query.sql.symbol.MultipleElementSymbol;
import org.teiid.query.sql.symbol.QueryString;
import org.teiid.query.sql.symbol.Reference;
import org.teiid.query.sql.symbol.ScalarSubquery;
import org.teiid.query.sql.symbol.SearchedCaseExpression;
import org.teiid.query.sql.symbol.TextLine;
import org.teiid.query.sql.symbol.WindowFunction;
import org.teiid.query.sql.symbol.WindowSpecification;
import org.teiid.query.sql.symbol.XMLAttributes;
import org.teiid.query.sql.symbol.XMLElement;
import org.teiid.query.sql.symbol.XMLForest;
import org.teiid.query.sql.symbol.XMLNamespaces;
import org.teiid.query.sql.symbol.XMLParse;
import org.teiid.query.sql.symbol.XMLQuery;
import org.teiid.query.sql.symbol.XMLSerialize;

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
                if (LanguageObject.class.isAssignableFrom(param)) {
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

    protected <T extends LanguageObject> T createNode(ASTNodes nodeType) {
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

    protected void isApplicable(ILanguageObject node) {
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

    public void visit(LanguageObject node) {
        isApplicable(node);
    }

    public void visit(Command node) {
        isApplicable(node);
    }

    public void visit(AlterView node) {
        isApplicable(node);
    }

    public void visit(AlterTrigger node) {
        isApplicable(node);
    }

    public void visit(AlterProcedure node) {
        isApplicable(node);
    }

    public void visit(TriggerAction node) {
        isApplicable(node);
    }

    public void visit(Drop node) {
        isApplicable(node);
    }

    public void visit(Create node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(RaiseErrorStatement node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(RaiseStatement node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(ExceptionExpression node) {
        isApplicable(node);
    }

    public void visit(Statement node) {
        isApplicable(node);
    }

    public void visit(BranchingStatement node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(ReturnStatement node) {
        isApplicable(node);
    }

    public void visit(WhileStatement node) {
        isApplicable(node);
    }

    public void visit(LoopStatement node) {
        isApplicable(node);
    }

    public void visit(IfStatement node) {
        isApplicable(node);
    }

    public void visit(DeclareStatement node) {
        isApplicable(node);
    }

    public void visit(CommandStatement node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(CreateProcedureCommand node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(CreateUpdateProcedureCommand node) {
        isApplicable(node);
    }

    public void visit(DynamicCommand node) {
        isApplicable(node);
    }

    public void visit(SetClauseList node) {
        isApplicable(node);
    }

    public void visit(SetClause node) {
        isApplicable(node);
    }

    public void visit(ProjectedColumn node) {
        isApplicable(node);
    }

    public void visit(StoredProcedure node) {
        isApplicable(node);
    }

    public void visit(Insert node) {
        isApplicable(node);
    }

    public void visit(Update node) {
        isApplicable(node);
    }

    public void visit(Delete node) {
        isApplicable(node);
    }

    public void visit(QueryCommand node) {
        isApplicable(node);
    }

    public void visit(WithQueryCommand node) {
        isApplicable(node);
    }

    public void visit(SetQuery node) {
        isApplicable(node);
    }

    public void visit(Query node) {
        isApplicable(node);
    }

    public void visit(Into node) {
        isApplicable(node);
    }

    public void visit(Select node) {
        isApplicable(node);
    }

    public void visit(ExpressionSymbol node) {
        isApplicable(node);
    }

    public void visit(DerivedColumn node) {
        isApplicable(node);
    }

    public void visit(MultipleElementSymbol node) {
        isApplicable(node);
    }

    public void visit(From node) {
        isApplicable(node);
    }

    public void visit(FromClause node) {
        isApplicable(node);
    }

    public void visit(JoinPredicate node) {
        isApplicable(node);
    }

    public void visit(JoinType node) {
        isApplicable(node);
    }

    public void visit(XMLSerialize node) {
        isApplicable(node);
    }

    public void visit(ArrayTable node) {
        isApplicable(node);
    }

    public void visit(TextTable node) {
        isApplicable(node);
    }

    public void visit(TextColumn node) {
        isApplicable(node);
    }

    public void visit(XMLQuery node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(ObjectTable node) {
        isApplicable(node);
    }

    public void visit(ObjectColumn node) {
        isApplicable(node);
    }

    public void visit(XMLTable node) {
        isApplicable(node);
    }

    public void visit(XMLColumn node) {
        isApplicable(node);
    }

    public void visit(SubqueryFromClause node) {
        isApplicable(node);
    }

    public void visit(UnaryFromClause node) {
        isApplicable(node);
    }

    public void visit(Criteria node) {
        isApplicable(node);
    }

    public void visit(CompoundCriteria node) {
        isApplicable(node);
    }

    public void visit(NotCriteria node) {
        isApplicable(node);
    }

    public void visit(CompareCriteria node) {
        isApplicable(node);
    }

    public void visit(SubqueryCompareCriteria node) {
        isApplicable(node);
    }

    public void visit(MatchCriteria node) {
        isApplicable(node);
    }

    public void visit(BetweenCriteria node) {
        isApplicable(node);
    }

    public void visit(IsNullCriteria node) {
        isApplicable(node);
    }

    public void visit(SubquerySetCriteria node) {
        isApplicable(node);
    }

    public void visit(SetCriteria node) {
        isApplicable(node);
    }

    public void visit(ExistsCriteria node) {
        isApplicable(node);
    }

    public void visit(GroupBy node) {
        isApplicable(node);
    }

    public void visit(OrderBy node) {
        isApplicable(node);
    }

    public void visit(OrderByItem node) {
        isApplicable(node);
    }

    public void visit(Limit node) {
        isApplicable(node);
    }

    public void visit(Option node) {
        isApplicable(node);
    }

    public void visit(Reference node) {
        isApplicable(node);
    }

    public void visit(CaseExpression node) {
        isApplicable(node);
    }

    public void visit(SearchedCaseExpression node) {
        isApplicable(node);
    }

    public void visit(Function node) {
        isApplicable(node);
    }

    public void visit(XMLParse node) {
        isApplicable(node);
    }

    public void visit(QueryString node) {
        isApplicable(node);
    }

    public void visit(XMLElement node) {
        isApplicable(node);
    }

    public void visit(XMLAttributes node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(JSONObject node) {
        isApplicable(node);
    }

    public void visit(XMLForest node) {
        isApplicable(node);
    }

    public void visit(XMLNamespaces node) {
        isApplicable(node);
    }

    public void visit(AssignmentStatement node) {
        isApplicable(node);
    }

    public void visit(ScalarSubquery node) {
        isApplicable(node);
    }

    public void visit(GroupSymbol node) {
        isApplicable(node);
    }

    public void visit(Constant node) {
        isApplicable(node);
    }

    public void visit(ElementSymbol node) {
        isApplicable(node);
    }

    public void visit(Block node) {
        isApplicable(node);
    }

    public void visit(ExpressionCriteria node) {
        isApplicable(node);
    }

    public void visit(AliasSymbol node) {
        isApplicable(node);
    }

    public void visit(AggregateSymbol node) {
        isApplicable(node);
    }

    public void visit(WindowFunction node) {
        isApplicable(node);
    }

    public void visit(WindowSpecification node) {
        isApplicable(node);
    }

    public void visit(TextLine node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(CriteriaSelector node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(HasCriteria node) {
        isApplicable(node);
    }

    @Removed(Version.TEIID_8_0)
    public void visit(TranslateCriteria node) {
        isApplicable(node);
    }

    @Since(Version.TEIID_8_0)
    public void visit(Array node) {
        isApplicable(node);
    }

    // Visitor methods for language objects
    @Override
    public void visit(IBetweenCriteria obj) {
        visit((BetweenCriteria) obj);
    }

    @Override
    public void visit(ICaseExpression obj) {
        visit((CaseExpression) obj);
    }

    @Override
    public void visit(ICompareCriteria obj) {
        visit((CompareCriteria) obj);
    }

    @Override
    public void visit(ICompoundCriteria obj) {
        visit((CompoundCriteria) obj);
    }

    @Override
    public void visit(IDelete obj) {
        visit((Delete) obj);
    }

    @Override
    public void visit(IExistsCriteria obj) {
        visit((ExistsCriteria) obj);
    }

    @Override
    public void visit(IFrom obj) {
        visit((From) obj);
    }

    @Override
    public void visit(IGroupBy obj) {
        visit((GroupBy) obj);
    }

    @Override
    public void visit(IInsert obj) {
        visit((Insert) obj);
    }

    @Override
    public void visit(IIsNullCriteria obj) {
        visit((IsNullCriteria) obj);
    }

    @Override
    public void visit(IJoinPredicate obj) {
        visit((JoinPredicate) obj);
    }

    @Override
    public void visit(IJoinType obj) {
        visit((JoinType) obj);
    }

    @Override
    public void visit(ILimit obj) {
        visit((Limit) obj);
    }

    @Override
    public void visit(IMatchCriteria obj) {
        visit((MatchCriteria) obj);
    }

    @Override
    public void visit(INotCriteria obj) {
        visit((NotCriteria) obj);
    }

    @Override
    public void visit(IOption obj) {
        visit((Option) obj);
    }

    @Override
    public void visit(IOrderBy obj) {
        visit((OrderBy) obj);
    }

    @Override
    public void visit(IQuery obj) {
        visit((Query) obj);
    }

    @Override
    public void visit(ISearchedCaseExpression obj) {
        visit((SearchedCaseExpression) obj);
    }

    @Override
    public void visit(ISelect obj) {
        visit((Select) obj);
    }

    @Override
    public void visit(ISetCriteria obj) {
        visit((SetCriteria) obj);
    }

    @Override
    public void visit(ISetQuery obj) {
        visit((SetQuery) obj);
    }

    @Override
    public void visit(IStoredProcedure obj) {
        visit((StoredProcedure) obj);
    }

    @Override
    public void visit(ISubqueryCompareCriteria obj) {
        visit((SubqueryCompareCriteria) obj);
    }

    @Override
    public void visit(ISubqueryFromClause obj) {
        visit((SubqueryFromClause) obj);
    }

    @Override
    public void visit(ISubquerySetCriteria obj) {
        visit((SubquerySetCriteria) obj);
    }

    @Override
    public void visit(IUnaryFromClause obj) {
        visit((UnaryFromClause) obj);
    }

    @Override
    public void visit(IUpdate obj) {
        visit((Update) obj);
    }

    @Override
    public void visit(IInto obj) {
        visit((Into) obj);
    }

    public void visit(IDependentSetCriteria obj) {
    }

    public void visit(ICreate obj) {
    }

    @Override
    public void visit(IDrop obj) {
        visit((Drop) obj);
    }

    // Visitor methods for symbol objects
    @Override
    public void visit(IAggregateSymbol obj) {
        visit((AggregateSymbol) obj);
    }

    @Override
    public void visit(IAliasSymbol obj) {
        visit((AliasSymbol) obj);
    }

    @Override
    public void visit(IMultipleElementSymbol obj) {
        visit((MultipleElementSymbol) obj);
    }

    @Override
    public void visit(IConstant obj) {
        visit((Constant) obj);
    }

    @Override
    public void visit(IElementSymbol obj) {
        visit((ElementSymbol) obj);
    }

    @Override
    public void visit(IExpressionSymbol obj) {
        visit((ExpressionSymbol) obj);
    }

    @Override
    public void visit(IFunction obj) {
        visit((Function) obj);
    }

    @Override
    public void visit(IGroupSymbol obj) {
        visit((GroupSymbol) obj);
    }

    @Override
    public void visit(IReference obj) {
        visit((Reference) obj);
    }

    @Override
    public void visit(IScalarSubquery obj) {
        visit((ScalarSubquery) obj);
    }

    // Visitor methods for procedure language objects    
    @Override
    public void visit(IAssignmentStatement obj) {
        visit((AssignmentStatement) obj);
    }

    @Override
    public void visit(IBlock obj) {
        visit((Block) obj);
    }

    @Override
    public void visit(ICommandStatement obj) {
        visit((CommandStatement) obj);
    }

    @Override
    public void visit(ICreateProcedureCommand obj) {
        if (obj instanceof CreateProcedureCommand)
            visit((CreateProcedureCommand) obj);
        else
            visit((CreateUpdateProcedureCommand) obj);
    }

    @Override
    public void visit(ICriteriaSelector obj) {
        visit((CriteriaSelector) obj);
    }

    @Override
    public void visit(IDeclareStatement obj) {
        visit((IAssignmentStatement) obj);
    }

    @Override
    public void visit(IHasCriteria obj) {
        visit((HasCriteria) obj);
    }

    @Override
    public void visit(IIfStatement obj) {
        visit((IfStatement) obj);
    }

    @Override
    public void visit(IRaiseStatement obj) {
        if (obj instanceof RaiseStatement)
            visit((RaiseStatement) obj);
        else
            visit((RaiseErrorStatement) obj);
    }

    @Override
    public void visit(IBranchingStatement obj) {
        visit((BranchingStatement) obj);
    }

    @Override
    public void visit(ITranslateCriteria obj) {
        visit((TranslateCriteria) obj);
    }

    @Override
    public void visit(IWhileStatement obj) {
        visit((WhileStatement) obj);
    }

    @Override
    public void visit(ILoopStatement obj) {
        visit((LoopStatement) obj);
    }

    @Override
    public void visit(IDynamicCommand obj) {
        visit((DynamicCommand) obj);
    }

    @Override
    public void visit(ISetClauseList obj) {
        visit((SetClauseList) obj);
    }

    @Override
    public void visit(ISetClause obj) {
        visit((SetClause) obj);
    }

    @Override
    public void visit(IOrderByItem obj) {
        visit((OrderByItem) obj);
    }

    @Override
    public void visit(IXMLElement obj) {
        visit((XMLElement) obj);
    }

    @Override
    public void visit(IXMLAttributes obj) {
        visit((XMLAttributes) obj);
    }

    @Override
    public void visit(IXMLForest obj) {
        visit((XMLForest) obj);
    }

    @Override
    public void visit(IXMLNamespaces obj) {
        visit((XMLNamespaces) obj);
    }

    @Override
    public void visit(ITextTable obj) {
        visit((TextTable) obj);
    }

    @Override
    public void visit(ITextLine obj) {
        visit((TextLine) obj);
    }

    @Override
    public void visit(IXMLTable obj) {
        visit((XMLTable) obj);
    }

    @Override
    public void visit(IDerivedColumn obj) {
        visit((DerivedColumn) obj);
    }

    @Override
    public void visit(IXMLSerialize obj) {
        visit((XMLSerialize) obj);
    }

    @Override
    public void visit(IXMLQuery obj) {
        visit((XMLQuery) obj);
    }

    @Override
    public void visit(IQueryString obj) {
        visit((QueryString) obj);
    }

    @Override
    public void visit(IXMLParse obj) {
        visit((XMLParse) obj);
    }

    @Override
    public void visit(IExpressionCriteria obj) {
        visit((ExpressionCriteria) obj);
    }

    @Override
    public void visit(IWithQueryCommand obj) {
        visit((WithQueryCommand) obj);
    }

    @Override
    public void visit(ITriggerAction obj) {
        visit((TriggerAction) obj);
    }

    @Override
    public void visit(IObjectTable obj) {
        visit((ObjectTable) obj);
    }

    @Override
    public void visit(IArrayTable obj) {
        visit((ArrayTable) obj);
    }

    @Override
    public void visit(IAlterView obj) {
        visit((AlterView) obj);
    }

    @Override
    public void visit(IAlterProcedure obj) {
        visit((AlterProcedure) obj);
    }

    @Override
    public void visit(IAlterTrigger obj) {
        visit((AlterTrigger) obj);
    }

    @Override
    public void visit(IWindowFunction obj) {
        visit((WindowFunction) obj);
    }

    @Override
    public void visit(IWindowSpecification obj) {
        visit((WindowSpecification) obj);
    }

    @Override
    public void visit(IExceptionExpression obj) {
        visit((ExceptionExpression) obj);
    }

    @Override
    public void visit(IReturnStatement obj) {
        visit((ReturnStatement) obj);
    }

    @Override
    public void visit(IArray obj) {
        visit((Array) obj);
    }
}
