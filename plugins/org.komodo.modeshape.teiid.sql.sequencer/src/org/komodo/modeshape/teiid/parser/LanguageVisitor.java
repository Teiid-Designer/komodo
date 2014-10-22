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
package org.komodo.modeshape.teiid.parser;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import org.komodo.modeshape.teiid.Messages;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sql.lang.AlterProcedure;
import org.komodo.modeshape.teiid.sql.lang.AlterTrigger;
import org.komodo.modeshape.teiid.sql.lang.AlterView;
import org.komodo.modeshape.teiid.sql.lang.ArrayTable;
import org.komodo.modeshape.teiid.sql.lang.BetweenCriteria;
import org.komodo.modeshape.teiid.sql.lang.Command;
import org.komodo.modeshape.teiid.sql.lang.CompareCriteria;
import org.komodo.modeshape.teiid.sql.lang.CompoundCriteria;
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.modeshape.teiid.sql.lang.Delete;
import org.komodo.modeshape.teiid.sql.lang.DynamicCommand;
import org.komodo.modeshape.teiid.sql.lang.ExistsCriteria;
import org.komodo.modeshape.teiid.sql.lang.ExpressionCriteria;
import org.komodo.modeshape.teiid.sql.lang.From;
import org.komodo.modeshape.teiid.sql.lang.FromClause;
import org.komodo.modeshape.teiid.sql.lang.GroupBy;
import org.komodo.modeshape.teiid.sql.lang.Insert;
import org.komodo.modeshape.teiid.sql.lang.Into;
import org.komodo.modeshape.teiid.sql.lang.IsNullCriteria;
import org.komodo.modeshape.teiid.sql.lang.JoinPredicate;
import org.komodo.modeshape.teiid.sql.lang.JoinType;
import org.komodo.modeshape.teiid.sql.lang.LanguageObject;
import org.komodo.modeshape.teiid.sql.lang.Limit;
import org.komodo.modeshape.teiid.sql.lang.MatchCriteria;
import org.komodo.modeshape.teiid.sql.lang.NotCriteria;
import org.komodo.modeshape.teiid.sql.lang.ObjectColumn;
import org.komodo.modeshape.teiid.sql.lang.ObjectTable;
import org.komodo.modeshape.teiid.sql.lang.Option;
import org.komodo.modeshape.teiid.sql.lang.OrderBy;
import org.komodo.modeshape.teiid.sql.lang.OrderByItem;
import org.komodo.modeshape.teiid.sql.lang.ProjectedColumn;
import org.komodo.modeshape.teiid.sql.lang.Query;
import org.komodo.modeshape.teiid.sql.lang.QueryCommand;
import org.komodo.modeshape.teiid.sql.lang.Select;
import org.komodo.modeshape.teiid.sql.lang.SetClause;
import org.komodo.modeshape.teiid.sql.lang.SetClauseList;
import org.komodo.modeshape.teiid.sql.lang.SetCriteria;
import org.komodo.modeshape.teiid.sql.lang.SetQuery;
import org.komodo.modeshape.teiid.sql.lang.StoredProcedure;
import org.komodo.modeshape.teiid.sql.lang.SubqueryCompareCriteria;
import org.komodo.modeshape.teiid.sql.lang.SubqueryFromClause;
import org.komodo.modeshape.teiid.sql.lang.SubquerySetCriteria;
import org.komodo.modeshape.teiid.sql.lang.TextColumn;
import org.komodo.modeshape.teiid.sql.lang.TextTable;
import org.komodo.modeshape.teiid.sql.lang.UnaryFromClause;
import org.komodo.modeshape.teiid.sql.lang.Update;
import org.komodo.modeshape.teiid.sql.lang.WithQueryCommand;
import org.komodo.modeshape.teiid.sql.lang.XMLColumn;
import org.komodo.modeshape.teiid.sql.lang.XMLTable;
import org.komodo.modeshape.teiid.sql.proc.AssignmentStatement;
import org.komodo.modeshape.teiid.sql.proc.Block;
import org.komodo.modeshape.teiid.sql.proc.BranchingStatement;
import org.komodo.modeshape.teiid.sql.proc.CommandStatement;
import org.komodo.modeshape.teiid.sql.proc.CreateProcedureCommand;
import org.komodo.modeshape.teiid.sql.proc.DeclareStatement;
import org.komodo.modeshape.teiid.sql.proc.ExceptionExpression;
import org.komodo.modeshape.teiid.sql.proc.IfStatement;
import org.komodo.modeshape.teiid.sql.proc.LoopStatement;
import org.komodo.modeshape.teiid.sql.proc.RaiseStatement;
import org.komodo.modeshape.teiid.sql.proc.ReturnStatement;
import org.komodo.modeshape.teiid.sql.proc.Statement;
import org.komodo.modeshape.teiid.sql.proc.TriggerAction;
import org.komodo.modeshape.teiid.sql.proc.WhileStatement;
import org.komodo.modeshape.teiid.sql.symbol.AggregateSymbol;
import org.komodo.modeshape.teiid.sql.symbol.AliasSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Array;
import org.komodo.modeshape.teiid.sql.symbol.CaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.Constant;
import org.komodo.modeshape.teiid.sql.symbol.DerivedColumn;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.ExpressionSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Function;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.modeshape.teiid.sql.symbol.JSONObject;
import org.komodo.modeshape.teiid.sql.symbol.MultipleElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.QueryString;
import org.komodo.modeshape.teiid.sql.symbol.Reference;
import org.komodo.modeshape.teiid.sql.symbol.ScalarSubquery;
import org.komodo.modeshape.teiid.sql.symbol.SearchedCaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.TextLine;
import org.komodo.modeshape.teiid.sql.symbol.WindowFunction;
import org.komodo.modeshape.teiid.sql.symbol.WindowSpecification;
import org.komodo.modeshape.teiid.sql.symbol.XMLAttributes;
import org.komodo.modeshape.teiid.sql.symbol.XMLElement;
import org.komodo.modeshape.teiid.sql.symbol.XMLForest;
import org.komodo.modeshape.teiid.sql.symbol.XMLNamespaces;
import org.komodo.modeshape.teiid.sql.symbol.XMLParse;
import org.komodo.modeshape.teiid.sql.symbol.XMLQuery;
import org.komodo.modeshape.teiid.sql.symbol.XMLSerialize;
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
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;

/**
 *
 */
@SuppressWarnings( "javadoc" )
public abstract class LanguageVisitor extends AbstractLanguageVisitor {

    private final ITeiidVersion teiidVersion;

    /*
     * Required if nodes are to be created by the visitor
     */
    private final QueryParser parser;

    private boolean abort = false;

    private static final Map<Class<?>, Method> methodCache = new HashMap<Class<?>, Method>();

    static {
        // cache all the methods on this visitor
        Method[] methods = LanguageVisitor.class.getMethods();
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
    public LanguageVisitor(ITeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
        this.parser = new QueryParser(teiidVersion);
    }

    /**
     * @return the teiidVersion
     */
    public ITeiidVersion getTeiidVersion() {
        return this.teiidVersion;
    }

    protected boolean isTeiidVersionOrGreater(Version teiidVersion) {
        ITeiidVersion minVersion = getTeiidVersion().getMinimumVersion();
        return minVersion.equals(teiidVersion.get()) || minVersion.isGreaterThan(teiidVersion.get());
    }

    protected boolean isLessThanTeiidVersion(Version teiidVersion) {
        ITeiidVersion maxVersion = getTeiidVersion().getMaximumVersion();
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
    public QueryParser getQueryParser() {
        return this.parser;
    }

    protected ITeiidParser getTeiidParser() {
        return parser.getTeiidParser();
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

    @Override
    public void visit(ICreate obj) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void visit(IDrop obj) {
        throw new UnsupportedOperationException();
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
        visit((CreateProcedureCommand) obj);
    }

    @Override
    public void visit(ICriteriaSelector obj) {
        throw new UnsupportedOperationException(Messages.getString(Messages.TeiidParser.teiid_version_failure));
    }

    @Override
    public void visit(IDeclareStatement obj) {
        visit((IAssignmentStatement) obj);
    }

    @Override
    public void visit(IHasCriteria obj) {
        throw new UnsupportedOperationException(Messages.getString(Messages.TeiidParser.teiid_version_failure));
    }

    @Override
    public void visit(IIfStatement obj) {
        visit((IfStatement) obj);
    }

    @Override
    public void visit(IRaiseStatement obj) {
        visit((RaiseStatement) obj);
    }

    @Override
    public void visit(IBranchingStatement obj) {
        visit((BranchingStatement) obj);
    }

    @Override
    public void visit(ITranslateCriteria obj) {
        throw new UnsupportedOperationException(Messages.getString(Messages.TeiidParser.teiid_version_failure));
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
