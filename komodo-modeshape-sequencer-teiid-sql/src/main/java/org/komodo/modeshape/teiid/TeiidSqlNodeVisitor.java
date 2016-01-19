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
package org.komodo.modeshape.teiid;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.nodetype.NodeType;
import org.komodo.modeshape.AbstractNodeVisitor;
import org.komodo.modeshape.teiid.cnd.*;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AbstractCompareCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AbstractSetCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AggregateSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AliasSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Alter;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AlterTrigger;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ArraySymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ArrayTable;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AssignmentStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.BetweenCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Block;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.BranchingStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CaseExpression;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Command;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CommandStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CompareCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CompoundCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Constant;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CreateProcedureCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.DeclareStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Delete;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.DerivedColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.DynamicCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ElementSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ExceptionExpression;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ExistsCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Expression;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ExpressionCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ExpressionStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ExpressionSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.From;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.FromClause;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Function;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.GroupBy;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.GroupSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.IfStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Insert;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Into;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.IsNullCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.JSONObject;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.JoinPredicate;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.JoinType;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Labeled;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.LexTokens;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Limit;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.LoopStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.MakeDep;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.MatchCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.MultipleElementSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.NamespaceItem;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.NotCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ObjectColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ObjectTable;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Option;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.OrderBy;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.OrderByItem;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ProjectedColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Query;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.QueryCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.QueryString;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.RaiseStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Reference;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SPParameter;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Select;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SetClause;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SetClauseList;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SetCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SetQuery;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SourceHint;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SpecificHint;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.StoredProcedure;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubqueryCompareCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubqueryContainer;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubqueryFromClause;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubqueryHint;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubquerySetCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Symbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TableFunctionReference;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TargetedCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TeiidSqlCallback;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TextColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TextLine;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TextTable;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TriggerAction;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.UnaryFromClause;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Update;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.WhileStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.WindowFunction;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.WindowSpecification;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.WithQueryCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLAttributes;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLElement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLForest;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLNamespaces;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLParse;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLQuery;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLSerialize;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLTable;
import org.komodo.modeshape.teiid.language.SortSpecification;
import org.komodo.modeshape.teiid.language.SortSpecification.NullOrdering;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants.NonReserved;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants.Reserved;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants.Tokens;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator;
import org.komodo.modeshape.teiid.sql.lang.SubqueryCompareCriteriaImpl.PredicateQuantifier;
import org.komodo.modeshape.teiid.sql.lang.TriggerEvent;
import org.komodo.modeshape.teiid.sql.proc.BranchingStatementImpl.BranchingMode;
import org.komodo.spi.query.sql.lang.JoinType.Types;
import org.komodo.spi.query.sql.lang.MatchCriteria.MatchMode;
import org.komodo.spi.query.sql.lang.SPParameter.ParameterInfo;
import org.komodo.spi.query.sql.lang.SetQuery.Operation;
import org.komodo.spi.query.sql.symbol.AggregateSymbol.Type;
import org.komodo.spi.query.sql.symbol.ElementSymbol.DisplayMode;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager.DataTypeName;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.common.collection.EmptyIterator;
import org.modeshape.jcr.JcrSession;

/**
 *
 */
public class TeiidSqlNodeVisitor extends AbstractNodeVisitor
    implements TeiidSqlCallback, Reserved, NonReserved, Tokens {

    protected static final String UNDEFINED = "<undefined>"; //$NON-NLS-1$

    protected static final String BEGIN_HINT = "/*+"; //$NON-NLS-1$

    protected static final String END_HINT = "*/"; //$NON-NLS-1$

    private static final String NODE_KEY = "node"; //$NON-NLS-1$

    private static final String KEYWORD_KEY = "keyword"; //$NON-NLS-1$

    private static final String SHORT_NAME_ONLY_KEY = "shortNameOnly"; //$NON-NLS-1$

    protected class TeiidSqlNodeContext implements TeiidSqlContext {

        private Node node;

        private Map<String, Object> index = new HashMap<String, Object>();

        public TeiidSqlNodeContext(Node node) {
            this.node = node;
        }

        @Override
        public Object get(String key) {
            if (NODE_KEY.equals(key))
                return node;

            return index.get(key);
        }

        @Override
        public void add(String key, Object obj) {
            index.put(key, obj);
        }
    }

    private KLog logger = KLog.getLogger();

    private Session session;

    private StringBuilder builder;

    /**
     * Create new instance
     *
     * @param teiidVersion teiid version
     */
    public TeiidSqlNodeVisitor(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    @Override
    protected String undefined() {
        return UNDEFINED;
    }

    /**
     * @return the session
     */
    public Session getSession() {
        return this.session;
    }

    /**
     * @param node node to be visited
     * @return SQL String representation of the given node
     * @throws Exception if node causes a failure
     */
    public String getTeiidSql(Node node) throws Exception {
        if (node == null)
            return undefined();

        this.builder = new StringBuilder();
        this.session = node.getSession();

        node.accept(this);
        return builder.toString();
    }

    protected String encode(String path) {
        if (session instanceof JcrSession)
            return ((JcrSession)session).encode(path);

        return path;
    }

    protected void append(String value) {
        builder.append(value);
    }

    protected void beginClause(int level) {
        append(SPACE);
    }

    protected String stripNameSpace(String name) {
        name = name.replace(TeiidSqlLexicon.Namespace.PREFIX, EMPTY_STRING);
        name = name.replaceAll("[\\p{C}\\p{Z}:]", EMPTY_STRING); //$NON-NLS-1$
        return name;
    }

    protected void appendToken(String name) {
        name = stripNameSpace(name);
        append(name.toUpperCase());
    }

    protected void appendToken(Node node) throws RepositoryException {
        String name = node.getName();
        appendToken(name);
    }

    protected Node reference(Node node, String refName) throws RepositoryException {
        if (node == null || refName == null)
            return null;

        refName = encode(refName);
        
        if (! node.hasNode(refName))
            return null;

        return node.getNode(refName);
    }

    protected int size(Node node, String refName)  throws RepositoryException {
        if (node == null || refName == null)
            return 0;

        refName = encode(refName);

        if (! node.hasNode(refName))
            return 0;

        NodeIterator nodes = node.getNodes(refName);

        int size = 0;
        for (size = 0; nodes.hasNext(); ++size)
            nodes.next();

        return size;
    }

    protected Iterator<Node> references(Node node, String refName) throws RepositoryException {
        if (node == null || refName == null)
            return null;

        refName = encode(refName);

        if (! node.hasNode(refName))
            return new EmptyIterator<Node>();

        return node.getNodes(refName);
    }

    protected void iterate(Iterator<Node> nodes) throws RepositoryException {
        for (int i = 0; nodes.hasNext(); ++i) {
            if (i > 0)
                append(COMMA + SPACE);

            Node node = nodes.next();
            visit(node);
        }
    }

    protected void iterate(Node node, String refName) throws RepositoryException {
        Iterator<Node> nodes = references(node, refName);
        for (int i = 0; nodes.hasNext(); ++i) {
            if (i > 0)
                append(COMMA + SPACE);

            Node childNode = nodes.next();
            visit(childNode);
        }
    }

    protected boolean propertyBoolean(Node node, String propName) throws RepositoryException {
        Property property = property(node, propName);
        if (property == null)
            return false;
    
        Boolean value = propertyValue(property, DataTypeName.BOOLEAN);
        return value;
    }

    protected String propertyString(Node node, String propName) throws RepositoryException {
        Property property = property(node, propName);
        if (property == null)
            return null;
    
        String value = propertyValue(property, DataTypeName.STRING);
        return value;
    }

    protected long propertyLong(Node node, String propName) throws RepositoryException {
        Property property = property(node, propName);
        if (property == null)
            return -1L;
    
        Long value = propertyValue(property, DataTypeName.LONG);
        return value;
    }

    protected <T> T propertyValue(Value value, DataTypeName dataTypeName) throws RepositoryException {
        Object valueObject = null;

        switch (dataTypeName) {
            case STRING:
            case CHAR:
            case VARCHAR:
            case XML:
            case DATE:
            case TIME:
            case TIMESTAMP:
                valueObject = value.getString();
                break;
            case DOUBLE:
            case FLOAT:
                valueObject = value.getDouble();
                break;
            case DECIMAL:
            case BIG_DECIMAL:
                valueObject = value.getDecimal();
                break;
            case LONG:
            case BYTE:
            case INTEGER:
            case REAL:
            case BIGINT:
            case SHORT:
            case TINYINT:
            case SMALLINT:
            case BIG_INTEGER:
                valueObject = value.getLong();
                break;
            case BOOLEAN:
                valueObject = value.getBoolean();
                break;
            case BLOB:
            case CLOB:
            case OBJECT:
            case VARBINARY:
                valueObject = value.getBinary();
                break;
            default:
                throw new UnsupportedOperationException();
        }

        return (T) valueObject;
    }

    <T> T propertyValue(Property property, DataTypeName dataTypeName) throws RepositoryException {
        if (property == null)
            return null;

        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        return propertyValue(value, dataTypeName);
    }

    protected <T> T propertyValue(Node node, String propName, DataTypeName dataTypeName) throws RepositoryException {
        Property property = property(node, propName);
        if (property == null)
            return null;

        T value = propertyValue(property, dataTypeName);
        return value;
    }

    protected <T> Collection<T> propertyValues(Node node, String propName, DataTypeName dataTypeName) throws RepositoryException {
        Collection<T> collection = Collections.emptyList();
        Property property = property(node, propName);
        if (property == null)
            return collection;

        if (! property.isMultiple()) {

            T value = propertyValue(property, dataTypeName);
            if (value != null)
                collection = Collections.singleton(value);

        } else {

            List<T> values = new ArrayList<T>();
            for (Value valueObject : property.getValues()) {
                T value = propertyValue(valueObject, dataTypeName);
                if (value != null)
                    values.add(value);
            }

            collection = values;

        }

        return collection;
    }

    protected boolean isTeiidSqlType(NodeType nodeType) {
        if (nodeType == null)
            return false;

        return nodeType.getName().startsWith(TeiidSqlLexicon.Namespace.PREFIX + COLON);
    }

    protected boolean instanceOf(Node node, LexTokens superTypeToken) throws RepositoryException {
        if (node == null)
            return false;

        NodeType tsqlType = findTeiidSqlType(node);
        if (tsqlType == null)
            return false;

        LexTokens stToken = LexTokens.findClass(tsqlType.getName());
        if (superTypeToken.equals(stToken))
            return true;

        NodeType[] superTypes = tsqlType.getSupertypes();

        for (NodeType superType : superTypes) {
            if (! isTeiidSqlType(superType))
                continue;

            stToken = LexTokens.findClass(superType.getName());
            if (superTypeToken.equals(stToken))
                return true;
        }

        return false;
    }

    protected NodeType findTeiidSqlType(Node node) throws RepositoryException {
        NodeType[] mixinTypes = node.getMixinNodeTypes();
        if (mixinTypes.length == 0)
            return null;

        NodeType tsqlMixinType = null;
        for (NodeType mixinType : mixinTypes) {
            if (! isTeiidSqlType(mixinType))
                continue;

            tsqlMixinType = mixinType;
            break;
        }

        return tsqlMixinType;
    }

    protected void addWithClause(Node node) throws RepositoryException {
        Iterator<Node> withNodes = references(node, QueryCommand.WITH_REF_NAME);
        if (! withNodes.hasNext())
            return;

        appendToken(QueryCommand.WITH_REF_NAME);
        append(SPACE);
        iterate(node, QueryCommand.WITH_REF_NAME);
        beginClause(0);
    }

    protected boolean hasHint(Node node) throws RepositoryException {
        boolean optional = propertyBoolean(node, FromClause.OPTIONAL_PROP_NAME);
        boolean makeInd = propertyBoolean(node, FromClause.MAKE_IND_PROP_NAME);
        boolean makeNotDep = propertyBoolean(node, FromClause.MAKE_NOT_DEP_PROP_NAME);
        boolean noUnnest = propertyBoolean(node, FromClause.NO_UNNEST_PROP_NAME);
        boolean preserve = propertyBoolean(node, FromClause.PRESERVE_PROP_NAME);
        Node makeDep = reference(node, FromClause.MAKE_DEPENDENCY_REF_NAME);

        return optional || makeInd || makeNotDep || noUnnest || preserve || (makeDep != null);
    }

    protected boolean isMakeDepSimple(Node makeDep) throws RepositoryException {
        if (makeDep == null)
            return false;

        boolean hasMax = makeDep.hasProperty(MakeDep.MAX_PROP_NAME);
        boolean join = propertyBoolean(makeDep, MakeDep.JOIN_PROP_NAME);
  
        return !hasMax && !join;
    }
    
    protected void addHintComment(Node node) throws RepositoryException {

        boolean optional = propertyBoolean(node, FromClause.OPTIONAL_PROP_NAME);
        boolean makeInd = propertyBoolean(node, FromClause.MAKE_IND_PROP_NAME);
        boolean makeNotDep = propertyBoolean(node, FromClause.MAKE_NOT_DEP_PROP_NAME);
        boolean noUnnest = propertyBoolean(node, FromClause.NO_UNNEST_PROP_NAME);
        boolean preserve = propertyBoolean(node, FromClause.PRESERVE_PROP_NAME);
        Node makeDep = reference(node, FromClause.MAKE_DEPENDENCY_REF_NAME);

        if (hasHint(node)) {
            append(BEGIN_HINT);
            append(SPACE);

            if (optional) {
                append(OPTIONAL);
                append(SPACE);
            }

            if (makeDep != null && isMakeDepSimple(makeDep)) {
                append(MAKEDEP);
                append(SPACE);
            }

            if (makeNotDep) {
                append(MAKENOTDEP);
                append(SPACE);
            }

            if (makeInd) {
                append(MAKEIND);
                append(SPACE);
            }

            if (noUnnest) {
                append(NOUNNEST);
                append(SPACE);
            }

            if (preserve) {
                append(PRESERVE);
                append(SPACE);
            }

            append(END_HINT);
            append(SPACE);
        }
    }

    protected void addMakeDep(Node node) throws RepositoryException {
        if (isLessThanTeiidVersion(Version.TEIID_8_5))
            return;

        Node makeDep = reference(node, FromClause.MAKE_DEPENDENCY_REF_NAME);

        if (makeDep != null && !isMakeDepSimple(makeDep)) {
            append(SPACE);
            append(MAKEDEP);
            visit(makeDep);
        }
    }

    /**
     * Take a string literal and escape it as necessary. By default, this converts ' to ''.
     * 
     * @param str String literal value (unquoted), never null
     * @return Escaped string literal value
     */
    protected String escapeStringValue(String str, String tick) {
        return StringUtils.replaceAll(str, tick, tick + tick);
    }

    protected String escapeSinglePart(String token) {
        if (TeiidSQLConstants.isReservedWord(getVersion(), token)) {
            return ID_ESCAPE_CHAR + token + ID_ESCAPE_CHAR;
        }

        boolean escape = true;
        char start = token.charAt(0);
        if (start == '#' || start == '@' || StringUtils.isLetter(start)) {
            escape = false;
            for (int i = 1; !escape && i < token.length(); i++) {
                char c = token.charAt(i);
                escape = !StringUtils.isLetterOrDigit(c) && c != '_';
            }
        }

        if (escape) {
            return ID_ESCAPE_CHAR + escapeStringValue(token, SPEECH_MARK) + ID_ESCAPE_CHAR;
        }

        return token;
    }

    protected void appendDisplayName(String name) {
        String[] pathParts = name.split("\\."); //$NON-NLS-1$
        for (int i = 0; i < pathParts.length; i++) {
            if (i > 0) {
                append(org.komodo.spi.query.sql.symbol.Symbol.SEPARATOR);
            }

            append(escapeSinglePart(pathParts[i]));
        }
    }

    protected String shortName(String name) {
        int index = name.lastIndexOf(org.komodo.spi.query.sql.symbol.Symbol.SEPARATOR);
        if(index >= 0) {
            return name.substring(index + 1);
        }

        return name;
    }

    protected String outputName(Node node) throws RepositoryException {
        String name = propertyString(node,  Symbol.NAME_PROP_NAME);
        String outputName = propertyString(node,  Symbol.OUTPUT_NAME_PROP_NAME);
        return outputName == null ? name : outputName;
    }

    protected void appendNested(Node node) throws RepositoryException {
        boolean useParens = instanceOf(node, LexTokens.CRITERIA);
        if (useParens) {
            append(LPAREN);
        }

        visit(node);

        if (useParens) {
            append(RPAREN);
        }
    }

    protected void appendLiteral(Class<?> type, boolean multiValued, Object value) {
        Class<?> booleanClass = getDataTypeManager().getDefaultDataClass(DataTypeName.BOOLEAN);
        
        
        String[] constantParts = null;
        if (multiValued) {
            constantParts = new String[] {QUESTION_MARK}; 
        } else if (value == null) {
            
            if (booleanClass.equals(type)) {
                constantParts = new String[] {UNKNOWN};
            } else {
                constantParts = new String[] {NULL.toLowerCase()};
            }
        } else {

            if (isTeiid87OrGreater() && value instanceof java.sql.Array) {
                java.sql.Array av = (java.sql.Array) value; 
                append(LPAREN);

                try {
                    Object[] values = (Object[])av.getArray();
                    for (int i = 0; i < values.length; i++) {
                        if (i > 0) {
                            append(COMMA);
                            append(SPACE);
                        }

                        Object value2 = values[i];
                        appendLiteral(value2 != null ? value2.getClass() : values.getClass().getComponentType(), multiValued, value2);
                    }

                } catch (Exception ex) {
                    logger.error(ex.getMessage(), ex);
                    append(ERROR);
                }

                append(RPAREN);
                return;
            }

            if (Number.class.isAssignableFrom(type)) {
                constantParts = new String[] {value.toString()};
            } else if (booleanClass.equals(type)) {
                constantParts = new String[] {value.equals(Boolean.TRUE) ? TRUE : FALSE};
            } else if (type.equals(getDataTypeManager().getDefaultDataClass(DataTypeName.TIMESTAMP))) {
                constantParts = new String[] {OPEN_BRACE + "ts'", value.toString(), QUOTE_MARK + CLOSE_BRACE};  //$NON-NLS-1$
            } else if (type.equals(getDataTypeManager().getDefaultDataClass(DataTypeName.TIME))) {
                constantParts = new String[] {OPEN_BRACE + "t'", value.toString(), QUOTE_MARK + CLOSE_BRACE};  //$NON-NLS-1$
            } else if (type.equals(getDataTypeManager().getDefaultDataClass(DataTypeName.DATE))) {
                constantParts = new String[] {OPEN_BRACE + "d'", value.toString(), QUOTE_MARK + CLOSE_BRACE};  //$NON-NLS-1$
            } else if (type.equals(getDataTypeManager().getDefaultDataClass(DataTypeName.VARBINARY))) {
                constantParts = new String[] {"X'", value.toString(), QUOTE_MARK};  //$NON-NLS-1$
            }

            if (constantParts == null) {
                if (getDataTypeManager().isLOB(type)) {
                    constantParts = new String[] {QUESTION_MARK};
                } else {
                    String strValue = value.toString();
                    strValue = escapeStringValue(strValue, QUOTE_MARK);
                    constantParts = new String[] {QUOTE_MARK, strValue, QUOTE_MARK};
                }

            }
        }

        for (String string : constantParts) {
            append(string);
        }
    }

    protected void appendLabel(Node node) throws RepositoryException {
        String label = propertyString(node, Labeled.LABEL_PROP_NAME);
        if (label != null) {
            appendDisplayName(label);
            append(SPACE);
            append(COLON);
            append(SPACE);
        }
    }

    protected void appendStatements(Node node, String refName) throws RepositoryException {
        Iterator<Node> statements = references(node, refName);
        while (statements.hasNext()) {
            // Add each statement
            visit(statements.next());
            append(NEW_LINE);
        }
    }

    @Override
    public void visit(Property property) {
        // Not required
    }
    
    protected void visit(Node node, TeiidSqlContext context) throws RepositoryException {
        if (node == null) {
            append(undefined());
            return;
        }

        NodeType tsqlMixinType = findTeiidSqlType(node);
        if (tsqlMixinType == null) {
            visitChildren(node); // Not a tsql node but may contain them
            return;
        }

        LexTokens lexEnum = LexTokens.findClass(tsqlMixinType.getName());
        try {
            TeiidSqlLexicon.redirect(lexEnum, this, context);
        } catch (RepositoryException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new RepositoryException(ex);
        }
    }

    @Override
    public void visit(Node node) throws RepositoryException {
        visit(node, new TeiidSqlNodeContext(node));
    }

    @Override
    public Object nullNode(TeiidSqlContext context) throws Exception {
        append(undefined());

        return null;
    }

    @Override
    public Object criteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        String keyword = (String) context.get(KEYWORD_KEY);
        if (keyword != null) {
            append(keyword);
            append(SPACE);
        }

        visit(node);

        return null;
    }

    @Override
    public Object compareCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node leftExp = reference(node, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME);
        visit(leftExp);
        append(SPACE);

        String opString = propertyString(node, AbstractCompareCriteria.OPERATOR_PROP_NAME);
        CriteriaOperator.Operator operator = CriteriaOperator.Operator.findOperator(opString);
        append(operator.getSymbols().iterator().next());
        append(SPACE);

        Node rightExp = reference(node, CompareCriteria.RIGHT_EXPRESSION_REF_NAME);
        visit(rightExp);

        return null;
    }

    @Override
    public Object subqueryCompareCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node leftExp = reference(node, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME);
        visit(leftExp);

        String opString = propertyString(node, AbstractCompareCriteria.OPERATOR_PROP_NAME);
        CriteriaOperator.Operator operator = CriteriaOperator.Operator.findOperator(opString);

        String quantString = propertyString(node, SubqueryCompareCriteria.PREDICATE_QUANTIFIER_PROP_NAME);
        PredicateQuantifier quantifier = PredicateQuantifier.findPredicateQuantifier(quantString);

        // operator and beginning of list
        append(SPACE);
        append(operator.getSymbols().iterator().next());
        append(SPACE);
        append(quantifier.name());
        append(SPACE);

        append(OPEN_BRACKET);
        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        visit(command);
        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object setCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node expression = reference(node, AbstractSetCriteria.EXPRESSION_REF_NAME);
        appendNested(expression);

        // operator and beginning of list
        append(SPACE);

        boolean negated = propertyBoolean(node, AbstractSetCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(IN);
        append(SPACE + OPEN_BRACKET);

        // value list
        iterate(node, SetCriteria.VALUES_REF_NAME);
        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object subquerySetCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node expression = reference(node, AbstractSetCriteria.EXPRESSION_REF_NAME);
        visit(expression);

        // operator and beginning of list
        append(SPACE);

        boolean negated = propertyBoolean(node, AbstractSetCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(IN);

        Node subqueryHint = reference(node, SubquerySetCriteria.SUBQUERY_HINT_REF_NAME);
        visit(subqueryHint);

        append(SPACE + OPEN_BRACKET);
        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        visit(command);
        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object betweenCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node expression = reference(node, BetweenCriteria.EXPRESSION_REF_NAME);
        visit(expression);
        append(SPACE);

        boolean negated = propertyBoolean(node,  BetweenCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(BETWEEN);
        append(SPACE);

        Node lowerExpression = reference(node, BetweenCriteria.LOWER_EXPRESSION_REF_NAME);
        visit(lowerExpression);

        append(SPACE);
        append(AND);
        append(SPACE);

        Node upperExpression = reference(node, BetweenCriteria.UPPER_EXPRESSION_REF_NAME);
        visit(upperExpression);

        return null;
    }

    @Override
    public Object compoundCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        // Get operator string
        long operator = propertyLong(node, CompoundCriteria.OPERATOR_PROP_NAME);
        String operatorStr = EMPTY_STRING;
        if (operator == org.komodo.spi.query.sql.lang.CompoundCriteria.AND) {
            operatorStr = AND;
        } else if (operator == org.komodo.spi.query.sql.lang.CompoundCriteria.OR) {
            operatorStr = OR;
        }

        // Get criteria

        Iterator<Node> criteria = references(node, CompoundCriteria.CRITERIA_REF_NAME);
        int size = size(node, CompoundCriteria.CRITERIA_REF_NAME);

        // Build parts
        if (size == 1) {
            // Special case - should really never happen, but we are tolerant
            visit(criteria.next());
        } else {
            // Add first criteria

            for (int i = 0; criteria.hasNext(); ++i) {
                if (i > 0) {
                    // Add connector
                    append(SPACE);
                    append(operatorStr);
                    append(SPACE);
                }

                // Add criteria
                Node crit = criteria.next();
                append(LPAREN);
                visit(crit);
                append(RPAREN);
            }
        }

        return null;
    }

    @Override
    public Object existsCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean negated = propertyBoolean(node,  ExistsCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(EXISTS);

        Node subqueryHint = reference(node, ExistsCriteria.SUBQUERY_HINT_REF_NAME);
        visit(subqueryHint);

        append(SPACE + OPEN_BRACKET);
        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        visit(command);
        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object expressionCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        
        Node expression = reference(node, ExpressionCriteria.EXPRESSION_REF_NAME);
        visit(expression);

        return null;
    }

    @Override
    public Object isNullCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node expression = reference(node, IsNullCriteria.EXPRESSION_REF_NAME);
        appendNested(expression);

        append(SPACE);
        append(IS);
        append(SPACE);

        boolean negated = propertyBoolean(node,  IsNullCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(NULL);

        return null;
    }

    @Override
    public Object matchCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node leftExpression = reference(node, MatchCriteria.LEFT_EXPRESSION_REF_NAME);
        visit(leftExpression);

        append(SPACE);
        boolean negated = propertyBoolean(node,  MatchCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        String modeString = propertyString(node,  MatchCriteria.MODE_PROP_NAME);
        MatchMode mode = org.komodo.spi.query.sql.lang.MatchCriteria.MatchMode.findMatchMode(modeString);
        switch (mode) {
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

        Node rightExpression = reference(node, MatchCriteria.RIGHT_EXPRESSION_REF_NAME);
        visit(rightExpression);

        String escapeChar = propertyString(node,  MatchCriteria.ESCAPE_CHAR_PROP_NAME);
        if (! Character.toString(org.komodo.spi.query.sql.lang.MatchCriteria.NULL_ESCAPE_CHAR).equals(escapeChar)) {
            append(SPACE);
            append(ESCAPE);
            append(SPACE);
            appendLiteral(String.class, false, escapeChar);
        }

        return null;
    }

    @Override
    public Object notCriteria(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(NOT);
        append(SPACE + OPEN_BRACKET);

        Node criteria = reference(node, NotCriteria.CRITERIA_REF_NAME);
        visit(criteria);

        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object alterProcedure(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(ALTER);
        append(SPACE);
        append(PROCEDURE);
        append(SPACE);

        Node target = reference(node, Alter.TARGET_REF_NAME);
        visit(target);

        beginClause(1);
        append(AS);

        Node definition = reference(node, Alter.DEFINITION_REF_NAME);
        if (instanceOf(definition, LexTokens.CREATE_PROCEDURE_COMMAND)) {
            Node defnBlock = reference(definition, CreateProcedureCommand.BLOCK_REF_NAME);
            visit(defnBlock);
        }

        return null;
    }

    @Override
    public Object alterTrigger(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean create = propertyBoolean(node, AlterTrigger.CREATE_PROP_NAME);
        if (create) {
            append(CREATE);
        } else {
            append(ALTER);
        }

        append(SPACE);
        append(TRIGGER);
        append(SPACE);
        append(ON);
        append(SPACE);

        Node target = reference(node, Alter.TARGET_REF_NAME);
        visit(target);

        beginClause(0);
        append(INSTEAD);
        append(SPACE);
        append(OF);
        append(SPACE);

        String eventName = propertyString(node, AlterTrigger.EVENT_PROP_NAME);
        TriggerEvent triggerEvent = TriggerEvent.findTriggerEvent(eventName);
        append(triggerEvent.name());

        Node definition = reference(node, Alter.DEFINITION_REF_NAME);
        if (definition != null) {
            beginClause(0);
            append(AS);
            append(NEW_LINE);
            visit(definition);
        } else {
            append(SPACE);

            boolean enabled = propertyBoolean(node, AlterTrigger.ENABLED_PROP_NAME);
            append(enabled ? ENABLED : DISABLED);
        }

        return null;
    }

    @Override
    public Object alterView(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(ALTER);
        append(SPACE);
        append(VIEW);
        append(SPACE);

        Node target = reference(node, Alter.TARGET_REF_NAME);
        visit(target);

        beginClause(0);
        append(AS);
        append(NEW_LINE);

        Node definition = reference(node, Alter.DEFINITION_REF_NAME);
        visit(definition);

        return null;
    }

    @Override
    public Object delete(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(DELETE);

        Node sourceHint = reference(node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(sourceHint);

        append(SPACE);

        // add from clause
        append(FROM);
        append(SPACE);

        Node group = reference(node, TargetedCommand.GROUP_REF_NAME);
        visit(group);

        // add where clause
        Node criteria = reference(node, Delete.CRITERIA_REF_NAME);
        if (criteria != null) {
            beginClause(0);

            TeiidSqlContext ccontext = new TeiidSqlNodeContext(criteria);
            ccontext.add(KEYWORD_KEY, WHERE);
            criteria(ccontext);
        }

        // Option clause
        Node option = reference(node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(0);
            visit(option);
        }

        return null;
    }

    @Override
    public Object insert(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean merge = propertyBoolean(node, Insert.MERGE_PROP_NAME);
        if (merge) {
            append(MERGE);
        } else {
            append(INSERT);
        }

        Node sourceHint = reference(node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(sourceHint);

        append(SPACE);
        append(INTO);
        append(SPACE);

        Node group = reference(node, TargetedCommand.GROUP_REF_NAME);
        visit(group);

        Iterator<Node> variables = references(node, Insert.VARIABLES_REF_NAME);
        int size = size(node, Insert.VARIABLES_REF_NAME);
        if (size > 0) {
            beginClause(2);

            // Columns clause
            append(OPEN_BRACKET);

            for (int i = 0; variables.hasNext(); i++) {
                Node variable = variables.next();
                if (i > 0) {
                    append(COMMA + SPACE);
                }

                TeiidSqlContext varContext = new TeiidSqlNodeContext(variable);
                varContext.add(SHORT_NAME_ONLY_KEY, true);
                visit(variable, varContext);
            }

            append(CLOSE_BRACKET);
        }

        beginClause(1);

        Node queryExp = reference(node, Insert.QUERY_EXPRESSION_REF_NAME);
        Iterator<Node> values = references(node, Insert.VALUES_REF_NAME);

        if (queryExp != null) {
            visit(queryExp);
        } else if (values.hasNext()) {
            append(VALUES);
            beginClause(2);
            append(OPEN_BRACKET);
            iterate(node, Insert.VALUES_REF_NAME);
            append(CLOSE_BRACKET);
        }

        // Option clause
        Node option = reference(node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(option);
        }

        return null;
    }

    @Override
    public Object storedProcedure(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean calledWithReturn = propertyBoolean(node, StoredProcedure.CALLED_WITH_RETURN_PROP_NAME);
        boolean displayNamedParams = propertyBoolean(node, StoredProcedure.DISPLAY_NAMED_PARAMETERS_PROP_NAME);

        if (calledWithReturn) {
            Iterator<Node> parameters = references(node, StoredProcedure.PARAMETERS_REF_NAME);

            while (parameters.hasNext()) {
                Node parameter = parameters.next();
                long paramType = propertyLong(parameter, SPParameter.PARAMETER_TYPE_PROP_NAME);
                if (paramType == ParameterInfo.RETURN_VALUE.index()) {
                    Node expression = reference(parameter, SPParameter.EXPRESSION_REF_NAME);
                    if (expression == null) {
                        append(QUESTION_MARK);
                    } else {
                        visit(expression);
                    }

                }

            }

            append(SPACE);
            append(EQ);
            append(SPACE);
        }

        // exec clause
        append(EXEC);
        append(SPACE);

        String procedureName = propertyString(node, StoredProcedure.PROCEDURE_NAME_PROP_NAME);
        append(procedureName);

        append(OPEN_BRACKET);

        boolean first = true;
        Iterator<Node> parameters = references(node, StoredProcedure.PARAMETERS_REF_NAME);
        while (parameters.hasNext()) {
            Node parameter = parameters.next();
            
            boolean usingDefault = propertyBoolean(parameter, SPParameter.USING_DEFAULT_PROP_NAME);
            long paramType = propertyLong(parameter, SPParameter.PARAMETER_TYPE_PROP_NAME);
            Node expression = reference(parameter, SPParameter.EXPRESSION_REF_NAME);

            if (usingDefault)
                continue;
            if (paramType == ParameterInfo.RETURN_VALUE.index() || 
                    paramType == ParameterInfo.RESULT_SET.index())
                continue;
            
            if(expression == null)
                continue;

            if (first)
                first = false;
            else
                append(COMMA + SPACE);

            if (displayNamedParams) {
                String paramName = propertyString(parameter, SPParameter.NAME_PROP_NAME);
                append(escapeSinglePart(shortName(paramName)));
                append(SPACE + EQUALS + CLOSE_ANGLE_BRACKET + SPACE);
            }

            boolean addParens = displayNamedParams && instanceOf(expression, LexTokens.COMPARE_CRITERIA);
            if (addParens) {
                append(LPAREN);
            }

            visit(expression);

            if (addParens) {
                append(RPAREN);
            }
        }

        append(CLOSE_BRACKET);

        // Option clause
        Node option = reference(node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(option);
        }

        return null;
    }

    @Override
    public Object update(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(UPDATE);

        Node sourceHint = reference(node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(sourceHint);

        append(SPACE);
        
        Node group = reference(node, TargetedCommand.GROUP_REF_NAME);
        visit(group);

        beginClause(1);

        // Set clause
        append(SET);
        beginClause(2);

        Node changeList = reference(node, Update.CHANGE_LIST_REF_NAME);
        visit(changeList);

        // add where clause
        Node criteria = reference(node, Update.CRITERIA_REF_NAME);
        if (criteria != null) {
            beginClause(0);

            TeiidSqlContext ccontext = new TeiidSqlNodeContext(criteria);
            ccontext.add(KEYWORD_KEY, WHERE);
            criteria(ccontext);
        }

        // Option clause
        Node option = reference(node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(option);
        }

        return null;
    }

    @Override
    public Object dynamicCommand(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(EXECUTE);
        append(SPACE);
        append(IMMEDIATE);
        append(SPACE);

        Node sql = reference(node, DynamicCommand.SQL_REF_NAME);
        visit(sql);

        boolean asClauseSet = propertyBoolean(node, DynamicCommand.AS_CLAUSE_SET_PROP_NAME);
        if (asClauseSet) {
            beginClause(1);
            append(AS);
            append(SPACE);

            Iterator<Node> asColumns = references(node, DynamicCommand.AS_COLUMNS_REF_NAME);
            for (int i = 0; asColumns.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                Node asColumn = asColumns.next();
                String asColName = propertyString(asColumn, Symbol.NAME_PROP_NAME);

                append(shortName(asColName));
                append(SPACE);

                String typeName = propertyString(asColumn, Expression.TYPE_CLASS_PROP_NAME);
                DataTypeName dataTypeName = DataTypeName.findDataTypeName(typeName);
                Class<?> dataTypeClass = getDataTypeManager().getDefaultDataClass(dataTypeName);
                append(getDataTypeManager().getDataTypeName(dataTypeClass));
            }
        }

        Node intoGroup = reference(node, DynamicCommand.INTO_GROUP_REF_NAME);
        if (intoGroup != null) {
            beginClause(1);
            append(INTO);
            append(SPACE);
            visit(intoGroup);
        }

        Node using = reference(node, DynamicCommand.USING_REF_NAME);
        Iterator<Node> usingList = references(using, SetClauseList.SET_CLAUSES_REF_NAME);
        if (using != null && usingList.hasNext()) {
            beginClause(1);
            append(USING);
            append(SPACE);
            visit(using);
        }

        long updatingModelCount = propertyLong(node, DynamicCommand.UPDATING_MODEL_COUNT_PROP_NAME);
        if (updatingModelCount > 0) {
            beginClause(1);
            append(UPDATE);
            append(SPACE);

            if (updatingModelCount > 1) {
                append(STAR);
            } else {
                append(Integer.toString(1));
            }
        }

        return null;
    }

    @Override
    public Object query(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        addWithClause(node);
        appendToken(Select.ID);

        Node sourceHint = reference(node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(sourceHint);

        Node select = reference(node, Query.SELECT_REF_NAME);
        if (select != null)
            visit(select);

        Node into = reference(node, Query.INTO_REF_NAME);
        if (into != null) {
            beginClause(1);
            visit(into);
        }

        Node from = reference(node, Query.FROM_REF_NAME);
        if (from != null) {
            beginClause(1);
            visit(from);
        }

        // Where clause
        Node criteria = reference(node, Query.CRITERIA_REF_NAME);
        if (criteria != null) {
            beginClause(1);

            TeiidSqlContext ccontext = new TeiidSqlNodeContext(criteria);
            ccontext.add(KEYWORD_KEY, WHERE);
            criteria(ccontext);
        }

        // Group by clause
        Node groupBy = reference(node, Query.GROUP_BY_REF_NAME);
        if (groupBy != null) {
            beginClause(1);
            visit(groupBy);
        }

        // Having clause
        Node having = reference(node, Query.HAVING_REF_NAME);
        if (having != null) {
            beginClause(1);
            TeiidSqlContext hcontext = new TeiidSqlNodeContext(having);
            hcontext.add(KEYWORD_KEY, HAVING);
            criteria(hcontext);
        }

        // Order by clause
        Node orderBy = reference(node, QueryCommand.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            beginClause(1);
            visit(orderBy);
        }

        Node limit = reference(node, QueryCommand.LIMIT_REF_NAME);
        if (limit != null) {
            beginClause(1);
            visit(limit);
        }

        // Option clause
        Node option = reference(node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(option);
        }

        return null;
    }

    protected void appendSetQuery(Node parent, Node queryCommand, boolean right) throws RepositoryException {
        
        Node limit = reference(queryCommand, QueryCommand.LIMIT_REF_NAME);
        Node orderBy = reference(queryCommand, QueryCommand.ORDER_BY_REF_NAME);

        boolean isSetQuery = instanceOf(queryCommand, LexTokens.SET_QUERY);
        
        boolean parentIsAll = propertyBoolean(parent, SetQuery.ALL_PROP_NAME);
        boolean cmdIsAll = propertyBoolean(queryCommand, SetQuery.ALL_PROP_NAME);

        String parentOpName = propertyString(parent, SetQuery.OPERATION_PROP_NAME);
        Operation parentOp = Operation.findOperation(parentOpName);

        String cmdOpName = propertyString(queryCommand, SetQuery.OPERATION_PROP_NAME);
        Operation cmdOp = Operation.findOperation(cmdOpName);

        if (limit != null || orderBy != null ||
            (right && 
                ((isSetQuery && 
                    (parentIsAll && !(cmdIsAll) || parentOp.equals(cmdOp) ))))) {
            append(LPAREN);
            visit(queryCommand);
            append(RPAREN);
        } else {
            visit(queryCommand);
        }
    }

    @Override
    public Object setQuery(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addWithClause(node);

        Node queryCommand = reference(node, SetQuery.LEFT_QUERY_REF_NAME);
        appendSetQuery(node, queryCommand, false);

        beginClause(0);

        String opName = propertyString(node, SetQuery.OPERATION_PROP_NAME);
        Operation op = Operation.findOperation(opName);
        append(op.name());

        boolean isAll = propertyBoolean(node, SetQuery.ALL_PROP_NAME);
        if (isAll) {
            append(SPACE);
            append(ALL);
        }

        beginClause(0);

        Node rightQuery = reference(node, SetQuery.RIGHT_QUERY_REF_NAME);
        appendSetQuery(node, rightQuery, true);

        // Order by clause
        Node orderBy = reference(node, QueryCommand.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            beginClause(1);
            visit(orderBy);
        }

        Node limit = reference(node, QueryCommand.LIMIT_REF_NAME);
        if (limit != null) {
            beginClause(1);
            visit(limit);
        }

        // Option clause
        Node option = reference(node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(option);
        }

        return null;
    }

    @Override
    public Object createProcedureCommand(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        if (isLessThanTeiidVersion(Version.TEIID_8_4)) {
            append(CREATE);
            append(SPACE);
            append(VIRTUAL);
            append(SPACE);
            append(PROCEDURE);
            append(NEW_LINE);
        }

        Node block = reference(node, CreateProcedureCommand.BLOCK_REF_NAME);
        visit(block);

        return null;
    }

    @Override
    public Object triggerAction(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(FOR);
        append(SPACE);
        append(EACH);
        append(SPACE);
        append(ROW);
        append(NEW_LINE);

        Node block = reference(node, TriggerAction.BLOCK_REF_NAME);
        visit(block);

        return null;
    }

    @Override
    public Object arrayTable(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addHintComment(node);

        append(ARRAYTABLE);
        append(OPEN_BRACKET);

        Node arrayValue = reference(node, ArrayTable.ARRAY_VALUE_REF_NAME);
        visit(arrayValue);

        append(SPACE);
        append(COLUMNS);

        iterate(node, ArrayTable.COLUMNS_REF_NAME);

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(node);

        return null;
    }

    @Override
    public Object objectTable(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addHintComment(node);

        append(OBJECTTABLE);
        append(OPEN_BRACKET);

        String scriptLanguage = propertyString(node, ObjectTable.SCRIPTING_LANGUAGE_PROP_NAME);
        if (scriptLanguage != null) {
            append(LANGUAGE);
            append(SPACE);
            append(scriptLanguage);
            append(SPACE);
        }

        String rowScript = propertyString(node, ObjectTable.ROW_SCRIPT_PROP_NAME);
        appendLiteral(String.class, false, rowScript);

        Iterator<Node> passings = references(node, ObjectTable.PASSING_REF_NAME);
        if (passings.hasNext()) {
            append(SPACE);
            append(PASSING);
            append(SPACE);
            iterate(node, ObjectTable.PASSING_REF_NAME);
        }

        append(SPACE);
        append(COLUMNS);

        Iterator<Node> columns = references(node, ObjectTable.COLUMNS_REF_NAME);
        for (int i = 0; columns.hasNext(); ++i) {
            if (i > 0)
                append(COMMA + SPACE);

            visit(columns.next());
        }

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(node);

        return null;
    }

    @Override
    public Object textTable(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addHintComment(node);

        append(TEXTTABLE);
        append(OPEN_BRACKET);

        Node fileExp = reference(node, TextTable.FILE_REF_NAME);
        visit(fileExp);

        String selector = propertyString(node, TextTable.SELECTOR_PROP_NAME);
        if (selector != null) {
            append(SPACE);
            append(SELECTOR);
            append(SPACE);
            append(escapeSinglePart(selector));
        }

        append(SPACE);
        append(COLUMNS);

        Iterator<Node> columns = references(node, TextTable.COLUMNS_REF_NAME);
        for (int i = 0; columns.hasNext(); ++i) {
            if (i > 0)
                append(COMMA);

            visit(columns.next());
        }

        boolean usingRowDelimiter = propertyBoolean(node, TextTable.USING_ROW_DELIMITER_PROP_NAME);
        if (!usingRowDelimiter) {
            append(SPACE);
            append(NO);
            append(SPACE);
            append(ROW);
            append(SPACE);
            append(DELIMITER);
        }

        String delimiter = propertyString(node, TextTable.DELIMITER_PROP_NAME);
        if (delimiter != null) {
            append(SPACE);
            append(DELIMITER);
            append(SPACE);
            appendLiteral(String.class, false, delimiter);
        }

        String quote = propertyString(node, TextTable.QUOTE_PROP_NAME);
        if (quote != null) {
            append(SPACE);

            boolean escape = propertyBoolean(node, TextTable.ESCAPE_PROP_NAME);
            if (escape) {
                append(ESCAPE);
            } else {
                append(QUOTE);
            }

            append(SPACE);
            appendLiteral(String.class, false, quote);
        }

        if (node.hasProperty(TextTable.HEADER_PROP_NAME)) {
            append(SPACE);
            append(HEADER);
            long header = propertyLong(node, TextTable.HEADER_PROP_NAME);
            if (1 != header) {
                append(SPACE);
                append(Long.toString(header));
            }
        }

        if (node.hasProperty(TextTable.SKIP_PROP_NAME)) {
            append(SPACE);
            append(SKIP);
            append(SPACE);

            long skip = propertyLong(node, TextTable.SKIP_PROP_NAME);
            append(Long.toString(skip));
        }

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(node);

        return null;
    }

    @Override
    public Object xmlTable(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addHintComment(node);

        append(XMLTABLE);
        append(OPEN_BRACKET);

        Node namespaces = reference(node, XMLTable.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            visit(namespaces);
            append(COMMA);
            append(SPACE);
        }

        String xquery = propertyString(node, XMLTable.XQUERY_PROP_NAME);
        appendLiteral(String.class, false, xquery);

        Iterator<Node> passings = references(node, XMLTable.PASSING_REF_NAME);
        if (passings.hasNext()) {
            append(SPACE);
            append(PASSING);
            append(SPACE);
            iterate(node, XMLTable.PASSING_REF_NAME);
        }

        Iterator<Node> columns = references(node, XMLTable.COLUMNS_REF_NAME);
        boolean usingDefColumn = propertyBoolean(node, XMLTable.USING_DEFAULT_COLUMN_PROP_NAME);

        if (columns.hasNext() && !usingDefColumn) {
            append(SPACE);
            append(COLUMNS);

            for (int i = 0; columns.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                visit(columns.next());
            }
        }

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(node);

        return null;
    }

    @Override
    public Object joinPredicate(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addHintComment(node);
        boolean hasHint = hasHint(node);

        if (hasHint) {
            append(OPEN_BRACKET);
        }

        // left clause
        Node leftClause = reference(node, JoinPredicate.LEFT_CLAUSE_REF_NAME);
        if (instanceOf(leftClause, LexTokens.JOIN_PREDICATE) && !hasHint(leftClause)) {
            append(OPEN_BRACKET);
            visit(leftClause);
            append(CLOSE_BRACKET);
        } else
            visit(leftClause);

        // join type
        append(SPACE);

        Node joinType = reference(node, JoinPredicate.JOIN_TYPE_REF_NAME);
        visit(joinType);
        append(SPACE);

        // right clause
        Node rightClause = reference(node, JoinPredicate.RIGHT_CLAUSE_REF_NAME);
        if (instanceOf(rightClause, LexTokens.JOIN_PREDICATE) && !hasHint(rightClause)) {
            append(OPEN_BRACKET);
            visit(rightClause);
            append(CLOSE_BRACKET);
        } else
            visit(rightClause);

        // join criteria
        Iterator<Node> criterions = references(node, JoinPredicate.JOIN_CRITERIA_REF_NAME);
        int size = size(node, JoinPredicate.JOIN_CRITERIA_REF_NAME);
        System.out.println(size);
        if (criterions.hasNext()) {
            append(SPACE);
            append(ON);
            append(SPACE);
            for (int i = 0; criterions.hasNext(); ++i) {
                if (i > 0) {
                    append(SPACE);
                    append(AND);
                    append(SPACE);
                }
                    
                Node criterion = criterions.next();
                if (instanceOf(criterion, LexTokens.PREDICATE_CRITERIA) || instanceOf(criterion, LexTokens.NOT_CRITERIA)) {
                    visit(criterion);
                } else {
                    append(OPEN_BRACKET);
                    visit(criterion);
                    append(CLOSE_BRACKET);
                }

            }
        }

        if (hasHint) {
            append(CLOSE_BRACKET);
        }

        addMakeDep(node);

        return null;
    }

    @Override
    public Object subqueryFromClause(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addHintComment(node);

        boolean table = propertyBoolean(node, SubqueryFromClause.TABLE_PROP_NAME);
        if (table) {
            append(TABLE);
        }

        append(OPEN_BRACKET);

        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        visit(command);

        append(CLOSE_BRACKET);
        append(SPACE + AS+ SPACE);

        String name = propertyString(node, SubqueryFromClause.NAME_PROP_NAME);
        append(escapeSinglePart(name));

        addMakeDep(node);

        return null;
    }

    @Override
    public Object unaryFromClause(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        addHintComment(node);

        Node group = reference(node, UnaryFromClause.GROUP_REF_NAME);
        visit(group);
        addMakeDep(node);
        return null;
    }

    @Override
    public Object from(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        appendToken(node);
        beginClause(1);
        
        iterate(node, From.CLAUSES_REF_NAME);

        return null;
    }

    @Override
    public Object groupBy(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(GROUP);
        append(SPACE);
        append(BY);
        append(SPACE);

        boolean rollup = propertyBoolean(node, GroupBy.ROLLUP_PROP_NAME);
        if (isTeiidVersionOrGreater(Version.TEIID_8_5) && rollup) {
            append(ROLLUP);
            append(LPAREN);
        }

        iterate(node, GroupBy.SYMBOLS_REF_NAME);

        if (isTeiidVersionOrGreater(Version.TEIID_8_5) && rollup) {
            append(RPAREN);
        }

        return null;
    }

    @Override
    public Object into(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        appendToken(node);
        append(SPACE);
        Node group = reference(node, Into.GROUP_REF_NAME);
        visit(group);

        return null;
    }

    @Override
    public Object joinType(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String kindName = propertyString(node, JoinType.KIND_PROP_NAME);
        Types kind = Types.findType(kindName);

        append(kind.toPrintStatement());

        return null;
    }

    @Override
    public Object limit(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean strict = propertyBoolean(node, Limit.STRICT_PROP_NAME);
        if (!strict) {
            append(BEGIN_HINT);
            append(SPACE);
            append(NON_STRICT);
            append(SPACE);
            append(END_HINT);
            append(SPACE);
        }

        Node offset = reference(node, Limit.OFFSET_REF_NAME);
        Node rowLimit = reference(node, Limit.ROW_LIMIT_REF_NAME);
        if (rowLimit == null) {
            append(OFFSET);
            append(SPACE);            
            visit(offset);
            append(SPACE);
            append(ROWS);
        } else {

            append(LIMIT);
            if (offset != null) {
                append(SPACE);
                visit(offset);
                append(COMMA);
            }

            append(SPACE);
            visit(rowLimit);
        }

        return null;
    }

    @Override
    public Object namespaceItem(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String prefix = propertyString(node, NamespaceItem.PREFIX_PROP_NAME);
        String uri = propertyString(node, NamespaceItem.URI_PROP_NAME);

        if (prefix == null) {
            if (uri == null) {
                append(NO_DEFAULT);
            } else {
                append(DEFAULT + SPACE);
                appendLiteral(String.class, false, uri);
            }
        } else {
            appendLiteral(String.class, false, uri);
            append(SPACE + AS + SPACE);
            appendLiteral(String.class, false, prefix);
        }

        return null;
    }

    @Override
    public Object projectedColumn(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String name = propertyString(node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(node, ProjectedColumn.TYPE_PROP_NAME);
        
        append(SPACE);
        appendDisplayName(name);
        append(SPACE);
        append(type);

        return null;
    }

    @Override
    public Object objectColumn(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String name = propertyString(node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(node, ProjectedColumn.TYPE_PROP_NAME);
        String path = propertyString(node, ObjectColumn.PATH_PROP_NAME);

        append(SPACE);
        appendDisplayName(name);
        append(SPACE);
        append(type);
        append(SPACE);

        appendLiteral(String.class, false, path);

        Node defaultExp = reference(node, ObjectColumn.DEFAULT_EXPRESSION_REF_NAME);
        if (defaultExp != null) {
            append(SPACE);
            append(DEFAULT);
            append(SPACE);
            visit(defaultExp);
        }

        return null;
    }

    @Override
    public Object textColumn(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String name = propertyString(node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(node, ProjectedColumn.TYPE_PROP_NAME);
        boolean ordinal = propertyBoolean(node, TextColumn.ORDINAL_PROP_NAME);

        append(SPACE);
        appendDisplayName(name);
        append(SPACE);

        if (ordinal) {
            // Will only ever come in here if Teiid 8.7 or greater
            append(FOR);
            append(SPACE);
            append(ORDINALITY);
        } else {
            append(type);

            String width = propertyString(node, TextColumn.WIDTH_PROP_NAME);
            if (width != null) {
                append(SPACE);
                append(WIDTH);
                append(SPACE);
                append(width);
            }

            boolean noTrim = propertyBoolean(node, TextColumn.NO_TRIM_PROP_NAME);
            if (noTrim) {
                append(SPACE);
                append(NO);
                append(SPACE);
                append(TRIM);
            }

            String colSelector = propertyString(node, TextColumn.SELECTOR_PROP_NAME);
            if (colSelector != null) {
                append(SPACE);
                append(SELECTOR);
                append(SPACE);
                append(escapeSinglePart(colSelector));
                append(SPACE);

                long position = propertyLong(node, TextColumn.POSITION_PROP_NAME);
                append(Long.toString(position));
            }
        }

        return null;
    }

    @Override
    public Object xmlColumn(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        
        String name = propertyString(node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(node, ProjectedColumn.TYPE_PROP_NAME);
        String path = propertyString(node, XMLColumn.PATH_PROP_NAME);
        Node defaultExp = reference(node, XMLColumn.DEFAULT_EXPRESSION_REF_NAME);
        boolean ordinal = propertyBoolean(node, XMLColumn.ORDINAL_PROP_NAME);

        append(SPACE);
        appendDisplayName(name);
        append(SPACE);

        if (ordinal) {
            append(FOR);
            append(SPACE);
            append(ORDINALITY);
        } else {
            append(type);

            if (defaultExp != null) {
                append(SPACE);
                append(DEFAULT);
                append(SPACE);
                visit(defaultExp);
            }

            if (path != null) {
                append(SPACE);
                append(PATH);
                append(SPACE);
                appendLiteral(String.class, false, path);
            }
        }

        return null;
    }

    @Override
    public Object makeDep(TeiidSqlContext context) throws Exception {
        if (isLessThanTeiidVersion(Version.TEIID_8_5))
            return null;

        Node node = (Node) context.get(NODE_KEY);

        boolean hasMax = node.hasProperty(MakeDep.MAX_PROP_NAME);
        boolean join = propertyBoolean(node, MakeDep.JOIN_PROP_NAME);

        boolean parens = false;
        if (hasMax || join) {
            append(LPAREN);
            parens = true;
        }

        boolean space = false;

        if (hasMax) {
            if (space) {
                append(SPACE);
            } else {
                space = true;
            }

            append(MAX);
            append(COLON);

            long max = propertyLong(node, MakeDep.MAX_PROP_NAME);
            append(Long.toString(max));
        }

        if (join) {
            if (space) {
                append(SPACE);
            } else {
                space = true;
            }

            append(JOIN);
        }

        if (parens) {
            append(RPAREN);
        }

        return null;
    }

    @Override
    public Object option(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(OPTION);

        Collection<String> groups = propertyValues(node, Option.DEPENDENT_GROUPS_PROP_NAME, DataTypeName.STRING);
        if (groups != null && !groups.isEmpty()) {
            append(SPACE);
            append(MAKEDEP);
            append(SPACE);

            Iterator<String> iter = groups.iterator();
            Iterator<Node> groupOptions = references(node, Option.DEPENDENT_GROUP_OPTIONS_REF_NAME);
            for(int i = 0; iter.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                appendDisplayName(iter.next());

                if (groupOptions.hasNext())
                    visit(groupOptions.next());

            }
        }

        Collection<String> notGroups = propertyValues(node, Option.NOT_DEPENDENT_GROUPS_PROP_NAME, DataTypeName.STRING);
        if (notGroups != null && !notGroups.isEmpty()) {
            append(SPACE);
            append(MAKENOTDEP);
            append(SPACE);

            Iterator<String> iter = groups.iterator();
            for (int i = 0; iter.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                appendDisplayName(iter.next());
            }
        }

        boolean noCache = propertyBoolean(node, Option.NO_CACHE_PROP_NAME);
        Collection<String> noCacheGroups = propertyValues(node, Option.NO_CACHE_GROUPS_PROP_NAME, DataTypeName.STRING);
        if (noCacheGroups != null && !noCacheGroups.isEmpty()) {
            append(SPACE);
            append(NOCACHE);
            append(SPACE);

            Iterator<String> iter = noCacheGroups.iterator();

            for (int i = 0; iter.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                appendDisplayName(iter.next());
            }
        } else if (noCache) {
            append(SPACE);
            append(NOCACHE);
        }

        return null;
    }

    @Override
    public Object orderBy(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(ORDER);
        append(SPACE);
        append(BY);
        append(SPACE);

        iterate(node, OrderBy.ORDER_BY_ITEMS_REF_NAME);

        return null;
    }

    @Override
    public Object orderByItem(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node ses = reference(node, OrderByItem.SYMBOL_REF_NAME);
        if (instanceOf(ses, LexTokens.ALIAS_SYMBOL)) {
            appendDisplayName(outputName(ses));
        } else {
            visit(ses);
        }

        boolean ascending = propertyBoolean(node, OrderByItem.ASCENDING_PROP_NAME);
        if (!ascending) {
            append(SPACE);
            append(DESC);
        } // Don't print default "ASC"

        String nullOrderingName = propertyString(node, OrderByItem.NULL_ORDERING_PROP_NAME);
        NullOrdering nullOrdering = SortSpecification.NullOrdering.findNullOrdering(nullOrderingName);
        if (nullOrdering != null) {
            append(SPACE);
            append(NULLS);
            append(SPACE);
            append(nullOrdering.name());
        }

        return null;
    }

    @Override
    public Object spParameter(TeiidSqlContext context) {
        return null;
    }

    @Override
    public Object select(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean distinct = propertyBoolean(node, Select.DISTINCT_PROP_NAME);
        if (distinct) {
            append(SPACE);
            append(DISTINCT);
        }

        append(SPACE);

        iterate(node, Select.SYMBOLS_REF_NAME);

        return null;
    }

    @Override
    public Object setClause(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node symbol = reference(node, SetClause.SYMBOL_REF_NAME);
        String name = propertyString(symbol, Symbol.NAME_PROP_NAME);
        append(shortName(name));

        append(SPACE + EQUALS + SPACE);

        Node value = reference(node, SetClause.VALUE_REF_NAME);
        visit(value);

        return null;
    }

    @Override
    public Object setClauseList(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        iterate(node, SetClauseList.SET_CLAUSES_REF_NAME);

        return null;
    }

    protected void appendSourceHintValue(String sh) {
        append(COLON);
        append(QUOTE_MARK);
        append(escapeStringValue(sh, QUOTE_MARK));
        append(QUOTE_MARK);
        append(SPACE);
    }

    @Override
    public Object sourceHint(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(SPACE);
        append(BEGIN_HINT);
        append("sh"); //$NON-NLS-1$

        boolean useAliases = propertyBoolean(node, SourceHint.USE_ALIASES_PROP_NAME);
        if (useAliases) {
            append(SPACE);
            append("KEEP ALIASES"); //$NON-NLS-1$
        }

        String generalHint = propertyString(node, SourceHint.GENERAL_HINT_PROP_NAME);
        if (generalHint != null) {
            appendSourceHintValue(generalHint);
        } else {
            append(SPACE);
        }

        Iterator<Node> specificHints = references(node, SourceHint.SOURCE_HINTS_REF_NAME);
        while(specificHints.hasNext()) {
            visit(specificHints.next());
        }

        append(END_HINT);

        return null;
    }

    @Override
    public Object specificHint(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean useAliases = propertyBoolean(node, SpecificHint.USE_ALIASES_PROP_NAME);
        String translator = propertyString(node, SpecificHint.TRANSLATOR_NAME_PROP_NAME);
        String hint = propertyString(node, SpecificHint.HINT_PROP_NAME);
        
        append(translator);
        if (useAliases) {
            append(SPACE);
            append("KEEP ALIASES"); //$NON-NLS-1$
        }

        appendSourceHintValue(hint);

        return null;
    }

    @Override
    public Object subqueryHint(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean noUnnest = propertyBoolean(node, SubqueryHint.NO_UNNEST_PROP_NAME);
        boolean depJoin = propertyBoolean(node, SubqueryHint.DEP_JOIN_PROP_NAME);
        boolean mergeJoin = propertyBoolean(node, SubqueryHint.MERGE_JOIN_PROP_NAME);

        if (noUnnest) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(NOUNNEST);
            append(SPACE);
            append(END_HINT);
        } else if (depJoin) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(DJ);
            append(SPACE);
            append(END_HINT);
        } else if (mergeJoin) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(MJ);
            append(SPACE);
            append(END_HINT);
        }

        return null;
    }

    @Override
    public Object withQueryCommand(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node groupSymbol = reference(node, WithQueryCommand.GROUP_SYMBOL_REF_NAME);
        visit(groupSymbol);
        
        append(SPACE);

        Iterator<Node> columns = references(node, WithQueryCommand.COLUMNS_REF_NAME);
        if (columns != null && columns.hasNext()) {
            append(LPAREN);

            for(int i = 0; columns.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                Node column = columns.next();
            
                TeiidSqlContext ccontext = new TeiidSqlNodeContext(column);
                ccontext.add(SHORT_NAME_ONLY_KEY, true);
                visit(column, ccontext);
            }

            append(RPAREN);
            append(SPACE);
        }

        append(AS);
        append(SPACE);
        append(LPAREN);

        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        if (isTeiidVersionOrGreater(Version.TEIID_8_5) && command == null) {
            append("<dependent values>"); //$NON-NLS-1$
        } else {
            visit(command);
        }

        append(RPAREN);

        return null;
    }

    private void createAssignment(Node node) throws RepositoryException {
        Node variable = reference(node, AssignmentStatement.VARIABLE_REF_NAME);
        visit(variable);

        Node expression = reference(node, ExpressionStatement.EXPRESSION_REF_NAME);
        if (expression != null) {
            append(SPACE + EQUALS + SPACE);
            visit(expression);
        }

        append(SEMI_COLON);
    }

    @Override
    public Object assignmentStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        createAssignment(node);

        return null;
    }

    @Override
    public Object declareStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(DECLARE);
        append(SPACE);

        String varType = propertyString(node, DeclareStatement.VARIABLE_TYPE_PROP_NAME);
        append(varType);
        append(SPACE);
        createAssignment(node);

        return null;
    }

    @Override
    public Object returnStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(RETURN);

        Node expression = reference(node, ExpressionStatement.EXPRESSION_REF_NAME);
        if (expression != null) {
            append(SPACE);
            visit(expression);
        }

        append(SEMI_COLON);

        return null;
    }

    @Override
    public Object block(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        appendLabel(node);

        // Add first clause
        append(BEGIN);

        boolean atomic = propertyBoolean(node, Block.ATOMIC_PROP_NAME);
        if (atomic) {
            append(SPACE);
            append(ATOMIC);
        }

        append(NEW_LINE);

        appendStatements(node, Block.STATEMENTS_REF_NAME);

        String exceptionGroup = propertyString(node, Block.EXCEPTION_GROUP_PROP_NAME);
        if (exceptionGroup != null) {
            append(EXCEPTION);
            append(SPACE);
            appendDisplayName(exceptionGroup);
            append(NEW_LINE);

            appendStatements(node, Block.EXCEPTION_STATEMENTS_REF_NAME);
        }

        append(END);

        return null;
    }

    @Override
    public Object branchingStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String modeName = propertyString(node, BranchingStatement.MODE_PROP_NAME);
        BranchingMode mode = BranchingMode.findBranchingMode(modeName);

        append(mode.name());

        String label = propertyString(node, Labeled.LABEL_PROP_NAME);
        if (label != null) {
            append(SPACE);
            appendDisplayName(label);
        }

        append(SEMI_COLON);
        
        return null;
    }

    @Override
    public Object commandStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        visit(command);

        boolean returnable = propertyBoolean(node, CommandStatement.RETURNABLE_PROP_NAME);
        if (!returnable) {
            append(SPACE);
            append(WITHOUT);
            append(SPACE);
            append(RETURN);
        }

        append(SEMI_COLON);

        return null;
    }

    @Override
    public Object ifStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(IF);
        append(OPEN_BRACKET);

        Node condition = reference(node, IfStatement.CONDITION_REF_NAME);
        visit(condition);

        append(CLOSE_BRACKET);
        append(NEW_LINE);

        Node ifBlock = reference(node, IfStatement.IF_BLOCK_REF_NAME);
        visit(ifBlock);

        Node elseBlock = reference(node, IfStatement.ELSE_BLOCK_REF_NAME);
        if (elseBlock != null) {
            append(NEW_LINE);
            append(ELSE);
            append(NEW_LINE);
            visit(elseBlock);
        }

        return null;
    }

    @Override
    public Object loopStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        appendLabel(node);
        append(LOOP);
        append(SPACE);
        append(ON);
        append(SPACE + OPEN_BRACKET);

        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        visit(command);

        append(CLOSE_BRACKET + SPACE);
        append(AS);
        append(SPACE);

        String cursorName = propertyString(node, LoopStatement.CURSOR_NAME_PROP_NAME);
        appendDisplayName(cursorName);

        append(NEW_LINE);
 
        Node block = reference(node, LoopStatement.BLOCK_REF_NAME);
        visit(block);

        return null;
    }

    @Override
    public Object raiseStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(RAISE);
        append(SPACE);

        boolean warning = propertyBoolean(node, RaiseStatement.WARNING_PROP_NAME);
        if (warning) {
            append(SQLWARNING);
            append(SPACE);
        }

        Node expression = reference(node, ExpressionStatement.EXPRESSION_REF_NAME);
        visit(expression);
        append(SEMI_COLON);

        return null;
    }

    @Override
    public Object whileStatement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        appendLabel(node);
        append(WHILE);
        append(OPEN_BRACKET);

        Node condition = reference(node, WhileStatement.CONDITION_REF_NAME);
        visit(condition);
        append(SEMI_COLON + NEW_LINE);
 
        Node block = reference(node, WhileStatement.BLOCK_REF_NAME);
        visit(block);

        return null;
    }

    @Override
    public Object exceptionExpression(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(SQLEXCEPTION);
        append(SPACE);

        Node message = reference(node, ExceptionExpression.MESSAGE_REF_NAME);
        Node sqlState = reference(node, ExceptionExpression.SQL_STATE_REF_NAME);
        Node errorCode = reference(node, ExceptionExpression.ERROR_CODE_REF_NAME);
        Node parent = reference(node, ExceptionExpression.PARENT_EXPRESSION_REF_NAME);

        visit(message);

        if (sqlState != null) {
            append(SPACE);
            append(SQLSTATE);
            append(SPACE);
            visit(sqlState);

            if (errorCode != null) {
                append(COMMA);
                append(SPACE);
                visit(errorCode);
            }
        }

        if (parent != null) {
            append(SPACE);
            append(CHAIN);
            append(SPACE);
            visit(parent);
        }

        return null;
    }

    @Override
    public Object function(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String name = propertyString(node, Function.NAME_PROP_NAME);
        boolean implicit = propertyBoolean(node, Function.IMPLICIT_PROP_NAME);
        Iterator<Node> args = references(node, Function.ARGS_REF_NAME);

        if (implicit) {
            // Hide this function, which is implicit
            visit(args.next());

        } else if (name.equalsIgnoreCase(CONVERT) || name.equalsIgnoreCase(CAST)) {
            append(name);
            append(OPEN_BRACKET);

            if (args.hasNext()) {
                visit(args.next());

                if (name.equalsIgnoreCase(CONVERT)) {
                    append(COMMA + SPACE);
                } else {
                    append(SPACE);
                    append(AS);
                    append(SPACE);
                }

                Node args1 = null;
                if (args.hasNext())
                    args1 = args.next();

                if (args1 != null && instanceOf(args1, LexTokens.CONSTANT)) {
                    Property valueProp = args1.getProperty(Constant.VALUE_PROP_NAME);
                    append(toString(valueProp));
                } else {
                    append(undefined());
                }

            }

            append(CLOSE_BRACKET);

        } else if (name.equals(PLUS) || name.equals(MINUS) || name.equals(MULTIPLY) || name.equals(DIVIDE) || name.equals(LOGICAL_OR)) {
            append(OPEN_BRACKET);

            for(int i = 0; args.hasNext(); ++i) {
                if (i > 0) {
                    append(SPACE);
                    append(name);
                    append(SPACE);
                }

                visit(args.next());
            }

            append(CLOSE_BRACKET);

        } else if (name.equalsIgnoreCase(TIMESTAMPADD) || name.equalsIgnoreCase(TIMESTAMPDIFF)) {
            append(name);
            append(OPEN_BRACKET);

            if (args.hasNext()) {
                Property valueProp = args.next().getProperty(Constant.VALUE_PROP_NAME);
                append(toString(valueProp));

                iterate(args);
            }

            append(CLOSE_BRACKET);

        } else if (name.equalsIgnoreCase(XMLPI)) {
            append(name);
            append(OPEN_BRACKET + "NAME" + SPACE); //$NON-NLS-1$

            Property valueProp = args.next().getProperty(Constant.VALUE_PROP_NAME);
            appendDisplayName(toString(valueProp));
            iterate(args);

            append(CLOSE_BRACKET);

        } else if (name.equalsIgnoreCase(TRIM)) {
            append(name);
            append(LPAREN);

            Property valueProp = args.next().getProperty(Constant.VALUE_PROP_NAME);
            String value = toString(valueProp);
            if (!value.equalsIgnoreCase(BOTH)) {
                append(value);
                append(SPACE);
            }

            visit(args.next());
            append(SPACE);
            append(FROM);
            append(SPACE);
            visit(args.next());
            append(CLOSE_BRACKET);
        } else {
            append(name);
            append(OPEN_BRACKET);
            iterate(args);
            append(CLOSE_BRACKET);
        }

        return null;
    }

    @Override
    public Object aggregateSymbol(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String name = propertyString(node, AggregateSymbol.NAME_PROP_NAME);
        boolean distinct = propertyBoolean(node, AggregateSymbol.DISTINCT_PROP_NAME);

        String aggFunctionName = propertyString(node, AggregateSymbol.AGGREGATE_FUNCTION_PROP_NAME);
        Type aggregateFunction = Type.findAggregateFunction(aggFunctionName);

        append(name);
        append(OPEN_BRACKET);

        if (distinct) {
            append(DISTINCT);
            append(SPACE);
        } else if (aggregateFunction == Type.USER_DEFINED) {
            append(ALL);
            append(SPACE);
        }

        Iterator<Node> args = references(node, AggregateSymbol.ARGS_REF_NAME);
        if (args.hasNext()) {
            iterate(args);
        } else {
            if (aggregateFunction == Type.COUNT) {
                append(ALL_COLS);
            }
        }

        Node orderBy = reference(node, AggregateSymbol.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            append(SPACE);
            visit(orderBy);
        }

        append(CLOSE_BRACKET);

        Node condition = reference(node, AggregateSymbol.CONDITION_REF_NAME);
        if (condition != null) {
            append(SPACE);
            append(FILTER);
            append(LPAREN);
            append(WHERE);
            append(SPACE);
            visit(condition);
            append(RPAREN);
        }

        return null;
    }

    @Override
    public Object aliasSymbol(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node symbol = reference(node, AliasSymbol.SYMBOL_REF_NAME);
        visit(symbol);
        append(SPACE);
        append(AS);
        append(SPACE);

        append(escapeSinglePart(outputName(node)));

        return null;
    }

    @Override
    public Object elementSymbol(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean shortNameOnly = context.get(SHORT_NAME_ONLY_KEY) != null ? (Boolean) context.get(SHORT_NAME_ONLY_KEY) : false;

        String displayModeName = propertyString(node, ElementSymbol.DISPLAY_MODE_PROP_NAME);
        DisplayMode displayMode = DisplayMode.findDisplayMode(displayModeName);

        String outputName = outputName(node);
        
        if (DisplayMode.SHORT_OUTPUT_NAME.equals(displayMode) || shortNameOnly) {
            appendDisplayName(shortName(outputName));
            return null;
        }

        if (DisplayMode.FULLY_QUALIFIED.equals(displayMode)) {
            outputName = propertyString(node,  Symbol.NAME_PROP_NAME);
        }

        appendDisplayName(outputName);

        return null;
    }

    @Override
    public Object expressionSymbol(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        Node expression = reference(node, ExpressionSymbol.EXPRESSION_REF_NAME);
        visit(expression);
        return null;
    }

    @Override
    public Object groupSymbol(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);
        String alias = null;
        String name = propertyString(node, org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Symbol.NAME_PROP_NAME);
        String defn = propertyString(node, GroupSymbol.DEFINITION_PROP_NAME);
        
        if (defn != null) {
            alias = name;
            name = defn;
        }

        appendDisplayName(name);

        if (alias != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            append(escapeSinglePart(alias));
        }

        return null;
    }

    @Override
    public Object arraySymbol(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        boolean implicit = propertyBoolean(node, ArraySymbol.IMPLICIT_PROP_NAME);
        if (!implicit) {
            append(LPAREN);
        }

        iterate(node, ArraySymbol.EXPRESSIONS_REF_NAME);
        if (!implicit) {

            if (size(node, ArraySymbol.EXPRESSIONS_REF_NAME) == 1) {
                append(COMMA);
            }

            append(RPAREN);
        }

        return null;
    }

    @Override
    public Object caseExpression(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(CASE);
        append(SPACE);
        Node expression = reference(node, CaseExpression.EXPRESSION_REF_NAME);
        visit(expression);
        append(SPACE);

        Iterator<Node> whens = references(node, CaseExpression.WHEN_REF_NAME);
        Iterator<Node> thens = references(node, CaseExpression.THEN_REF_NAME);
        while(whens.hasNext() && thens.hasNext()) {
            append(WHEN);
            append(SPACE);
            visit(whens.next());
            append(SPACE);
            append(THEN);
            append(SPACE);
            visit(thens.next());
            append(SPACE);
        }

        Node elseExpression = reference(node, CaseExpression.ELSE_EXPRESSION_REF_NAME);
        if (elseExpression != null) {
            append(ELSE);
            append(SPACE);
            visit(elseExpression);
            append(SPACE);
        }

        append(END);

        return null;
    }

    @Override
    public Object constant(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        String typeName = propertyString(node, Expression.TYPE_CLASS_PROP_NAME);
        DataTypeName dataTypeName = DataTypeName.findDataTypeName(typeName);

        Class<?> type = getDataTypeManager().getDefaultDataClass(dataTypeName);
        boolean multiValued = propertyBoolean(node, Constant.MULTI_VALUED_PROP_NAME);
        Object value = propertyValue(node, Constant.VALUE_PROP_NAME, dataTypeName);
        appendLiteral(type, multiValued, value);

        return null;
    }

    @Override
    public Object derivedColumn(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node expression = reference(node, DerivedColumn.EXPRESSION_REF_NAME);
        visit(expression);

        String alias = propertyString(node, DerivedColumn.ALIAS_PROP_NAME);
        if (alias != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            appendDisplayName(alias);
        }

        return null;
    }

    @Override
    public Object jsonObject(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(JSONOBJECT);
        append(OPEN_BRACKET);
        iterate(node, JSONObject.ARGS_REF_NAME);
        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object multipleElementSymbol(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node groupNode = reference(node, MultipleElementSymbol.GROUP_REF_NAME);
        if (groupNode == null) {
            append(STAR);
        } else {
            visit(groupNode);
            append(DOT);
            append(STAR);
        }

        return null;
    }

    @Override
    public Object queryString(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(QUERYSTRING);
        append(OPEN_BRACKET);

        Node path = reference(node, QueryString.PATH_REF_NAME);
        visit(path);

        Iterator<Node> args = references(node, QueryString.ARGS_REF_NAME);
        if (args.hasNext()) {
            append(COMMA);
            append(SPACE);
            iterate(node, QueryString.ARGS_REF_NAME);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object reference(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node expression = reference(node, Reference.EXPRESSION_REF_NAME);
        boolean positional = propertyBoolean(node, Reference.POSITIONAL_PROP_NAME);
        if (!positional && expression != null) {    
            visit(expression);
        } else {
            append(QUESTION_MARK);
        }

        return null;
    }

    @Override
    public Object scalarSubquery(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        // operator and beginning of list
        append(OPEN_BRACKET);
        Node command = reference(node, SubqueryContainer.COMMAND_REF_NAME);
        visit(command);
        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object searchedCaseExpression(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(CASE);

        Iterator<Node> whens = references(node, CaseExpression.WHEN_REF_NAME);
        Iterator<Node> thens = references(node, CaseExpression.THEN_REF_NAME);
        while(whens.hasNext() && thens.hasNext()) {
            append(SPACE);
            append(WHEN);
            append(SPACE);
            visit(whens.next());
            append(SPACE);
            append(THEN);
            append(SPACE);
            visit(thens.next());
        }

        append(SPACE);

        Node elseExpression = reference(node, CaseExpression.ELSE_EXPRESSION_REF_NAME);
        if (elseExpression != null) {
            append(ELSE);
            append(SPACE);
            visit(elseExpression);
            append(SPACE);
        }

        append(END);

        return null;
    }

    @Override
    public Object textLine(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(FOR);
        append(SPACE);
        iterate(node, TextLine.EXPRESSIONS_REF_NAME);

        String delimiter = propertyString(node, TextLine.DELIMITER_PROP_NAME);
        if (delimiter != null) {
            append(SPACE);
            append(DELIMITER);
            append(SPACE);
            appendLiteral(String.class, false, delimiter);
        }

        String quote = propertyString(node, TextLine.QUOTE_PROP_NAME);
        if (quote != null) {
            append(SPACE);
            append(QUOTE);
            append(SPACE);
            appendLiteral(String.class, false, quote);
        }

        boolean includeHeader = propertyBoolean(node, TextLine.INCLUDE_HEADER_PROP_NAME);
        if (!includeHeader) {
            append(SPACE);
            append(HEADER);
        }

        String encoding = propertyString(node, TextLine.ENCODING_PROP_NAME);
        if (encoding != null) {
            append(SPACE);
            append(ENCODING);
            append(SPACE);
            appendDisplayName(encoding);
        }

        return null;
    }

    @Override
    public Object windowFunction(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        Node windowFunction = reference(node, WindowFunction.FUNCTION_REF_NAME);
        visit(windowFunction);

        append(SPACE);
        append(OVER);
        append(SPACE);

        Node windowSpec = reference(node, WindowFunction.WINDOW_SPECIFICATION_REF_NAME);
        visit(windowSpec);

        return null;
    }

    @Override
    public Object windowSpecification(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(LPAREN);
        boolean needsSpace = false;
        
        Iterator<Node> partitions = references(node, WindowSpecification.PARTITION_REF_NAME);
        if (partitions.hasNext()) {
            append(PARTITION);
            append(SPACE);
            append(BY);
            append(SPACE);
            iterate(node, WindowSpecification.PARTITION_REF_NAME);
            needsSpace = true;
        }

        Node orderBy = reference(node, WindowSpecification.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            if (needsSpace) {
                append(SPACE);
            }

            visit(orderBy);
        }

        append(RPAREN);

        return null;
    }

    @Override
    public Object xmlAttributes(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(XMLATTRIBUTES);
        append(OPEN_BRACKET);
        iterate(node, XMLAttributes.ARGS_REF_NAME);
        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object xmlElement(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(XMLELEMENT);
        append(OPEN_BRACKET + "NAME" + SPACE); //$NON-NLS-1$

        String name = propertyString(node, XMLElement.NAME_PROP_NAME);
        appendDisplayName(name);

        Node namespaces = reference(node, XMLElement.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            append(COMMA + SPACE);
            visit(namespaces);
        }

        Node attributes = reference(node, XMLElement.ATTRIBUTES_REF_NAME);
        if (attributes != null) {
            append(COMMA + SPACE);
            visit(attributes);
        }

        Iterator<Node> contents = references(node, XMLElement.CONTENT_REF_NAME);
        if (contents.hasNext()) {
            append(COMMA + SPACE);
        }

        iterate(node, XMLElement.CONTENT_REF_NAME);

        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object xmlForest(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(XMLFOREST);
        append(OPEN_BRACKET);

        Node namespaces = reference(node, XMLForest.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            visit(namespaces);
            append(COMMA + SPACE);
        }

        iterate(node, XMLForest.ARGUMENTS_REF_NAME);

        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object xmlNamespaces(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(XMLNAMESPACES);
        append(OPEN_BRACKET);

        iterate(node, XMLNamespaces.NAMESPACE_ITEMS_REF_NAME);

        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object xmlParse(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(XMLPARSE);
        append(LPAREN);

        boolean document = propertyBoolean(node, XMLParse.DOCUMENT_PROP_NAME);
        if (document) {
            append(DOCUMENT);
        } else {
            append(CONTENT);
        }

        append(SPACE);
        
        Node expression = reference(node, XMLParse.EXPRESSION_REF_NAME);
        visit(expression);

        boolean wellFormed = propertyBoolean(node, XMLParse.WELL_FORMED_PROP_NAME);
        if (wellFormed) {
            append(SPACE);
            append(WELLFORMED);
        }

        append(RPAREN);

        return null;
    }

    @Override
    public Object xmlQuery(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(XMLQUERY);
        append(OPEN_BRACKET);

        Node namespaces = reference(node, XMLQuery.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            visit(namespaces);
            append(COMMA);
            append(SPACE);
        }

        String xquery = propertyString(node, XMLQuery.XQUERY_PROP_NAME);
        appendLiteral(String.class, false, xquery);

        Iterator<Node> passings = references(node, XMLQuery.PASSING_REF_NAME);
        if (passings.hasNext()) {
            append(SPACE);
            append(PASSING);
            append(SPACE);
            iterate(node, XMLQuery.PASSING_REF_NAME);
        }

        
        if (node.hasProperty(XMLQuery.EMPTY_ON_EMPTY_PROP_NAME)) {
            append(SPACE);
            boolean emptyOnEmpty = propertyBoolean(node, XMLQuery.EMPTY_ON_EMPTY_PROP_NAME);
            if (emptyOnEmpty) {
                append(EMPTY);
            } else {
                append(NULL);
            }

            append(SPACE);
            append(ON);
            append(SPACE);
            append(EMPTY);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    @Override
    public Object xmlSerialize(TeiidSqlContext context) throws RepositoryException {
        Node node = (Node) context.get(NODE_KEY);

        append(XMLSERIALIZE);
        append(LPAREN);
        
        if (node.hasProperty(XMLSerialize.DOCUMENT_PROP_NAME)) {
            boolean document = propertyBoolean(node,  XMLSerialize.DOCUMENT_PROP_NAME);
            if (document) {
                append(DOCUMENT);
            } else {
                append(CONTENT);
            }

            append(SPACE);
        }

        Node expression = reference(node, XMLSerialize.EXPRESSION_REF_NAME);
        visit(expression);

        String typeString = propertyString(node,  XMLSerialize.TYPE_STRING_PROP_NAME);
        if (typeString != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            append(typeString);
        }

        String encoding = propertyString(node,  XMLSerialize.ENCODING_PROP_NAME);
        if (encoding != null) {
            append(SPACE);
            append(ENCODING);
            append(SPACE);
            append(escapeSinglePart(encoding));
        }

        String version = propertyString(node,  XMLSerialize.VERSION_PROP_NAME);
        if (version != null) {
            append(SPACE);
            append(VERSION);
            append(SPACE);
            appendLiteral(String.class, false, version);
        }

        if (node.hasProperty(XMLSerialize.DECLARATION_PROP_NAME)) {
            boolean declaration = propertyBoolean(node,  XMLSerialize.DECLARATION_PROP_NAME);
            append(SPACE);
            if (declaration) {
                append(INCLUDING);
            } else {
                append(EXCLUDING);
            }

            append(SPACE);
            append(XMLDECLARATION);
        }

        append(RPAREN);

        return null;
    }
}
