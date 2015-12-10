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
package org.komodo.repository.search;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.komodo.core.KomodoLexicon.Search;
import org.komodo.core.Messages;
import org.komodo.spi.KException;
import org.komodo.spi.query.sql.SQLConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.KeywordCriteria;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.ModeShapeLexicon;
import org.modeshape.jcr.api.JcrConstants;

/**
 * Finder class for searching a repository
 */
public class ObjectSearcher implements SQLConstants {

    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss"); //$NON-NLS-1$

    private final Repository repository;

    private Set<FromType> fromTypes;

    private List<Clause> whereClauses;

    private String customWhereClause;

    /**
     * @param repository the repository to search
     */
    public ObjectSearcher(Repository repository) {
        ArgCheck.isNotNull(repository);
        this.repository = repository;
    }

    private boolean isEmpty(Collection<?> list) {
        return list == null || list.isEmpty();
    }

    /**
     * @return the repository
     */
    public Repository getRepository() {
        return this.repository;
    }

    /**
     * Add a node type to the From clause
     *
     * @param type the type to be added
     * @param alias the alias of the type
     * @return this search object
     */
    public ObjectSearcher addFromType(String type, String alias) {
        ArgCheck.isNotEmpty(type);

        if (fromTypes == null)
            fromTypes = new LinkedHashSet<FromType>();

        fromTypes.add(new FromType(type, alias));
        return this;
    }

    /**
     * Add a node type to the From clause
     *
     * @param type the type to be added
     * @return this search object
     */
    public ObjectSearcher addFromType(String type) {
        addFromType(type, null);
        return this;
    }

    /**
     * @return set of node types for the From clause
     */
    Set<FromType> getFromTypes() {
        if (fromTypes == null) {
            return Collections.emptySet();
        }

        return fromTypes;
    }

    @SuppressWarnings( "unchecked" )
    private <T extends PropertyClause> T findWhereAliasClause(Class<T> clauseType, String alias, String property) {
        for (Clause clause : getWhereClauses()) {
            if (! (clauseType.isInstance(clause)))
                continue;

            PropertyClause aliasClause = (PropertyClause) clause;
            String clauseAlias = aliasClause.getAlias();
            if (clauseAlias == null && alias != null)
                continue;

            if (clauseAlias != null && alias == null)
                continue;

            if (StringUtils.equalsIgnoreCase(clauseAlias, alias) &&
                StringUtils.equalsIgnoreCase(aliasClause.getProperty(), property)) {
                return (T) aliasClause;
            }
        }

        return null;
    }

    /**
     * @return the whereClauses
     */
    public List<Clause> getWhereClauses() {
        if (whereClauses == null) {
            return Collections.emptyList();
        }

        return this.whereClauses;
    }

    /**
     * Adds the given where clause
     *
     * @param whereClause the where clause
     */
    public void addWhereClause(Clause whereClause) {
        ArgCheck.isTrue(customWhereClause == null,
                                    "searchObject cannot contain both whereClauses and customWhereClause"); //$NON-NLS-1$

        if (whereClauses == null)
            whereClauses = new ArrayList<Clause>();

        whereClause.setParent(this);
        whereClauses.add(whereClause);
    }

    /**
     * Add an IN sub-clause to the WHERE clause, eg. WHERE alias.property IN (value1, value2, value3)
     *
     * @param operator the AND/OR operator preceding the clause. Can be <null> if the first clause
     * @param alias the alias of the type
     * @param property the name of the property belonging to the type
     * @param values the value(s) that the property can be
     * @return this search object
     */
    public ObjectSearcher addWhereSetClause(LogicalOperator operator, String alias, String property, String... values) {
        SetClause whereClause = findWhereAliasClause(SetClause.class, alias, property);
        if (whereClause == null) {
            whereClause = new SetClause(operator, alias, property, values);
            addWhereClause(whereClause);
        }
        else {
            for (String value : values)
                whereClause.addValue(value);
        }

        return this;
    }

    /**
     * Add a comparison sub-clause to the WHERE clause, eg.
     *   WHERE alias.[property] = 'blah'
     *   WHERE alias.[property] LIKE 'bl%h'
     *
     * @param operator the AND/OR operator preceding the clause. Can be <null> if the first clause
     * @param alias the alias of the type
     * @param property the name of the property belonging to the type
     * @param compareOperator the comparison operator
     * @param value the value that the property can be
     * @return this search object
     */
    public ObjectSearcher addWhereCompareClause(LogicalOperator operator, String alias, String property,
                                                                ComparisonOperator compareOperator, String value) {
        CompareClause whereClause = new CompareClause(operator, alias, property, compareOperator, value);
        addWhereClause(whereClause);

        return this;
    }

    /**
     * Add a CONTAINS sub-clause to the WHERE clause, eg. WHERE CONTAINS (alias.property, 'value')
     *
     * @param operator the AND/OR operator preceding the clause. Can be <null> if the first clause
     * @param alias the alias of the type
     * @param property the name of the property belonging to the type
     * @param keywordCriteria the criteria settings for the given values
     * @param keywords the keywords to search for in the property
     * @return this search object
     */
    public ObjectSearcher addWhereContainsClause(LogicalOperator operator, String alias, String property,
                                                                  KeywordCriteria keywordCriteria,
                                                                  String... keywords) {

        ContainsClause whereClause = findWhereAliasClause(ContainsClause.class, alias, property);
        if (whereClause == null) {
            whereClause = new ContainsClause(operator, alias, property, keywordCriteria, keywords);
            addWhereClause(whereClause);
        }
        else {
            for (String keyword : keywords)
                whereClause.addKeyword(keyword);
        }

        return this;
    }

    /**
     * Add a CONTAINS sub-clause to the WHERE clause, eg. WHERE CONTAINS (alias.property, 'value')
     *
     * @param operator the AND/OR operator preceding the clause. Can be <null> if the first clause
     * @param alias the alias of the type
     * @param property the name of the property belonging to the type
     * @param keyword the keyword to search for in the property
     * @return this search object
     */
    public ObjectSearcher addWhereContainsClause(LogicalOperator operator, String alias, String property, String keyword) {
        return addWhereContainsClause(operator, alias, property, KeywordCriteria.ANY, keyword);
    }

    /**
     * Add a PATH clause to the Where clause, eg. WHERE PATH(alias) = 'path1'
     *
     * Note. this would be the path of the node trying to be found and NOT the path of its parent
     *
     * @param operator the AND/OR operator preceding the clause. Can be <null> if the first clause
     * @param alias the alias of the selector
     * @param path the path to be added
     * @return this search object
     */
    public ObjectSearcher addWherePathClause(LogicalOperator operator, String alias, String path) {
        PathClause pathClause = new PathClause(operator, alias, path);
        addWhereClause(pathClause);

        return this;
    }

    /**
     * Add a parent clause to the Where clause, eg. WHERE alias.[jcr:path] LIKE 'path1/%'
     *
     * @param operator the AND/OR operator preceding the clause. Can be <null> if the first clause
     * @param alias the alias of the selector
     * @param parentPath the path to be added
     * @param childrenOnly set as true if to return only the direct children, false to return all descendants
     * @return this search object
     */
    public ObjectSearcher addWhereParentClause(LogicalOperator operator, String alias, String parentPath, boolean childrenOnly) {
        ParentPathClause pathClause = new ParentPathClause(operator, alias, parentPath, childrenOnly);
        addWhereClause(pathClause);

        return this;
    }

    /**
     * @return the customWhereClause
     */
    public String getCustomWhereClause() {
        return this.customWhereClause;
    }

    /**
     * Set a custom where clause for this searcher. Such a clause may be created
     * using {@link #toString()} or be completely custom.
     *
     * Note:
     * * no parsing of this clause will be conducted prior to appending this clause.
     * * do not prefix this clause with the WHERE keyword
     *
     * @param whereClause custom where clause string
     * @return this search object
     */
    public ObjectSearcher setCustomWhereClause(String whereClause) {
        ArgCheck.isNotEmpty(whereClause);
        ArgCheck.isTrue(whereClauses == null, "searchObject cannot contain both whereClauses and customWhereClause"); //$NON-NLS-1$

        this.customWhereClause = whereClause;
        return this;
    }

    private boolean hasWhere() {
        return (whereClauses != null && ! whereClauses.isEmpty()) || customWhereClause != null;
    }

    private void createSelect(StringBuffer buffer) {
        buffer.append(SELECT);
        buffer.append(SPACE);
        buffer.append(OPEN_SQUARE_BRACKET);
        buffer.append(JcrConstants.JCR_PATH);
        buffer.append(CLOSE_SQUARE_BRACKET);
        buffer.append(COMMA);
        buffer.append(SPACE);
        buffer.append(OPEN_SQUARE_BRACKET);
        buffer.append(ModeShapeLexicon.LOCALNAME.getString());
        buffer.append(CLOSE_SQUARE_BRACKET);
    }

    private void createFrom(StringBuffer buffer) {
        ArgCheck.isTrue(getFromTypes().size() > 0, "At least 1 from clause is required"); //$NON-NLS-1$

        //
        // If there are where clauses and there are multiple from types
        // then each from type should have an alias
        //
        if (hasWhere() && getFromTypes().size() > 1) {
            for (FromType fromType : getFromTypes()) {
                ArgCheck.isNotEmpty(fromType.getAlias());
            }
        }

        buffer.append(SPACE);
        buffer.append(FROM);
        buffer.append(SPACE);

        Iterator<FromType> iterator = getFromTypes().iterator();
        while (iterator.hasNext()) {
            FromType fromType = iterator.next();
            buffer.append(OPEN_SQUARE_BRACKET);
            buffer.append(fromType.getType());
            buffer.append(CLOSE_SQUARE_BRACKET);

            if (! fromType.getAlias().isEmpty()) {
                buffer.append(SPACE);
                buffer.append(AS);
                buffer.append(SPACE);
                buffer.append(fromType.getAlias());
            }

            if (iterator.hasNext()) {
                buffer.append(COMMA);
                buffer.append(SPACE);
            }
        }
    }

    /**
     * Create the Where clause
     *
     * @param buffer
     */
    private void createWhere(StringBuffer buffer) {
        ArgCheck.isTrue(getFromTypes().size() > 0, "At least 1 from clause is required"); //$NON-NLS-1$

        if (isEmpty(whereClauses) && customWhereClause == null)
            return;

        buffer.append(SPACE);
        buffer.append(WHERE);
        buffer.append(SPACE);

        if (! whereClauses.isEmpty()) {
            for (int i = 0; i < whereClauses.size(); i++) {
                Clause clause = whereClauses.get(i);
                buffer.append(clause.clauseString(i));

                if (i < whereClauses.size() - 1)
                    buffer.append(SPACE);
            }
        } else
            buffer.append(customWhereClause);
    }

    private String createStatement() {
        StringBuffer buffer = new StringBuffer();

        createSelect(buffer);
        createFrom(buffer);
        createWhere(buffer);

        return buffer.toString();
    }

    @Override
    public String toString() {
        return createStatement();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.customWhereClause == null) ? 0 : this.customWhereClause.hashCode());
        result = prime * result + ((this.fromTypes == null) ? 0 : this.fromTypes.hashCode());
        result = prime * result + ((this.repository == null) ? 0 : this.repository.hashCode());
        result = prime * result + ((this.whereClauses == null) ? 0 : this.whereClauses.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ObjectSearcher other = (ObjectSearcher)obj;
        if (this.customWhereClause == null) {
            if (other.customWhereClause != null)
                return false;
        } else
            if (!this.customWhereClause.equals(other.customWhereClause))
                return false;
        if (this.fromTypes == null) {
            if (other.fromTypes != null)
                return false;
        } else
            if (!this.fromTypes.equals(other.fromTypes))
                return false;
        if (this.repository == null) {
            if (other.repository != null)
                return false;
        } else
            if (!this.repository.equals(other.repository))
                return false;
        if (this.whereClauses == null) {
            if (other.whereClauses != null)
                return false;
        } else
            if (!this.whereClauses.equals(other.whereClauses))
                return false;
        return true;
    }

    /**
     * Performs the search using the parameters of this object seacher
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param statement an sql2-like query statement
     * @return a list of {@link KomodoObject}s resulting from this search. Empty list if no results are found
     * @throws KException if error occurs
     */
    public List<KomodoObject> searchObjects(final UnitOfWork transaction, String statement) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        List<KomodoObject> results = Collections.emptyList();

        // execute query
        results = getRepository().query(transaction, statement);

        return results;
    }

    /**
     * Performs the search using the parameters of this object seacher
     *
     * @param uow a transaction. Can be null
     * @return a list of {@link KomodoObject}s resulting from this search
     * @throws KException if error occurs
     */
    public List<KomodoObject> searchObjects(final UnitOfWork uow) throws KException {
        String statement = createStatement();
        List<KomodoObject> objects = searchObjects(uow, statement);
        return objects;
    }

    /**
     * Write the search object to the repository
     *
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param name the name given to the search object
     * @return the persisted searchObject
     * @throws KException
     *         if an error occurs
     */
    public KomodoObject write(final UnitOfWork uow, String name) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(getRepository(), "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(name, "name"); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = getRepository().komodoSearches(uow).getAbsolutePath();
        KomodoObject searchObject = getRepository().getFromWorkspace(uow, parentPath + FORWARD_SLASH + name);

        if (searchObject != null) {
            // Overwrite the existing version
            searchObject.remove(uow);
        }

        searchObject = getRepository().add(uow, parentPath, name, Search.NODE_TYPE);

        // The date/time this search was created
        String date = DATE_FORMAT.format(new Date());
        searchObject.setProperty(uow, Search.SEARCH_DATE, date);
        for (FromType fromType : getFromTypes()) {
            fromType.write(uow, searchObject);
        }

        if (getCustomWhereClause() != null) {
            searchObject.setProperty(uow, Search.CUSTOM_WHERE, getCustomWhereClause());
        } else {
            for (Clause clause : getWhereClauses()) {
                clause.write(uow, searchObject);
            }
        }

        return searchObject;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param name the name of the search to read
     *
     * @throws KException name is not a search in the repository or if another error occurs
     */
    public void read(UnitOfWork uow, String name) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(name, "name"); //$NON-NLS-1$

        String parentPath = repository.komodoSearches(uow).getAbsolutePath();
        KomodoObject searchObject = repository.getFromWorkspace(uow, parentPath + FORWARD_SLASH + name);
        if (searchObject == null)
            throw new KException(Messages.getString(Messages.Search.No_Saved_Search, name));

        // Clear any existing data from this object searcher
        customWhereClause = null;

        if (fromTypes != null)
            fromTypes.clear();

        if (whereClauses != null)
            whereClauses.clear();

        if (searchObject.hasProperty(uow, Search.CUSTOM_WHERE)) {
            Property customWhere = searchObject.getProperty(uow, Search.CUSTOM_WHERE);
            setCustomWhereClause(customWhere.getStringValue(uow));
        }

        KomodoObject[] fromTypes = searchObject.getChildrenOfType(uow, Search.FromType.NODE_TYPE);
        if (fromTypes != null) {
            for (KomodoObject fromType : fromTypes) {
                String alias = null;
                String type = null;

                if (fromType.hasProperty(uow, Search.FromType.ALIAS))
                    alias = fromType.getProperty(uow, Search.FromType.ALIAS).getStringValue(uow);

                if (fromType.hasProperty(uow, Search.FromType.TYPE))
                    type = fromType.getProperty(uow, Search.FromType.TYPE).getStringValue(uow);

                addFromType(type, alias);
            }
        }

        KomodoObject[] whereClauses = searchObject.getChildren(uow, Search.WHERE_CLAUSE);
        if (whereClauses != null) {
            for (KomodoObject whereClauseObject : whereClauses) {
                String primaryType = whereClauseObject.getPrimaryType(uow).getName();

                Clause clause = null;
                if (Search.WhereCompareClause.NODE_TYPE.equals(primaryType)) {
                    clause = new CompareClause(uow, whereClauseObject);
                } else if (Search.WhereContainsClause.NODE_TYPE.equals(primaryType)) {
                    clause = new ContainsClause(uow, whereClauseObject);
                } else if (Search.WhereSetClause.NODE_TYPE.equals(primaryType)) {
                    clause = new SetClause(uow, whereClauseObject);
                } else if (Search.WherePathClause.NODE_TYPE.equals(primaryType)) {
                    clause = new PathClause(uow, whereClauseObject);
                } else if (Search.WhereParentPathClause.NODE_TYPE.equals(primaryType)) {
                    clause = new ParentPathClause(uow, whereClauseObject);
                }

                if (clause != null)
                    addWhereClause(clause);
            }
        }
    }
}
