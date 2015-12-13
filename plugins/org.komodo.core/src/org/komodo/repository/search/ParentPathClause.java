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


import org.komodo.core.KomodoLexicon.Search;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.api.JcrConstants;

/**
 * A Parent Path Clause
 */
class ParentPathClause extends PathClause {

    private boolean childrenOnly = false;

    /**
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias of the selector
     * @param path path used in the clause
     * @param childrenOnly true to return only the direct children
     */
    public ParentPathClause(LogicalOperator operator, String alias, String path, boolean childrenOnly) {
        super(operator, alias, path);
        setChildrenOnly(childrenOnly);
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected ParentPathClause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        super(uow, whereClause);

        if (whereClause.hasProperty(uow, Search.WhereParentPathClause.CHILDREN_ONLY)) {
            setChildrenOnly(whereClause.getProperty(uow, Search.WhereParentPathClause.CHILDREN_ONLY).getBooleanValue(uow));
        }
    }

    /**
     * @return the childrenOnly
     */
    public boolean isChildrenOnly() {
        return this.childrenOnly;
    }

    /**
     * @param childrenOnly the childrenOnly to set
     */
    public void setChildrenOnly(boolean childrenOnly) {
        this.childrenOnly = childrenOnly;
    }

    @Override
    public String clauseString(int position) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(position, buffer);

        setAlias(checkWhereAlias(getAlias()));

//        where e.[jcr:path] LIKE '/inf:patient[265]%'

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias())
                     .append(DOT);
        }

        buffer.append(OPEN_SQUARE_BRACKET)
                 .append(JcrConstants.JCR_PATH)
                 .append(CLOSE_SQUARE_BRACKET)

                 .append(SPACE)
                 .append(LIKE)
                 .append(SPACE)

                 .append(QUOTE_MARK)
                 .append(getPath());

        if (! getPath().endsWith(CLOSE_SQUARE_BRACKET) &&
             ! getPath().endsWith(FORWARD_SLASH))
            buffer.append(FORWARD_SLASH);

        buffer.append(PERCENT)
                 .append(QUOTE_MARK);

        if (isChildrenOnly()) {

            String path = getPath();
            if (path.endsWith(PERCENT))
                path = path.substring(0, path.length() - 2);

            if (path.endsWith(FORWARD_SLASH))
                path = path.substring(0, path.length() - 2);

            buffer.append(SPACE)
                     .append(LogicalOperator.AND)
                     .append(SPACE)
                     .append("ISCHILDNODE") //$NON-NLS-1$
                     .append(OPEN_BRACKET);

            if (! StringUtils.isEmpty(getAlias())) {
                buffer.append(getAlias())
                         .append(COMMA)
                         .append(SPACE);
            }

            buffer.append(QUOTE_MARK)
                     .append(path)
                     .append(QUOTE_MARK)
                     .append(CLOSE_BRACKET);
        }

        return buffer.toString();
    }

    @Override
    void write(UnitOfWork uow, KomodoObject searchObject) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(searchObject, "searchObject"); //$NON-NLS-1$

        Repository repository = searchObject.getRepository();
        KomodoObject whereObject = repository.add(uow, searchObject.getAbsolutePath(),
                                                  Search.WHERE_CLAUSE,
                                                  Search.WhereParentPathClause.NODE_TYPE);

        writeProperties(uow, whereObject);

        whereObject.setProperty(uow, Search.WherePathClause.PATH, getPath());
        whereObject.setProperty(uow, Search.WhereParentPathClause.CHILDREN_ONLY, isChildrenOnly());
    }
}
