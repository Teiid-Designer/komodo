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
package org.komodo.relational.model.internal;

import javax.jcr.Node;
import org.komodo.core.KomodoLexicon;
import org.komodo.modeshape.visitor.DdlNodeVisitor;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Schema;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.TeiidVersionProvider;

/**
 * A named schema fragment
 */
public class SchemaImpl extends RelationalObjectImpl implements Schema {

    /**
     * The resolver of a {@link Schema}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType(transaction, repository, kobject, KomodoLexicon.Schema.NODE_TYPE);
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Schema resolve( final UnitOfWork transaction,
                               final Repository repository,
                               final KomodoObject kobject ) throws KException {
            return new SchemaImpl(transaction, repository, kobject.getAbsolutePath());
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public SchemaImpl( final UnitOfWork uow,
                       final Repository repository,
                       final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public String getRendition( UnitOfWork uow ) throws KException {
        String rendition = getObjectProperty(uow, Property.ValueType.STRING, "getRendition", //$NON-NLS-1$
                                             KomodoLexicon.Schema.RENDITION);

        return rendition == null ? EMPTY_STRING : rendition;
    }

    @Override
    public void setRendition( UnitOfWork uow,
                              String rendition ) throws KException {
        setObjectProperty(uow, "setRendition", KomodoLexicon.Schema.RENDITION, rendition); //$NON-NLS-1$
    }

    @Override
    public String export(UnitOfWork uow) throws KException {
        // Is there a situation where this schema fragment is just Teiid SQL?
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("schemaimpl-export", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("schemaimpl-export: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            StringBuffer result = new StringBuffer();
            Node schemaNode = node(transaction);

            DdlNodeVisitor visitor = new DdlNodeVisitor(TeiidVersionProvider.getInstance().getTeiidVersion());
            visitor.visit(schemaNode);
            result.append(visitor.getDdl());

            if (uow == null) {
                transaction.commit();
            }

            return result.toString();
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
