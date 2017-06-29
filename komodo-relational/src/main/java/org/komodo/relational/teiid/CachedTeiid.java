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
package org.komodo.relational.teiid;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.teiid.internal.CachedTeiidImpl;
import org.komodo.relational.template.Template;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * A model of a cached live teiid server instance
 */
public interface CachedTeiid extends RelationalObject, TeiidArchetype {

    /**
     * The default expiration time of all cached teiid objects in the teiid cache
     */
    int DEFAULT_TEIID_CACHE_THRESHOLD = 10 * 60 * 1000;
    
    /**
     * The folder under which all cached vdbs will be placed
     */
    String VDBS_FOLDER = "vdbs";  //$NON-NLS-1$
    /**
     * The folder under which all cached connections will be placed
     */
    String CONNECTIONS_FOLDER = "connections";  //$NON-NLS-1$
    /**
     * The folder under which all cached translators will be placed
     */
    String TRANSLATORS_FOLDER = "translators";  //$NON-NLS-1$
    /**
     * The folder under which all cached drivers will be placed
     */
    String DRIVERS_FOLDER = "drivers";  //$NON-NLS-1$
    /**
     * The folder under which all cached templates will be placed
     */
    String TEMPLATES_FOLDER = "templates";  //$NON-NLS-1$

    /**
     * The type identifier.
     */
    int TYPE_ID = CachedTeiid.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.CACHED_TEIID;

    /**
     * The resolver of a {@link CachedTeiid}.
     */
    TypeResolver< CachedTeiid > RESOLVER = new TypeResolver< CachedTeiid >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< CachedTeiidImpl > owningClass() {
            return CachedTeiidImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, KomodoLexicon.CachedTeiid.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public CachedTeiid resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == CachedTeiid.TYPE_ID ) {
                return ( CachedTeiid )kobject;
            }

            return new CachedTeiidImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the vdbs found on this teiid server (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Vdb[] getVdbs( final UnitOfWork transaction, final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name the vdb name
     * @return the named vdb found on this teiid server or null
     * @throws KException
     *         if an error occurs
     */
    Vdb getVdb(final UnitOfWork transaction, String name) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the translators found on this teiid server (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Translator[] getTranslators(UnitOfWork transaction, final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the data sources found on this teiid server (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Connection[] getConnections(UnitOfWork transaction, final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name the connection name
     * @return the named connection found on this teiid server or null
     * @throws KException
     *         if an error occurs
     */
    Connection getConnection(final UnitOfWork transaction, String name) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the drivers found on this teiid server (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Driver[] getDrivers(UnitOfWork transaction, final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name the driver name
     * @return the named driver found on this teiid server or null
     * @throws KException
     *         if an error occurs
     */
    Driver getDriver(final UnitOfWork transaction, String name) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the templates found on this teiid server (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Template[] getTemplates(UnitOfWork transaction, String... namePatterns) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name the template name
     * @return the named template found on this teiid server or null
     * @throws KException
     *         if an error occurs
     */
    Template getTemplate(UnitOfWork transaction, String name) throws KException;

    /**
     * Refresh VDBs with the supplied names
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param teiidInstance the teiid instance
     * @param vdbNames the Vdb names
     * @throws KException
     *         if an error occurs
     */
    void refreshVdbs(final UnitOfWork transaction, TeiidInstance teiidInstance, final String... vdbNames) throws KException;

    /**
     * Refresh connections with the supplied names
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param teiidInstance the teiid instance
     * @param connectionNames the connection names
     * @throws KException
     *         if an error occurs
     */
    void refreshConnections(final UnitOfWork transaction, TeiidInstance teiidInstance, String... connectionNames) throws KException;

    /**
     * Refresh Translators with the supplied names
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param teiidInstance the teiid instance
     * @param translatorNames the translator names
     * @throws KException
     *         if an error occurs
     */
    void refreshTranslators(final UnitOfWork transaction, TeiidInstance teiidInstance, String... translatorNames) throws KException;

    /**
     * Refresh Drivers with the supplied names
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param teiidInstance the teiid instance
     * @param driverNames the driver names
     * @throws KException
     *         if an error occurs
     */
    void refreshDrivers(final UnitOfWork transaction, TeiidInstance teiidInstance, String... driverNames) throws KException;

    /**
     * Refresh Templates with the supplied names
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param teiidInstance the teiid instance
     * @param templateNames the template names
     * @throws KException
     *         if an error occurs
     */
    void refreshTemplates(final UnitOfWork transaction, TeiidInstance teiidInstance, String... templateNames) throws KException;

    /**
     * @param uow
     * @return the timestamp when the cached teiid was created
     * @throws KException 
     */
    Long getTimestamp(UnitOfWork uow) throws KException;
}
