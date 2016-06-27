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
package org.komodo.relational.datasource;

import java.util.Properties;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.datasource.internal.DatasourceImpl;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * A model of a datasource instance
 */
public interface Datasource extends Exportable, RelationalObject {

    /**
     * XML tag for jdbc attribute
     */
    public static String XML_ATTR_JDBC = "jdbc"; //$NON-NLS-1$
    /**
     * XML tag for name attribute
     */
    public static String XML_ATTR_NAME = "name"; //$NON-NLS-1$
    /**
     * XML tag for property element
     */
    public static String XML_ELEM_PROPERTY = "property"; //$NON-NLS-1$
    /**
     * XML tag for dataSource element
     */
    public static String XML_ELEM_DATASOURCE = "dataSource"; //$NON-NLS-1$
    /**
     * XML tag for dataSourceSet element
     */
    public static String XML_ELEM_DATASOURCE_SET = "dataSourceSet"; //$NON-NLS-1$
    
    /**
     * The type identifier.
     */
    int TYPE_ID = Datasource.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.DATASOURCE;

    /**
     * An empty array of data sources.
     */
    Datasource[] NO_DATASOURCES = new Datasource[0];

    /**
     * The default value for the <code>jdbc</code> property. Value is {@value} .
     */
    boolean DEFAULT_JDBC = true;

    /**
     * The default value for the <code>preview</code> property. Value is {@value} .
     */
    boolean DEFAULT_PREVIEW = false;
    
    /**
     * The resolver of a {@link Datasource}.
     */
    public static final TypeResolver< Datasource > RESOLVER = new TypeResolver< Datasource >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Datasource create( final UnitOfWork transaction,
                             final Repository repository,
                             final KomodoObject parent,
                             final String id,
                             final RelationalProperties properties ) throws KException {
            final WorkspaceManager mgr = WorkspaceManager.getInstance( repository );
            return mgr.createDatasource( transaction, parent, id );
        }
    
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
        public Class< DatasourceImpl > owningClass() {
            return DatasourceImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, KomodoLexicon.DataSource.NODE_TYPE );
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Datasource resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Datasource.TYPE_ID ) {
                return ( Datasource )kobject;
            }
            return new DatasourceImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    
    };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return id of this datasource
     * @throws KException
     */
    String getId(UnitOfWork uow) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return jndi name of this datasource (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getJndiName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param jndiName jndi name of this datasource
     * @throws KException if error occurs
     */
    void setJndiName(UnitOfWork transaction, String jndiName) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return driver name of this datasource.  (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getDriverName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param driverName driver name of this datasource
     * @throws KException if error occurs
     */
    void setDriverName(UnitOfWork transaction, String driverName) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return class name of this datasource.  (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getClassName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param className class name of this datasource
     * @throws KException if error occurs
     */
    void setClassName(UnitOfWork transaction, String className) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return profile name of this datasource.  (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getProfileName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param profileName profile name of this datasource
     * @throws KException if error occurs
     */
    void setProfileName(UnitOfWork transaction, String profileName) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return 'true' if a JDBC source, 'false' if not.
     * @throws KException if error occurs
     */
    boolean isJdbc(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param isJdbc 'true' if source is JDBC, 'false' if not.
     * @throws KException if error occurs
     */
    void setJdbc(UnitOfWork transaction, boolean isJdbc) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return 'true' if a Preview source, 'false' if not.
     * @throws KException if error occurs
     */
    boolean isPreview(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param isPreview 'true' if source is Preview, 'false' if not.
     * @throws KException if error occurs
     */
    void setPreview(UnitOfWork transaction, boolean isPreview) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param teiidInstance the teiid instance for deployment
     * @return the properties for server deployment
     * @throws Exception if error occurs
     */
    Properties getPropertiesForServerDeployment(UnitOfWork transaction, TeiidInstance teiidInstance) throws Exception;

}
