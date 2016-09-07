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
package org.komodo.relational.teiid.internal;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.Map.Entry;
import java.util.Properties;
import org.komodo.core.KEngine;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoLexicon.TeiidArchetype;
import org.komodo.osgi.PluginService;
import org.komodo.relational.Messages;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceImpl;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.SynchronousCallback;
import org.komodo.spi.KException;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Implementation of teiid instance model
 */
public class TeiidImpl extends RelationalChildRestrictedObject implements Teiid, TeiidParent, EventManager {

    private class TeiidJdbcInfoImpl implements TeiidJdbcInfo {

        @Override
        public ConnectivityType getType() {
            return ConnectivityType.JDBC;
        }

        @Override
        public HostProvider getHostProvider() {
            return TeiidImpl.this;
        }

        @Override
        public void setHostProvider( HostProvider hostProvider ) {
            // host provider is provided by the TeiidImpl class
            // so this should do nothing
        }

        @Override
        public String getUrl( String vdbName ) {
            StringBuilder sb = new StringBuilder();
            sb.append(JDBC_TEIID_PREFIX);
            sb.append(vdbName);
            sb.append(AT);
            sb.append(isSecure() ? MMS : MM);
            sb.append(getHostProvider().getHost());
            sb.append(COLON);
            sb.append(getPort());

            return sb.toString();
        }

        /**
         * jdbc:teiid:<vdbname>@mm<s>://host:port
         */
        @Override
        public String getUrl() {
            return getUrl(VDB_PLACEHOLDER);
        }

        @Override
        public int getPort() {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               int port = getJdbcPort(uow);
               commit(uow);
               return port;
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
                return -1;
            }
        }

        @Override
        public void setPort( int port ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setJdbcPort(uow, port);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }

        @Override
        public String getUsername() {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               String user = getJdbcUsername(uow);
               commit(uow);
               return user;
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
                return null;
            }
        }

        @Override
        public void setUsername( String userName ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setJdbcUsername(uow, userName);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }

        @Override
        public String getPassword() {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               String passwd = getJdbcPassword(uow);
               commit(uow);
               return passwd;
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
                return null;
            }
        }

        @Override
        public void setPassword( String password ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setJdbcPassword(uow, password);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }

        @Override
        public boolean isSecure() {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               boolean secure = isJdbcSecure(uow);
               commit(uow);
               return secure;
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
                return false;
            }
        }

        @Override
        public void setSecure( boolean secure ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setJdbcSecure(uow, secure);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }
    }

    class TeiidAdminInfoImpl implements TeiidAdminInfo {

        @Override
        public ConnectivityType getType() {
            return ConnectivityType.ADMIN;
        }

        @Override
        public String getUrl() {
            StringBuilder sb = new StringBuilder();
            sb.append(isSecure() ? MMS : MM);
            sb.append(getHostProvider().getHost());
            sb.append(':');
            sb.append(getPort());

            return sb.toString();
        }

        @Override
        public HostProvider getHostProvider() {
            return TeiidImpl.this;
        }

        @Override
        public void setHostProvider( HostProvider hostProvider ) {
            // Nothing to do since this is the host provider
        }

        @Override
        public int getPort() {
            return TeiidImpl.this.getPort();
        }

        @Override
        public void setPort( int port ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setAdminPort(uow, port);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }

        @Override
        public String getUsername() {
            return TeiidImpl.this.getUsername();
        }

        @Override
        public void setUsername( String userName ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setAdminUser(uow, userName);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }

        @Override
        public String getPassword() {
            return TeiidImpl.this.getPassword();
        }

        @Override
        public void setPassword( String password ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setAdminPassword(uow, password);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }

        @Override
        public boolean isSecure() {
            return TeiidImpl.this.isSecure();
        }

        @Override
        public void setSecure( boolean secure ) {
            UnitOfWork uow = null;
            try {
               uow = getOrCreateTransaction();
               setAdminSecure(uow, secure);
               commit(uow);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                if (uow != null)
                    uow.rollback();
            }
        }
    }

    private volatile UnitOfWork currentTransaction = null;

    /**
     * User responsible for creating this teiid object
     */
    private final String txUser;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public TeiidImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
        this.txUser = uow.getUserName();
    }

    /**
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected UnitOfWork createTransaction() throws KException {
        final SynchronousCallback callback = new SynchronousCallback();
        final UnitOfWork result = getRepository().createTransaction(txUser,
                                                                    ( getClass().getSimpleName() + System.currentTimeMillis() ),
                                                                        false, callback );
        LOGGER.debug( "createTransaction:created '{0}', rollbackOnly = '{1}'", result.getName(), result.isRollbackOnly() ); //$NON-NLS-1$
        return result;
    }

    private void setCurrentTransaction(UnitOfWork currentTransaction) {
        this.currentTransaction = currentTransaction;
    }

    private UnitOfWork getOrCreateTransaction() throws KException {
        if (currentTransaction == null)
            return createTransaction();

        if (State.NOT_STARTED != currentTransaction.getState()) {
            //
            // current tx is no longer valid since its been committed / rolled back
            //
            currentTransaction = null;
            return createTransaction();
        }

        return currentTransaction;
    }

    private void commit(UnitOfWork uow) {
        if (uow == currentTransaction)
            return; // Don't commit transaction as its being stashed for the moment

        if (! uow.getName().startsWith(getClass().getSimpleName()))
            return;

        uow.commit();
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Teiid.IDENTIFIER;
    }

    protected TeiidInstance getTeiidInstance(UnitOfWork uow, TeiidVersion teiidVersion) {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            TeiidService teiidService = PluginService.getInstance().getTeiidService(teiidVersion);
            TeiidJdbcInfo jdbcInfo = new TeiidJdbcInfoImpl();

            //
            // The teiid service defers back to this class (as the TeiidParent) for
            // various settings and we want to try and keep it in the same
            // transaction if we can
            //
            setCurrentTransaction(uow);

            return teiidService.getTeiidInstance(this, jdbcInfo);
        } catch (Exception ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            return null;
        }
    }

    @Override
    public TeiidInstance getTeiidInstance(TeiidVersion teiidVersion) {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           TeiidInstance teiidInstance = getTeiidInstance(uow, teiidVersion);
           commit(uow);
           return teiidInstance;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            return null;
        }
    }

    @Override
    public TeiidInstance getTeiidInstance(UnitOfWork uow) {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        TeiidVersion version = null;
        try {
            version = getVersion(uow);
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            version = TeiidVersionProvider.getInstance().getTeiidVersion();
        }

        return getTeiidInstance(uow, version);
    }

    @Override
    public QueryService getQueryService(UnitOfWork uow) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        TeiidVersion version = null;
        try {
            version = getVersion(uow);
            TeiidService teiidService = PluginService.getInstance().getTeiidService(version);

            String host = getHost(uow);
            int port = getJdbcPort(uow);
            String user = getJdbcUsername(uow);
            String passwd = getJdbcPassword(uow);
            boolean isSecure = isJdbcSecure(uow);

            return teiidService.getQueryService(host, port, user, passwd, isSecure);
        } catch (Exception ex) {
            throw RelationalModelFactory.handleError(ex);
        }
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid host property
     * @throws KException
     *         if error occurs
     */
    @Override
    public TeiidVersion getVersion( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        String version = getObjectProperty(uow, PropertyValueType.STRING, "getVersion", TeiidArchetype.VERSION); //$NON-NLS-1$
        return version != null ? new DefaultTeiidVersion(version) : TeiidVersionProvider.getInstance().getTeiidVersion();
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * param host the host name
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setVersion(UnitOfWork uow, TeiidVersion version) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setVersion", TeiidArchetype.VERSION, version.toString()); //$NON-NLS-1$
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid id property (never empty)
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getId( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Property prop = getRawProperty( transaction, JcrLexicon.UUID.getString() );
        final String result = prop.getStringValue( transaction );
        return result;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid host property
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getHost( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        String host = getObjectProperty(uow, PropertyValueType.STRING, "getHost", TeiidArchetype.HOST); //$NON-NLS-1$
        return host != null ? host : HostProvider.DEFAULT_HOST;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * param host the host name
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setHost(UnitOfWork uow, String host) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setHost", TeiidArchetype.HOST, host); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid admin port property
     * @throws KException
     *         if error occurs
     */
    @Override
    public int getAdminPort( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Long port = getObjectProperty(uow, PropertyValueType.LONG, "getAdminPort", TeiidArchetype.ADMIN_PORT); //$NON-NLS-1$
        return port != null ? port.intValue() : TeiidAdminInfo.DEFAULT_PORT;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param port
     *        new value of admin port property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setAdminPort( UnitOfWork uow, int port ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setAdminPort", TeiidArchetype.ADMIN_PORT, port); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid admin user property
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getAdminUser( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        String user = getObjectProperty(uow, PropertyValueType.STRING, "getAdminUser", TeiidArchetype.ADMIN_USER); //$NON-NLS-1$
        return user != null ? user : TeiidAdminInfo.DEFAULT_ADMIN_USERNAME;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param userName
     *        new value of admin username property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setAdminUser( UnitOfWork uow, String userName ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setAdminUser", TeiidArchetype.ADMIN_USER, userName); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid admin password property
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getAdminPassword( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        String password = getObjectProperty(uow, PropertyValueType.STRING, "getAdminPassword", TeiidArchetype.ADMIN_PSWD); //$NON-NLS-1$
        return password != null ? password : TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param password
     *        new value of admin password property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setAdminPassword( UnitOfWork uow,
                                  String password ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setAdminPassword", TeiidArchetype.ADMIN_PSWD, password); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid secure property
     * @throws KException
     *         if error occurs
     */
    @Override
    public boolean isAdminSecure( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Boolean secure = getObjectProperty(uow, PropertyValueType.BOOLEAN, "isSecure", TeiidArchetype.ADMIN_SECURE); //$NON-NLS-1$
        return secure != null ? secure : TeiidAdminInfo.DEFAULT_SECURE;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param secure
     *        new value of admin secure property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setAdminSecure( UnitOfWork uow, boolean secure ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setAdminSecure", TeiidArchetype.ADMIN_SECURE, secure); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid jdbc port property
     * @throws KException
     *         if error occurs
     */
    @Override
    public int getJdbcPort( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Long port = getObjectProperty(uow, PropertyValueType.LONG, "getPort", TeiidArchetype.JDBC_PORT); //$NON-NLS-1$
        return port != null ? port.intValue() : TeiidJdbcInfo.DEFAULT_PORT;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param port
     *        new value of jdbc port property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setJdbcPort( UnitOfWork uow, int port ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setPort", TeiidArchetype.JDBC_PORT, port); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid jdbc user property
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getJdbcUsername( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        String user = getObjectProperty(uow, PropertyValueType.STRING, "getUsername", TeiidArchetype.JDBC_USER); //$NON-NLS-1$
        return user != null ? user : TeiidJdbcInfo.DEFAULT_JDBC_USERNAME;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param userName
     *        new value of jdbc username property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setJdbcUsername( UnitOfWork uow, String userName ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setUsername", TeiidArchetype.JDBC_USER, userName); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid JDBC password property
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getJdbcPassword( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        String password = getObjectProperty(uow, PropertyValueType.STRING, "getPassword", TeiidArchetype.JDBC_PSWD); //$NON-NLS-1$
        return password != null ? password : TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param password
     *        new value of jdbc password property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setJdbcPassword( UnitOfWork uow, String password ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setPassword", TeiidArchetype.JDBC_PSWD, password); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid secure property
     * @throws KException
     *         if error occurs
     */
    @Override
    public boolean isJdbcSecure( UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Boolean secure = getObjectProperty(uow, PropertyValueType.BOOLEAN, "isSecure", TeiidArchetype.JDBC_SECURE); //$NON-NLS-1$
        return secure != null ? secure : TeiidJdbcInfo.DEFAULT_SECURE;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param secure
     *        new value of jdbc secure property
     * @throws KException
     *         if error occurs
     */
    @Override
    public void setJdbcSecure( UnitOfWork uow, boolean secure ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        setObjectProperty(uow, "setSecure", TeiidArchetype.JDBC_SECURE, secure); //$NON-NLS-1$
    }

    @Override
    public boolean isConnected(UnitOfWork uow) {
        TeiidInstance teiidInstance = getTeiidInstance(uow);
        return teiidInstance.isConnected();
    }

    @Override
    public CachedTeiid importContent(UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isTrue(RepositoryImpl.isSystemTx(transaction), "transaction should be owned by " + Repository.SYSTEM_USER);

        KomodoObject teiidCache = getRepository().komodoTeiidCache(transaction);

        long expireThreshold = CachedTeiid.DEFAULT_TEIID_CACHE_THRESHOLD;
        Property expProp = teiidCache.getProperty(transaction, KomodoLexicon.TeiidCache.EXPIRATION_THRESHOLD);
        if (expProp == null)
            teiidCache.setProperty(transaction, KomodoLexicon.TeiidCache.EXPIRATION_THRESHOLD, expireThreshold);
        else
            expireThreshold = expProp.getLongValue(transaction);

        final String id = getName(transaction);
        if (teiidCache.hasChild(transaction, id)) {
            KomodoObject child = teiidCache.getChild(transaction, id);
            CachedTeiidImpl currCTeiid = new CachedTeiidImpl(transaction, getRepository(), child.getAbsolutePath());
            Long timestamp = currCTeiid.getTimestamp(transaction);
            Long current = System.currentTimeMillis();

            // Expiration time of 10 mins has elapsed or not
            if ((timestamp + expireThreshold) > current)
                return currCTeiid;
        }

        //
        // Either cache teiid does not exist or should be overwritten
        //
        ServerManager mgr = ServerManager.getInstance(getRepository());
        CachedTeiid cachedTeiid = mgr.createCachedTeiid(transaction, this);

        // Gets a teiid instance and connects if not connected
        TeiidInstance teiidInstance = getConnectedTeiidInstance(transaction);

        // Get the teiid instance artifacts and add them to CachedTeiid
        try {
            Collection<TeiidVdb> instVdbs = teiidInstance.getVdbs();
            if (instVdbs == null) instVdbs = Collections.emptyList();

            Collection<TeiidDataSource> instDataSrcs = teiidInstance.getDataSources();
            if (instDataSrcs == null) instDataSrcs = Collections.emptyList();

            Collection<TeiidTranslator> instTranslators = teiidInstance.getTranslators();
            if (instTranslators == null) instTranslators = Collections.emptyList();

            Collection<String> instDataSourceTypeNames = teiidInstance.getDataSourceTypeNames();
            if (instDataSourceTypeNames == null) instDataSourceTypeNames = Collections.emptyList();

            // Add VDBs to CachedTeiid
            for (TeiidVdb teiidVdb : instVdbs) {
                if (teiidVdb == null)
                    continue;

                // Export the vdb content into a string
                String content = teiidVdb.export();
                if (StringUtils.isEmpty(content))
                    continue;

                // Add VDB to CachedTeiid
                addOrReplaceCachedVdb(transaction, teiidVdb, cachedTeiid);
            }

            // Add DataSources to CachedTeiid
            for (TeiidDataSource teiidDataSrc : instDataSrcs) {
                if (teiidDataSrc == null)
                    continue;

                addOrReplaceCachedDataSource(transaction, teiidDataSrc, cachedTeiid);
            }

            // Add Translators to CachedTeiid
            for (TeiidTranslator teiidTranslator : instTranslators) {
                if (teiidTranslator == null)
                    continue;
                addOrReplaceCachedTranslator(transaction, teiidTranslator, cachedTeiid);
            }
            
            // Add Drivers to CachedTeiid
            for (String teiidDSType : instDataSourceTypeNames) {
                if (teiidDSType == null)
                    continue;

                addOrReplaceCachedDriver(transaction, teiidDSType, cachedTeiid);
            }

            
        } catch (Exception ex) {
            throw new KException(ex);
        }

        return cachedTeiid;
    }

    @Override
    public void updateCacheWithServerVdb(UnitOfWork transaction, String vdbName) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // If the CachedTeiid exists, refresh the VDBs
        CachedTeiid cachedTeiid = getCachedTeiid(transaction);
        if(cachedTeiid!=null) {
            TeiidInstance teiidInstance = getConnectedTeiidInstance(transaction);

            try {
                TeiidVdb instVdb = teiidInstance.getVdb(vdbName);
                if(instVdb==null) return;

                addOrReplaceCachedVdb(transaction, instVdb, cachedTeiid);
            } catch (Exception ex) {
                throw new KException(ex);
            }            
        }
    }
    
    @Override
    public void updateCacheWithServerDataSource(UnitOfWork transaction, String dataSourceName) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // If the CachedTeiid exists, replace the DataSource
        CachedTeiid cachedTeiid = getCachedTeiid(transaction);
        if(cachedTeiid!=null) {
            TeiidInstance teiidInstance = getConnectedTeiidInstance(transaction);

            try {
                TeiidDataSource instDataSrc = teiidInstance.getDataSource(dataSourceName);
                if (instDataSrc == null) return;
                
                addOrReplaceCachedDataSource(transaction, instDataSrc, cachedTeiid);
            } catch (Exception ex) {
                throw new KException(ex);
            }            
        }
    }
    
    @Override
    public void updateCacheWithServerTranslator(UnitOfWork transaction, String translatorName) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // If the CachedTeiid exists, replace the Translator
        CachedTeiid cachedTeiid = getCachedTeiid(transaction);
        if(cachedTeiid!=null) {
            TeiidInstance teiidInstance = getConnectedTeiidInstance(transaction);

            try {
                TeiidTranslator instTranslator = teiidInstance.getTranslator(translatorName);
                if (instTranslator == null) return;

                addOrReplaceCachedTranslator(transaction, instTranslator, cachedTeiid);
                
            } catch (Exception ex) {
                throw new KException(ex);
            }            
        }
    }
    
    @Override
    public void updateCacheWithServerDriver(UnitOfWork transaction, String driverName) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // If the CachedTeiid exists, replace the Driver
        CachedTeiid cachedTeiid = getCachedTeiid(transaction);
        if(cachedTeiid!=null) {
            TeiidInstance teiidInstance = getConnectedTeiidInstance(transaction);

            try {
                Collection<String> instDataSourceTypeNames = teiidInstance.getDataSourceTypeNames();
                if(instDataSourceTypeNames.contains(driverName)) {
                    addOrReplaceCachedDriver(transaction, driverName, cachedTeiid);
                }
            } catch (Exception ex) {
                throw new KException(ex);
            }            
        }
    }
    
    /*
     * Get a connected teiid instance.  If there is a problem getting the instance or connecting - an exception is thrown.
     */
    private TeiidInstance getConnectedTeiidInstance(UnitOfWork transaction) throws KException {
        TeiidInstance teiidInstance = getTeiidInstance(transaction);
        if (teiidInstance == null) {
            throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_ERROR));
        }
        try {
            teiidInstance.connect();
            if (! teiidInstance.isConnected()) {
                throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_CONNECTION_ERROR));
            }
        } catch (Exception ex) {
            throw new KException(ex);
        }
        return teiidInstance;
    }
    
    /*
     * Returns the CachedTeiid instance if it exists.  If doesnt exist, null is returned.
     */
    private CachedTeiid getCachedTeiid(UnitOfWork transaction) throws KException {
        CachedTeiid cachedTeiid = null;
        
        KomodoObject teiidCache = getRepository().komodoTeiidCache(transaction);
        final String id = getName(transaction);
        if (teiidCache.hasChild(transaction, id)) {
            KomodoObject child = teiidCache.getChild(transaction, id);
            cachedTeiid = CachedTeiid.RESOLVER.resolve(transaction, child);
        } 
        
        return cachedTeiid;
    }
        
    /*
     * Adds the server VDB to CachedTeiid.  If there is a VDB with same name in CachedTeiid, it is replaced
     */
    private void addOrReplaceCachedVdb(UnitOfWork transaction, TeiidVdb teiidVdb, CachedTeiid cachedTeiid) throws Exception {
        String vdbName = teiidVdb.getName();
        
        // Export the vdb content into a string
        String content = teiidVdb.export();
        if (content == null || StringUtils.isEmpty(content)) return;

        // Output the content to a temp file
        File tempFile = File.createTempFile(VDB_PREFIX, XML_SUFFIX);
        Files.write(Paths.get(tempFile.getPath()), content.getBytes());

        KomodoObject vdbsFolder = cachedTeiid.getChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);
        
        // Removes currently cached object, if it exists
        if(vdbsFolder.hasChild(transaction, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
            KomodoObject existingObj = vdbsFolder.getChild(transaction, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            existingObj.remove(transaction);
        }

        KomodoObject kobject = vdbsFolder.addChild(transaction, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        Vdb vdb = new VdbImpl( transaction, getRepository(), kobject.getAbsolutePath());
        vdb.setOriginalFilePath(transaction, tempFile.getAbsolutePath());
        vdb.setVdbName( transaction, vdbName );

        KomodoObject fileNode = vdb.addChild(transaction, JcrLexicon.CONTENT.getString(), null);
        fileNode.setProperty(transaction, JcrLexicon.DATA.getString(), content);
    }
    
    /*
     * Adds the server DataSource to CachedTeiid.  If there is a DataSource with same name in CachedTeiid, it is replaced
     */
    private void addOrReplaceCachedDataSource(UnitOfWork transaction, TeiidDataSource teiidDS, CachedTeiid cachedTeiid) throws Exception {
        String dataSourceName = teiidDS.getName();
        
        KomodoObject dataSourcesFolder = cachedTeiid.getChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        // Removes currently cached object, if it exists
        if(dataSourcesFolder.hasChild(transaction, dataSourceName, DataVirtLexicon.Connection.NODE_TYPE)) {
            KomodoObject existingObj = dataSourcesFolder.getChild(transaction, dataSourceName, DataVirtLexicon.Connection.NODE_TYPE);
            existingObj.remove(transaction);
        }

        KomodoObject kobject = dataSourcesFolder.addChild( transaction, dataSourceName, DataVirtLexicon.Connection.NODE_TYPE );
        Datasource dataSrc = new DatasourceImpl( transaction, getRepository(), kobject.getAbsolutePath() );

        dataSrc.setDriverName(transaction, teiidDS.getType());
        dataSrc.setJndiName(transaction, teiidDS.getJndiName());

        for (Entry<Object, Object> property : teiidDS.getProperties().entrySet()) {
            String key = property.getKey().toString();
            if (TeiidInstance.DATASOURCE_DRIVERNAME.equals(key) ||
                    TeiidInstance.DATASOURCE_JNDINAME.equals(key))
                continue; // Already set as explicit fields

            dataSrc.setProperty(transaction, key, property.getValue());
        }
    }

    /*
     * Adds the server Translator to CachedTeiid.  If there is a Translator with same name in CachedTeiid, it is replaced
     */
    private void addOrReplaceCachedTranslator(UnitOfWork transaction, TeiidTranslator teiidTranslator, CachedTeiid cachedTeiid) throws Exception {
        String translatorName = teiidTranslator.getName();
        
        KomodoObject translatorsFolder = cachedTeiid.getChild(transaction, CachedTeiid.TRANSLATORS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);
        
        // Removes currently cached object, if it exists
        if(translatorsFolder.hasChild(transaction, translatorName, VdbLexicon.Translator.TRANSLATOR)) {
            KomodoObject existingObj = translatorsFolder.getChild(transaction, translatorName, VdbLexicon.Translator.TRANSLATOR);
            existingObj.remove(transaction);
        }
        
        // create the new object
        KomodoObject kObject = translatorsFolder.addChild(transaction,
                                                          translatorName,
                                                          VdbLexicon.Translator.TRANSLATOR);
        TranslatorImpl translator = new TranslatorImpl(transaction,
                                                       getRepository(),
                                                       kObject.getAbsolutePath());
        translator.setDescription(transaction, teiidTranslator.getDescription());
        String type = teiidTranslator.getType() != null ? teiidTranslator.getType() : teiidTranslator.getName();
        translator.setType(transaction, type);
        Properties props = teiidTranslator.getProperties();
        for (Entry<Object, Object> entry : props.entrySet()) {
            translator.setProperty(transaction, entry.getKey().toString(), entry.getValue());
        }
    }

    /*
     * Adds the server Driver to CachedTeiid.  If there is a Driver with same name in CachedTeiid, it is replaced
     */
    private void addOrReplaceCachedDriver(UnitOfWork transaction, String driverName, CachedTeiid cachedTeiid) throws Exception {
        KomodoObject driversFolder = cachedTeiid.getChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        // Removes currently cached object, if it exists
        if(driversFolder.hasChild(transaction, driverName, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE)) {
            KomodoObject existingObj = driversFolder.getChild(transaction, driverName, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
            existingObj.remove(transaction);
        }
        
        KomodoObject driver = driversFolder.addChild(transaction,
                                                     driverName,
                                                     DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);

        byte[] content = new byte[1024];
        KomodoObject fileNode;
        if (! driver.hasChild(transaction, JcrLexicon.CONTENT.getString()))
            fileNode = driver.addChild(transaction, JcrLexicon.CONTENT.getString(), null);
        else
            fileNode = driver.getChild(transaction, JcrLexicon.CONTENT.getString());

        ByteArrayInputStream stream = new ByteArrayInputStream(content);
        fileNode.setProperty(transaction, JcrLexicon.DATA.getString(), stream);
    }

    @Override
    public boolean addListener( ExecutionConfigurationListener listener ) {
        return false;
    }

    @Override
    public void permitListeners( boolean enable ) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public void notifyListeners( ExecutionConfigurationEvent event ) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public boolean removeListener( ExecutionConfigurationListener listener ) {
        return false;
    }

    @Override
    public boolean isSound() {
        try {
            if (getRepository() == null)
                return false;

            if (! Repository.State.REACHABLE.equals(getRepository().getState()))
                return false;

            KomodoObject kObject = getRepository().getFromWorkspace(getOrCreateTransaction(), this.getAbsolutePath());
            return kObject != null;
        } catch (KException ex) {
            return false;
        }
    }

    @Override
    public String getHost() {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           String host = getHost(uow);
           commit(uow);
           return host;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();
            return null;
        }
    }

    @Override
    public int getPort() {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           int port = getAdminPort(uow);
           commit(uow);
           return port;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();
            return -1;
        }
    }

    @Override
    public Object getParentObject() {
        return this;
    }

    @Override
    public String getId() {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           String id = getId(uow);
           commit(uow);
           return id;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();
            return null;
        }
    }

    @Override
    public String getName() {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           String name = getName(uow);
           commit(uow);
           return name;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();
            return null;
        }
    }

    @Override
    public String getUsername() {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           String user = getAdminUser(uow);
           commit(uow);
           return user;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();
            return null;
        }
    }

    @Override
    public String getPassword() {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           String passwd = getAdminPassword(uow);
           commit(uow);
           return passwd;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();
            return null;
        }
    }

    @Override
    public boolean isSecure() {
        UnitOfWork uow = null;
        try {
           uow = getOrCreateTransaction();
           boolean secure = isAdminSecure(uow);
           commit(uow);
           return secure;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();
            return false;
        }
    }

    @Override
    public EventManager getEventManager() {
        return this;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();

        UnitOfWork uow = null;

        try {
            uow = getOrCreateTransaction();

            int nameHash = getName(uow).hashCode();
            result = prime * result + nameHash;

            int hostHash = getHost(uow).hashCode();
            result = prime * result + hostHash;

            int versionHash = getVersion(uow).hashCode();
            result = prime * result + versionHash;

            int adminPwdHash = getAdminPassword(uow).hashCode();
            result = prime * result + adminPwdHash;

            int adminUserHash = getAdminUser(uow).hashCode();
            result = prime * result + adminUserHash;

            int adminPortHash = getAdminPort(uow);
            result = prime * result + adminPortHash;

            int adminSecHash = (isAdminSecure(uow) ? 1231 : 1237);
            result = prime * result + adminSecHash;

            int jdbcPwdHash = getJdbcPassword(uow).hashCode();
            result = prime * result + jdbcPwdHash;

            int jdbcUserHash = getJdbcUsername(uow).hashCode();
            result = prime * result + jdbcUserHash;

            int jdbcPortHash = getJdbcPort(uow);
            result = prime * result + jdbcPortHash;

            int jdbcSecHash = (isJdbcSecure(uow) ? 1231 : 1237);
            result = prime * result + jdbcSecHash;

            commit(uow);
            return result;

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            return System.identityHashCode(this);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        TeiidImpl other = (TeiidImpl)obj;
        UnitOfWork uow = null;

        try {
            uow = getOrCreateTransaction();

            if (! getName(uow).equals(other.getName(uow)))
                return false;
            if (! getHost(uow).equals(other.getHost(uow)))
                return false;
            if (! getVersion(uow).equals(other.getVersion(uow)))
                return false;

            if (! getAdminPassword(uow).equals(other.getAdminPassword(uow)))
                return false;
            if (! getAdminUser(uow).equals(other.getAdminUser(uow)))
                return false;
            if (getAdminPort(uow) != other.getAdminPort(uow))
                return false;
            if (isAdminSecure(uow) != other.isAdminSecure(uow))
                return false;

            if (! getJdbcPassword(uow).equals(other.getJdbcPassword(uow)))
                return false;
            if (! getJdbcUsername(uow).equals(other.getJdbcUsername(uow)))
                return false;
            if (getJdbcPort(uow) != other.getJdbcPort(uow))
                return false;
            if (isJdbcSecure(uow) != other.isJdbcSecure(uow))
                return false;

            commit(uow);
            return true;

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            return false;
        }
    }

}
