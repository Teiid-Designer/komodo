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
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceImpl;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.spi.KException;
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
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Implementation of teiid instance model
 */
public class TeiidImpl extends RelationalChildRestrictedObject implements Teiid, EventManager {

    private class TeiidJdbcInfoImpl implements TeiidJdbcInfo {

        @Override
        public ConnectivityType getType() {
            return ConnectivityType.JDBC;
        }

        @Override
        public HostProvider getHostProvider() {
            return teiidParent;
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
            try {
                return getJdbcPort(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_PORT;
            }
        }

        @Override
        public void setPort( int port ) {
            try {
                setJdbcPort(transaction, port);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getUsername() {
            try {
                return getJdbcUsername(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_JDBC_USERNAME;
            }
        }

        @Override
        public void setUsername( String userName ) {
            try {
                setJdbcUsername(transaction, userName);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getPassword() {
            try {
                return getJdbcPassword(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_JDBC_PASSWORD;
            }
        }

        @Override
        public void setPassword( String password ) {
            try {
                setJdbcPassword(transaction, password);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public boolean isSecure() {
            try {
                return isJdbcSecure(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_SECURE;
            }
        }

        @Override
        public void setSecure( boolean secure ) {
            try {
                setJdbcSecure(transaction, secure);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }
    }

    private class TeiidAdminInfoImpl implements TeiidAdminInfo {

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
            return teiidParent;
        }

        @Override
        public void setHostProvider( HostProvider hostProvider ) {
            // Nothing to do since this is the host provider
        }

        @Override
        public int getPort() {
            try {
                return getAdminPort(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_PORT;
            }
        }

        @Override
        public void setPort( int port ) {
            try {
                setAdminPort(transaction, port);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getUsername() {
            try {
                return getAdminUser(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_ADMIN_USERNAME;
            }
        }

        @Override
        public void setUsername( String userName ) {
            try {
                setAdminUser(transaction, userName);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getPassword() {
            try {
                return getAdminPassword(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_ADMIN_PASSWORD;
            }
        }

        @Override
        public void setPassword( String password ) {
            try {
                setAdminPassword(transaction, password);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public boolean isSecure() {
            try {
                return isAdminSecure(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_SECURE;
            }
        }

        @Override
        public void setSecure( boolean secure ) {
            try {
                setAdminSecure(transaction, secure);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }
    }

    private class TeiidParentImpl implements TeiidParent {

        private TeiidInstance teiidInstance;

        @Override
        public TeiidInstance getTeiidInstance(TeiidVersion teiidVersion) {
            if (teiidInstance == null) {
                try {
                    TeiidService teiidService = PluginService.getInstance().getTeiidService(teiidVersion);
                    teiidInstance = teiidService.getTeiidInstance(this, jdbcInfo);
                } catch (Exception ex) {
                    KEngine.getInstance().getErrorHandler().error(ex);
                    return null;
                }
            }

            return teiidInstance;
        }

        @Override
        public String getHost() {
            try {
                return TeiidImpl.this.getHost(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return null;
            }
        }

        @Override
        public Teiid getParentObject() {
            return TeiidImpl.this;
        }

        @Override
        public String getId() {
            return null;
        }

        @Override
        public String getName() {
            try {
                return TeiidImpl.this.getName(transaction);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return null;
            }
        }

        @Override
        public int getPort() {
            return adminInfo.getPort();
        }

        @Override
        public String getUsername() {
            return adminInfo.getUsername();
        }

        @Override
        public String getPassword() {
            return adminInfo.getPassword();
        }

        @Override
        public boolean isSecure() {
            return adminInfo.isSecure();
        }

        @Override
        public EventManager getEventManager() {
            return TeiidImpl.this;
        }
    }

    /**
     * Admin info of the teiid connection
     */
    private final TeiidAdminInfoImpl adminInfo;

    /**
     * JDBC info of the teiid connection
     */
    private final TeiidJdbcInfoImpl jdbcInfo;

    /**
     * Parent of the teiid instance
     */
    private final TeiidParentImpl teiidParent;

    private UnitOfWork transaction;

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
        adminInfo = new TeiidAdminInfoImpl();
        jdbcInfo = new TeiidJdbcInfoImpl();
        teiidParent = new TeiidParentImpl();
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Teiid.IDENTIFIER;
    }

    @Override
    public TeiidInstance getTeiidInstance(UnitOfWork uow) {
        this.transaction = uow;
        TeiidVersion version = null;
        try {
            version = getVersion(uow);
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            version = TeiidVersionProvider.getInstance().getTeiidVersion();
        }

        return teiidParent.getTeiidInstance(version);
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
    public void setAdminPort( UnitOfWork uow,
                              int port ) throws KException {
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
    public void setAdminUser( UnitOfWork uow,
                              String userName ) throws KException {
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
    public void setAdminSecure( UnitOfWork uow,
                                boolean secure ) throws KException {
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
    public void setJdbcPort( UnitOfWork uow,
                             int port ) throws KException {
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
    public void setJdbcUsername( UnitOfWork uow,
                                 String userName ) throws KException {
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
    public void setJdbcPassword( UnitOfWork uow,
                                 String password ) throws KException {
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
    public void setJdbcSecure( UnitOfWork uow,
                               boolean secure ) throws KException {
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

        ServerManager mgr = ServerManager.getInstance(getRepository());
        CachedTeiid cachedTeiid = mgr.createCachedTeiid(transaction, this);

        TeiidInstance teiidInstance = getTeiidInstance(transaction);
        if (teiidInstance == null) {
            throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_ERROR));
        }

        try {
            teiidInstance.connect();
            if (! teiidInstance.isConnected()) {
                throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_CONNECTION_ERROR));
            }

            Collection<TeiidVdb> instVdbs = teiidInstance.getVdbs();
            if (instVdbs == null)
                instVdbs = Collections.emptyList();

            Collection<TeiidDataSource> instDataSrcs = teiidInstance.getDataSources();
            if (instDataSrcs == null)
                instDataSrcs = Collections.emptyList();

            Collection<TeiidTranslator> instTranslators = teiidInstance.getTranslators();
            if (instTranslators == null)
                instTranslators = Collections.emptyList();

            //
            // Process the vdbs
            //
            for (TeiidVdb teiidVdb : instVdbs) {
                if (teiidVdb == null)
                    continue;

                // Export the vdb content into a string
                String content = teiidVdb.export();
                if (content == null)
                    continue;

                if (StringUtils.isEmpty(content))
                    continue;

                String vdbName = teiidVdb.getName();

                // Output the content to a temp file
                File tempFile = File.createTempFile(VDB_PREFIX, XML_SUFFIX);
                Files.write(Paths.get(tempFile.getPath()), content.getBytes());

                KomodoObject kobject = cachedTeiid.addChild(transaction, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
                Vdb vdb = new VdbImpl( transaction, getRepository(), kobject.getAbsolutePath());
                vdb.setOriginalFilePath(transaction, tempFile.getAbsolutePath());
                vdb.setVdbName( transaction, vdbName );

                KomodoObject fileNode = vdb.addChild(transaction, JcrLexicon.CONTENT.getString(), null);
                fileNode.setProperty(transaction, JcrLexicon.DATA.getString(), content);
            }

            for (TeiidDataSource teiidDataSrc : instDataSrcs) {
                if (teiidDataSrc == null)
                    continue;

                KomodoObject kobject = cachedTeiid.addChild( transaction, teiidDataSrc.getName(), KomodoLexicon.DataSource.NODE_TYPE );
                Datasource dataSrc = new DatasourceImpl( transaction, getRepository(), kobject.getAbsolutePath() );

                dataSrc.setDriverName(transaction, teiidDataSrc.getType());
                dataSrc.setJndiName(transaction, teiidDataSrc.getJndiName());

                for (Entry<Object, Object> property : teiidDataSrc.getProperties().entrySet()) {
                    dataSrc.setProperty(transaction, property.getKey().toString(), property.getValue());
                }
            }

            for (TeiidTranslator teiidTranslator : instTranslators) {
                if (teiidTranslator == null)
                    continue;

                KomodoObject kObject = cachedTeiid.addChild(transaction,
                                                                                                                  teiidTranslator.getName(),
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

        } catch (Exception ex) {
            throw new KException(ex);
        }

        return cachedTeiid;
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

}
