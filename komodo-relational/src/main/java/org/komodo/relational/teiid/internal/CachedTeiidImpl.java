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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoLexicon.TeiidArchetype;
import org.komodo.relational.Messages;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceImpl;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.resource.internal.DriverImpl;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
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
public class CachedTeiidImpl extends RelationalObjectImpl implements CachedTeiid {

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
    public CachedTeiidImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param srcTeiid
     *        the source teiid object
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public CachedTeiidImpl(UnitOfWork transaction, Teiid srcTeiid, String path) throws KException {
        this(transaction, srcTeiid.getRepository(), path);

        this.setVersion(transaction, srcTeiid.getVersion(transaction));
        this.setHost(transaction, srcTeiid.getHost(transaction));

        this.setAdminUser(transaction, srcTeiid.getAdminUser(transaction));
        this.setAdminPassword(transaction, srcTeiid.getAdminPassword(transaction));
        this.setAdminPort(transaction, srcTeiid.getAdminPort(transaction));
        this.setAdminSecure(transaction, srcTeiid.isAdminSecure(transaction));

        this.setJdbcUsername(transaction, srcTeiid.getJdbcUsername(transaction));
        this.setJdbcPassword(transaction, srcTeiid.getJdbcPassword(transaction));
        this.setJdbcPort(transaction, srcTeiid.getJdbcPort(transaction));
        this.setJdbcSecure(transaction, srcTeiid.isJdbcSecure(transaction));

        this.setTimestamp(transaction, System.currentTimeMillis());
        
        // Add child folders which will contain the various types
        this.addChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);
        this.addChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE);
        this.addChild(transaction, CachedTeiid.TRANSLATORS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);
        this.addChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return CachedTeiid.IDENTIFIER;
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
    protected void setVersion(UnitOfWork uow, TeiidVersion version) throws KException {
        setObjectProperty(uow, "setVersion", TeiidArchetype.VERSION, version.toString()); //$NON-NLS-1$
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid timestamp property (never empty)
     * @throws KException
     *         if error occurs
     */
    @Override
    public Long getTimestamp(UnitOfWork uow) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Long timestamp = getObjectProperty(uow, PropertyValueType.LONG, "getTimestamp", KomodoLexicon.CachedTeiid.TIMESTAMP); //$NON-NLS-1$
        return timestamp == null ? 0L : timestamp;
    }

    protected void setTimestamp(UnitOfWork uow, long timestamp) throws KException {
        setObjectProperty(uow, "setTimestamp", KomodoLexicon.CachedTeiid.TIMESTAMP, timestamp); //$NON-NLS-1$
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
    protected void setHost(UnitOfWork uow, String host) throws KException {
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
    protected void setAdminPort( UnitOfWork uow,
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
    protected void setAdminUser( UnitOfWork uow,
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
    protected void setAdminPassword( UnitOfWork uow,
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
    protected void setAdminSecure( UnitOfWork uow,
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
    protected void setJdbcPort( UnitOfWork uow,
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
    protected void setJdbcUsername( UnitOfWork uow,
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
    protected void setJdbcPassword( UnitOfWork uow,
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
    protected void setJdbcSecure( UnitOfWork uow,
                               boolean secure ) throws KException {
        setObjectProperty(uow, "setSecure", TeiidArchetype.JDBC_SECURE, secure); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.teiid.CachedTeiid#getVdbs(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Vdb[] getVdbs(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if(!super.hasChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return new Vdb[0];
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);
        
        final List<Vdb> result = new ArrayList<Vdb>();
        for (final KomodoObject kobject : folderNode.getChildrenOfType(transaction, VdbLexicon.Vdb.VIRTUAL_DATABASE, namePatterns)) {
            final Vdb vdb = new VdbImpl(transaction, getRepository(), kobject.getAbsolutePath());
            result.add(vdb);
        }

        if (result.isEmpty()) {
            return Vdb.NO_VDBS;
        }

        return result.toArray(new Vdb[result.size()]);
    }

    @Override
    public Vdb getVdb(UnitOfWork transaction, String name) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if(!super.hasChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return null;
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        if (folderNode.hasChild(transaction, name, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
            KomodoObject kobject = folderNode.getChild(transaction, name, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            return new VdbImpl(transaction, getRepository(), kobject.getAbsolutePath());
        }

        return null;
    }

    @Override
    public Translator[] getTranslators(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if(!super.hasChild(transaction, CachedTeiid.TRANSLATORS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return new Translator[0];
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.TRANSLATORS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        final List<Translator> result = new ArrayList<Translator>();
        for (final KomodoObject kobject : folderNode.getChildrenOfType(transaction, VdbLexicon.Translator.TRANSLATOR, namePatterns)) {
            Translator translator = new TranslatorImpl(transaction, getRepository(), kobject.getAbsolutePath());
            result.add(translator);
        }

        if (result.isEmpty()) {
            return new Translator[0];
        }

        return result.toArray(new Translator[result.size()]);
    }

    @Override
    public Datasource[] getDataSources(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if(!super.hasChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return new Datasource[0];
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        final List<Datasource> result = new ArrayList<Datasource>();
        for (final KomodoObject kobject : folderNode.getChildrenOfType(transaction, DataVirtLexicon.Connection.NODE_TYPE, namePatterns)) {
            Datasource dataSource = new DatasourceImpl(transaction, getRepository(), kobject.getAbsolutePath());
            result.add(dataSource);
        }

        if (result.isEmpty()) {
            return new Datasource[0];
        }

        return result.toArray(new Datasource[result.size()]);
    }

    @Override
    public Datasource getDataSource(UnitOfWork transaction, String name) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if(!super.hasChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return null;
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        if (folderNode.hasChild(transaction, name, DataVirtLexicon.Connection.NODE_TYPE)) {
            KomodoObject kobject = folderNode.getChild(transaction, name, DataVirtLexicon.Connection.NODE_TYPE);
            return new DatasourceImpl(transaction, getRepository(), kobject.getAbsolutePath());
        }

        return null;
    }

    @Override
    public Driver[] getDrivers(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if(!super.hasChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return new Driver[0];
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        final List<Driver> result = new ArrayList<Driver>();
        for (final KomodoObject kobject : folderNode.getChildrenOfType(transaction, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE)) {
            Driver driver = new DriverImpl(transaction, getRepository(), kobject.getAbsolutePath());
            result.add(driver);
        }

        if (result.isEmpty()) {
            return new Driver[0];
        }

        return result.toArray(new Driver[result.size()]);
    }

    @Override
    public Driver getDriver(UnitOfWork transaction, String name) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if(!super.hasChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return null;
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        if (folderNode.hasChild(transaction, name, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE)) {
            KomodoObject kobject = folderNode.getChild(transaction, name, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
            return new DriverImpl(transaction, getRepository(), kobject.getAbsolutePath());
        }

        return null;
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.teiid.CachedTeiid#refreshVdbs(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.runtime.TeiidInstance, java.lang.String[])
     */
    @Override
    public void refreshVdbs(UnitOfWork transaction,
                            TeiidInstance teiidInstance,
                            String... vdbNames) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isNotNull(teiidInstance, "teiidInstance"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isTrue(RepositoryImpl.isSystemTx(transaction), "transaction should be owned by " + Repository.SYSTEM_USER);

        try {
            teiidInstance.reconnect();
            if (! teiidInstance.isConnected()) {
                throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_CONNECTION_ERROR));
            }
        } catch (Exception ex) {
            throw new KException(ex);
        }
        
        if(!super.hasChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return;
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.VDBS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        // No names supplied, remove all from the cache then refresh all
        if( vdbNames==null || vdbNames.length==0 ) {
            KomodoObject[] kobjs = folderNode.getChildren(transaction);
            for(KomodoObject kobj : kobjs) {
                kobj.remove(transaction);
            }

            // Update with the Server VDBS
            Collection<TeiidVdb> teiidVdbs;
            try {
                teiidVdbs = teiidInstance.getVdbs();
            } catch (Exception ex) {
                throw new KException(Messages.getString(Messages.CachedTeiid.GET_SERVER_VDBS_ERROR));
            }
            for(TeiidVdb teiidVdb : teiidVdbs) {
                updateVdb(transaction, folderNode, teiidVdb);
            }
        }
        
        // Names supplied, update only the specified vdbs.
        for(String vdbName : vdbNames) {
            TeiidVdb teiidVdb;
            try {
                teiidVdb = teiidInstance.getVdb(vdbName);
            } catch (Exception ex) {
                throw new KException(Messages.getString(Messages.CachedTeiid.GET_SERVER_VDB_ERROR, vdbName));
            }
            // No server vdb found, remove the cached vdb
            if(teiidVdb==null) {
                if(folderNode.hasChild(transaction, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
                    KomodoObject existingObj = folderNode.getChild(transaction, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
                    existingObj.remove(transaction);
                }
            // Update the cached vdb
            } else {
                updateVdb(transaction, folderNode, teiidVdb);
            }
        }
    }

    // ######################################
    //
    // Taken from Teiid's AdminFactory$Admin so mirrors
    // the teiid mechanism to remove the java context from
    // a JNDI identifier.
    //
    // ######################################

    private static final String JAVA_CONTEXT = "java:/";

    private String removeJavaContext(String deployedName) {
        if (deployedName.startsWith(JAVA_CONTEXT)) {
            deployedName = deployedName.substring(6);
        }
        return deployedName;
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.teiid.CachedTeiid#refreshDataSources(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.runtime.TeiidInstance, java.lang.String[])
     */
    @Override
    public void refreshDataSources(UnitOfWork transaction,
                                   TeiidInstance teiidInstance,
                                   String... dataSourceNames) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isTrue(RepositoryImpl.isSystemTx(transaction), "transaction should be owned by " + Repository.SYSTEM_USER);

        try {
            teiidInstance.reconnect();
            if (! teiidInstance.isConnected()) {
                throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_CONNECTION_ERROR));
            }
        } catch (Exception ex) {
            throw new KException(ex);
        }
        
        if(!super.hasChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return;
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.DATA_SOURCES_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        // No names supplied, remove all from the cache then refresh all
        if( dataSourceNames==null || dataSourceNames.length==0 ) {
            KomodoObject[] kobjs = folderNode.getChildren(transaction);
            for(KomodoObject kobj : kobjs) {
                kobj.remove(transaction);
            }

            // Update with the Server DataSources
            Collection<TeiidDataSource> teiidDataSources;
            try {
                teiidDataSources = teiidInstance.getDataSources();
            } catch (Exception ex) {
                throw new KException(Messages.getString(Messages.CachedTeiid.GET_SERVER_DATA_SOURCES_ERROR));
            }
            for(TeiidDataSource teiidDataSource : teiidDataSources) {
                updateDataSource(transaction, folderNode, teiidDataSource);
            }
        }
        
        // Names supplied, update only the specified DataSources.
        for(String dataSourceName : dataSourceNames) {
            dataSourceName = removeJavaContext(dataSourceName);

            TeiidDataSource teiidDataSource;
            try {
                teiidDataSource = teiidInstance.getDataSource(dataSourceName);
            } catch (Exception ex) {
                throw new KException(Messages.getString(Messages.CachedTeiid.GET_SERVER_DATA_SOURCES_ERROR,dataSourceName));
            }
            // No server datasource found, remove the cached datasource
            if(teiidDataSource==null) {
                if(folderNode.hasChild(transaction, dataSourceName, DataVirtLexicon.Connection.NODE_TYPE)) {
                    KomodoObject existingObj = folderNode.getChild(transaction, dataSourceName, DataVirtLexicon.Connection.NODE_TYPE);
                    existingObj.remove(transaction);
                }
            // Update the cached source
            } else { 
                updateDataSource(transaction, folderNode, teiidDataSource);
            }
        }
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.teiid.CachedTeiid#refreshTranslators(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.runtime.TeiidInstance, java.lang.String[])
     */
    @Override
    public void refreshTranslators(UnitOfWork transaction,
                                   TeiidInstance teiidInstance,
                                   String... translatorNames) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isTrue(RepositoryImpl.isSystemTx(transaction), "transaction should be owned by " + Repository.SYSTEM_USER);

        try {
            teiidInstance.reconnect();
            if (! teiidInstance.isConnected()) {
                throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_CONNECTION_ERROR));
            }
        } catch (Exception ex) {
            throw new KException(ex);
        }
        
        if(!super.hasChild(transaction, CachedTeiid.TRANSLATORS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return;
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.TRANSLATORS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        // No names supplied, remove all from the cache then refresh all
        if( translatorNames==null || translatorNames.length==0 ) {
            KomodoObject[] kobjs = folderNode.getChildren(transaction);
            for(KomodoObject kobj : kobjs) {
                kobj.remove(transaction);
            }

            // Update with the Server Translators
            Collection<TeiidTranslator> teiidTranslators;
            try {
                teiidTranslators = teiidInstance.getTranslators();
            } catch (Exception ex) {
                throw new KException(Messages.getString(Messages.CachedTeiid.GET_SERVER_TRANSLATORS_ERROR));
            }
            for(TeiidTranslator teiidTranslator : teiidTranslators) {
                updateTranslator(transaction, folderNode, teiidTranslator);
            }
        }
        
        // Names supplied, update only the specified DataSources.
        for(String translatorName : translatorNames) {
            TeiidTranslator teiidTranslator;
            try {
                teiidTranslator = teiidInstance.getTranslator(translatorName);
            } catch (Exception ex) {
                throw new KException(Messages.getString(Messages.CachedTeiid.GET_SERVER_TRANSLATOR_ERROR,translatorName));
            }
            // No server translator found, remove the cached translator
            if(teiidTranslator==null) {
                if(folderNode.hasChild(transaction, translatorName, VdbLexicon.Translator.TRANSLATOR)) {
                    KomodoObject existingObj = folderNode.getChild(transaction, translatorName, VdbLexicon.Translator.TRANSLATOR);
                    existingObj.remove(transaction);
                }
            // Update the cached translator
            } else {
                updateTranslator(transaction, folderNode, teiidTranslator);
            }
        }
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.teiid.CachedTeiid#refreshDrivers(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.runtime.TeiidInstance, java.lang.String[])
     */
    @Override
    public void refreshDrivers(UnitOfWork transaction,
                               TeiidInstance teiidInstance,
                               String... driverNames) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isTrue(RepositoryImpl.isSystemTx(transaction), "transaction should be owned by " + Repository.SYSTEM_USER);

        try {
            teiidInstance.reconnect();
            if (! teiidInstance.isConnected()) {
                throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_CONNECTION_ERROR));
            }
        } catch (Exception ex) {
            throw new KException(ex);
        }
        
        if(!super.hasChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE)) {
            return;
        }
        KomodoObject folderNode = super.getChild(transaction, CachedTeiid.DRIVERS_FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        Set<String> dsTypeNames;
        try {
            dsTypeNames = teiidInstance.getDataSourceTypeNames();
        } catch (Exception ex) {
            throw new KException(Messages.getString(Messages.CachedTeiid.GET_SERVER_DRIVERS_ERROR));
        }
        
        // No names supplied, remove all from the cache then refresh all
        if( driverNames==null || driverNames.length==0 ) {
            KomodoObject[] kobjs = folderNode.getChildren(transaction);
            for(KomodoObject kobj : kobjs) {
                kobj.remove(transaction);
            }

            for(String dsTypeName : dsTypeNames) {
                updateDriver(transaction, folderNode, dsTypeName);
            }
        }
        
        // Names supplied, update only the specified Drivers.
        for(String driverName : driverNames) {
            // No server driver found, remove the cached driver
            if(!dsTypeNames.contains(driverName)) {
                if(folderNode.hasChild(transaction, driverName, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE)) {
                    KomodoObject existingObj = folderNode.getChild(transaction, driverName, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
                    existingObj.remove(transaction);
                }
            // Update the cached driver
            } else {
                updateDriver(transaction, folderNode, driverName);
            }
        }
    }

    /*
     * Update cached VDB with the supplied TeiidVdb.
     */
    private void updateVdb(UnitOfWork transaction, KomodoObject vdbsFolder, TeiidVdb teiidVdb) throws KException {
        String vdbName = teiidVdb.getName();
        
        // Export the vdb content into a string
        String content = null;
        // Output the content to a temp file
        File tempFile = null;
        try {
            content = teiidVdb.export();
            if (content == null || StringUtils.isEmpty(content)) return;

            tempFile = File.createTempFile(VDB_PREFIX, XML_SUFFIX);
            Files.write(Paths.get(tempFile.getPath()), content.getBytes());
        } catch (Exception ex) {
            throw new KException(ex);
        }

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
     * Update cached DataSource with the supplied TeiidDataSource.
     */
    private void updateDataSource(UnitOfWork transaction, KomodoObject dataSourcesFolder, TeiidDataSource teiidDS) throws KException {
        String dataSourceName = teiidDS.getName();
        
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
     * Update cached Translator with the supplied TeiidTranslator.
     */
    private void updateTranslator(UnitOfWork transaction, KomodoObject translatorsFolder, TeiidTranslator teiidTranslator) throws KException {
        String translatorName = teiidTranslator.getName();
        
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
     * Update cached Driver with the supplied Driver name.
     */
    private void updateDriver(UnitOfWork transaction, KomodoObject driversFolder, String driverName) throws KException {
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

}
