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

import java.util.ArrayList;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoLexicon.TeiidArchetype;
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
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.ArgCheck;
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
     * @param transaction
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
        return timestamp != null ? timestamp : System.currentTimeMillis();
    }

    protected void setTimestamp(UnitOfWork uow, long timestamp) throws KException {
        setObjectProperty(uow, "setTimestamp", KomodoLexicon.CachedTeiid.TIMESTAMP, timestamp);
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

        final List<Vdb> result = new ArrayList<>();
        for (final KomodoObject kobject : super.getChildrenOfType(transaction, VdbLexicon.Vdb.VIRTUAL_DATABASE, namePatterns)) {
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

        if (hasChild(transaction, name, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
            KomodoObject kobject = getChild(transaction, name, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            return new VdbImpl(transaction, getRepository(), kobject.getAbsolutePath());
        }

        return null;
    }

    @Override
    public Translator[] getTranslators(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        final List<Translator> result = new ArrayList<>();
        for (final KomodoObject kobject : super.getChildrenOfType(transaction, VdbLexicon.Translator.TRANSLATOR, namePatterns)) {
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

        final List<Datasource> result = new ArrayList<>();
        for (final KomodoObject kobject : super.getChildrenOfType(transaction, DataVirtLexicon.Connection.NODE_TYPE, namePatterns)) {
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

        if (hasChild(transaction, name, DataVirtLexicon.Connection.NODE_TYPE)) {
            KomodoObject kobject = getChild(transaction, name, DataVirtLexicon.Connection.NODE_TYPE);
            return new DatasourceImpl(transaction, getRepository(), kobject.getAbsolutePath());
        }

        return null;
    }

    @Override
    public Driver[] getDrivers(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        final List<Driver> result = new ArrayList<Driver>();
        for (final KomodoObject kobject : super.getChildrenOfType(transaction, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE)) {
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

        if (hasChild(transaction, name, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE)) {
            KomodoObject kobject = getChild(transaction, name, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
            return new DriverImpl(transaction, getRepository(), kobject.getAbsolutePath());
        }

        return null;
    }
}
