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

import org.komodo.core.KEngine;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.runtime.client.instance.TCTeiidInstance;

/**
 * Implementation of teiid instance model
 */
public class TeiidImpl extends RelationalObjectImpl implements Teiid, EventManager {

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
        public void setHostProvider(HostProvider hostProvider) {
            // host provider is provided by the TeiidImpl class
            // so this should do nothing
        }

        @Override
        public String getUrl(String vdbName) {
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
                return getJdbcPort(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_PORT;
            }
        }

        @Override
        public void setPort(int port) {
            try {
                setJdbcPort(null, port);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getUsername() {
            try {
                return getJdbcUsername(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_JDBC_USERNAME;
            }
        }

        @Override
        public void setUsername(String userName) {
            try {
                setJdbcUsername(null, userName);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getPassword() {
            try {
                return getJdbcPassword(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_JDBC_PASSWORD;
            }
        }

        @Override
        public void setPassword(String password) {
            try {
                setJdbcPassword(null, password);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public boolean isSecure() {
            try {
                return isJdbcSecure(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_SECURE;
            }
        }

        @Override
        public void setSecure(boolean secure) {
            try {
                setJdbcSecure(null, secure);
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
        public void setHostProvider(HostProvider hostProvider) {
            // Nothing to do since this is the host provider
        }

        @Override
        public int getPort() {
            try {
                return getAdminPort(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_PORT;
            }
        }

        @Override
        public void setPort(int port) {
            try {
                setAdminPort(null, port);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getUsername() {
            try {
                return getAdminUser(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_ADMIN_USERNAME;
            }
        }

        @Override
        public void setUsername(String userName) {
            try {
                setAdminUser(null, userName);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public String getPassword() {
            try {
                return getAdminPassword(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_ADMIN_PASSWORD;
            }
        }

        @Override
        public void setPassword(String password) {
            try {
                setAdminPassword(null, password);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        @Override
        public boolean isSecure() {
            try {
                return isAdminSecure(null);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
                return DEFAULT_SECURE;
            }
        }

        @Override
        public void setSecure(boolean secure) {
            try {
                setAdminSecure(null, secure);
            } catch (KException ex) {
                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }
    }

    private class TeiidParentImpl implements TeiidParent {

        private TeiidInstance teiidInstance;

        /**
         * Construct the parent and child teiid instance
         */
        public TeiidParentImpl() {
            new TCTeiidInstance(this, jdbcInfo);
        }

        @Override
        public TeiidInstance getTeiidInstance() {
            return teiidInstance;
        }

        @Override
        public void setTeiidInstance(TeiidInstance teiidInstance) {
            this.teiidInstance = teiidInstance;
        }

        @Override
        public String getHost() {
            try {
                return TeiidImpl.this.getHost(null);
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
                return TeiidImpl.this.getName(null);
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

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository the repository
     * @param path the path
     * @throws KException if error occurs
     */
    public TeiidImpl(final UnitOfWork uow,
                     final Repository repository,
                     final String path) throws KException {
        super(uow, repository, path);
        adminInfo = new TeiidAdminInfoImpl();
        jdbcInfo = new TeiidJdbcInfoImpl();
        teiidParent = new TeiidParentImpl();
    }

    @Override
    public TeiidInstance getTeiidInstance() {
        return teiidParent.getTeiidInstance();
    }

    /**
     * @param uow the transaction
     * @return value of teiid id property
     * @throws KException if error occurs
     */
    @Override
    public String getId(UnitOfWork uow) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getId", JcrLexicon.UUID.getString()); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid host property
     * @throws KException if error occurs
     */
    @Override
    public String getHost(UnitOfWork uow) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getHost", KomodoLexicon.Teiid.HOST); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid admin port property
     * @throws KException if error occurs
     */
    @Override
    public int getAdminPort(UnitOfWork uow) throws KException {
        Long port = getObjectProperty(uow, Property.ValueType.LONG, "getAdminPort", KomodoLexicon.Teiid.ADMIN_PORT); //$NON-NLS-1$
        return port != null ? port.intValue() : TeiidAdminInfo.DEFAULT_PORT;
    }

    /**
     * @param uow the transaction
     * @param port new value of admin port property
     * @throws KException if error occurs
     */
    @Override
    public void setAdminPort(UnitOfWork uow, int port) throws KException {
        setObjectProperty(uow, "setAdminPort", KomodoLexicon.Teiid.ADMIN_PORT, port); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid admin user property
     * @throws KException if error occurs
     */
    @Override
    public String getAdminUser(UnitOfWork uow) throws KException {
        String user = getObjectProperty(uow, Property.ValueType.STRING, "getAdminUser", KomodoLexicon.Teiid.ADMIN_USER); //$NON-NLS-1$
        return user != null ? user : TeiidAdminInfo.DEFAULT_ADMIN_USERNAME;
    }

    /**
     * @param uow the transaction
     * @param userName new value of admin username property
     * @throws KException if error occurs
     */
    @Override
    public void setAdminUser(UnitOfWork uow, String userName) throws KException {
        setObjectProperty(uow, "setAdminUser", KomodoLexicon.Teiid.ADMIN_USER, userName); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid admin password property
     * @throws KException if error occurs
     */
    @Override
    public String getAdminPassword(UnitOfWork uow) throws KException {
        String password = getObjectProperty(uow, Property.ValueType.STRING, "getAdminPassword", KomodoLexicon.Teiid.ADMIN_PSWD); //$NON-NLS-1$
        return password != null ? password : TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD;
    }

    /**
     * @param uow the transaction
     * @param password new value of admin password property
     * @throws KException if error occurs
     */
    @Override
    public void setAdminPassword(UnitOfWork uow, String password) throws KException {
        setObjectProperty(uow, "setAdminPassword", KomodoLexicon.Teiid.ADMIN_PSWD, password); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid secure property
     * @throws KException if error occurs
     */
    @Override
    public boolean isAdminSecure(UnitOfWork uow) throws KException {
        Boolean secure = getObjectProperty(uow, Property.ValueType.BOOLEAN, "isSecure", KomodoLexicon.Teiid.ADMIN_SECURE); //$NON-NLS-1$
        return secure != null ? secure : TeiidAdminInfo.DEFAULT_SECURE;
    }

    /**
     * @param uow the transaction
     * @param secure new value of admin secure property
     * @throws KException if error occurs
     */
    @Override
    public void setAdminSecure(UnitOfWork uow, boolean secure) throws KException {
        setObjectProperty(uow, "setAdminSecure", KomodoLexicon.Teiid.ADMIN_SECURE, secure); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid jdbc port property
     * @throws KException if error occurs
     */
    private int getJdbcPort(UnitOfWork uow) throws KException {
        Long port = getObjectProperty(uow, Property.ValueType.LONG, "getPort", KomodoLexicon.Teiid.JDBC_PORT); //$NON-NLS-1$
        return port != null ? port.intValue() : TeiidJdbcInfo.DEFAULT_PORT;
    }

    /**
     * @param uow the transaction
     * @param port new value of jdbc port property
     * @throws KException if error occurs
     */
    @Override
    public void setJdbcPort(UnitOfWork uow, int port) throws KException {
        setObjectProperty(uow, "setPort", KomodoLexicon.Teiid.JDBC_PORT, port); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid jdbc user property
     * @throws KException if error occurs
     */
    private String getJdbcUsername(UnitOfWork uow) throws KException {
        String user = getObjectProperty(uow, Property.ValueType.STRING, "getUsername", KomodoLexicon.Teiid.JDBC_USER); //$NON-NLS-1$
        return user != null ? user : TeiidJdbcInfo.DEFAULT_JDBC_USERNAME;
    }

    /**
     * @param uow the transaction
     * @param userName new value of jdbc username property
     * @throws KException if error occurs
     */
    @Override
    public void setJdbcUsername(UnitOfWork uow, String userName) throws KException {
        setObjectProperty(uow, "setUsername", KomodoLexicon.Teiid.JDBC_USER, userName); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid jdbc password property
     * @throws KException if error occurs
     */
    @Override
    public String getJdbcPassword(UnitOfWork uow) throws KException {
        String password = getObjectProperty(uow, Property.ValueType.STRING, "getPassword", KomodoLexicon.Teiid.JDBC_PSWD); //$NON-NLS-1$
        return password != null ? password : TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD;
    }

    /**
     * @param uow the transaction
     * @param password new value of jdbc password property
     * @throws KException if error occurs
     */
    @Override
    public void setJdbcPassword(UnitOfWork uow, String password) throws KException {
        setObjectProperty(uow, "setPassword", KomodoLexicon.Teiid.JDBC_PSWD, password); //$NON-NLS-1$
    }

    /**
     * @param uow the transaction
     * @return value of teiid secure property
     * @throws KException if error occurs
     */
    @Override
    public boolean isJdbcSecure(UnitOfWork uow) throws KException {
        Boolean secure = getObjectProperty(uow, Property.ValueType.BOOLEAN, "isSecure", KomodoLexicon.Teiid.JDBC_SECURE); //$NON-NLS-1$
        return secure != null ? secure : TeiidJdbcInfo.DEFAULT_SECURE;
    }

    /**
     * @param uow the transaction
     * @param secure new value of jdbc secure property
     * @throws KException if error occurs
     */
    @Override
    public void setJdbcSecure(UnitOfWork uow, boolean secure) throws KException {
        setObjectProperty(uow, "setSecure", KomodoLexicon.Teiid.JDBC_SECURE, secure); //$NON-NLS-1$
    }

    @Override
    public boolean addListener(ExecutionConfigurationListener listener) {
        return false;
    }

    @Override
    public void permitListeners(boolean enable) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public void notifyListeners(ExecutionConfigurationEvent event) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public boolean removeListener(ExecutionConfigurationListener listener) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#validateInitialState(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    protected void validateInitialState( final UnitOfWork uow,
                                         final String path ) throws KException {
        validateType(uow, path, KomodoLexicon.Teiid.NODE_TYPE);
    }

}
