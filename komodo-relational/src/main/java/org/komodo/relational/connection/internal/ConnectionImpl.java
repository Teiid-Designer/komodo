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
package org.komodo.relational.connection.internal;

import java.util.Collection;
import java.util.Properties;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.ExcludeQNamesFilter;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * Implementation of connection instance model
 */
public class ConnectionImpl extends RelationalObjectImpl implements Connection, EventManager {

    /**
     * A filter to exclude specific, readonly properties.
     */
    private static final Filter PROPS_FILTER = new ExcludeQNamesFilter( DataVirtLexicon.Connection.CLASS_NAME );

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
    public ConnectionImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);

        // Update filters based on JDBC
        updatePropFilters(uow);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Connection.IDENTIFIER;
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
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#getJndiName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJndiName( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getJndiName", //$NON-NLS-1$
                                  DataVirtLexicon.Connection.JNDI_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getDescription", //$NON-NLS-1$
                                  KomodoLexicon.LibraryComponent.DESCRIPTION);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#getExternalLocation(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getExternalLocation( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getExternalLocation", //$NON-NLS-1$
                                  KomodoLexicon.WorkspaceItem.EXT_LOC);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#getDriverName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDriverName( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getDriverName", //$NON-NLS-1$
                                  DataVirtLexicon.Connection.DRIVER_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#getClassName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getClassName(UnitOfWork uow) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getClassName", //$NON-NLS-1$
                                  DataVirtLexicon.Connection.CLASS_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#isJdbc(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isJdbc(UnitOfWork uow) throws KException {
        final String connectionType = getObjectProperty(uow,
                                                        PropertyValueType.STRING,
                                                        "isJdbc", //$NON-NLS-1$
                                                        DataVirtLexicon.Connection.TYPE);

        if ( connectionType == null ) {
            return Connection.DEFAULT_JDBC;
        }

        return ( org.teiid.modeshape.sequencer.dataservice.Connection.Type.JDBC.name().equals(connectionType) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#setDriverName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDriverName( final UnitOfWork uow,
                               final String driverName ) throws KException {
        setObjectProperty( uow, "setDriverName", DataVirtLexicon.Connection.DRIVER_NAME, driverName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#setJndiName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setJndiName( final UnitOfWork uow,
                             final String jndiName ) throws KException {
        setObjectProperty( uow, "setJndiName", DataVirtLexicon.Connection.JNDI_NAME, jndiName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String description ) throws KException {
        setObjectProperty( uow, "setDescription", KomodoLexicon.LibraryComponent.DESCRIPTION, description ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#setExternalLocation(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setExternalLocation( final UnitOfWork uow,
                                     final String extLoc ) throws KException {
        setObjectProperty( uow, "setExternalLocation", KomodoLexicon.WorkspaceItem.EXT_LOC, extLoc ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#setClassName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setClassName(UnitOfWork uow,
                             String className) throws KException {
        setObjectProperty( uow, "setClassName", DataVirtLexicon.Connection.CLASS_NAME, className ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.connection.Connection#setJdbc(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setJdbc(UnitOfWork uow,
                        boolean isJdbc) throws KException {
        final String connectionType = ( isJdbc ? org.teiid.modeshape.sequencer.dataservice.Connection.Type.JDBC.name() 
                                               : org.teiid.modeshape.sequencer.dataservice.Connection.Type.RESOURCE.name() );
        setObjectProperty( uow, "setJdbc", DataVirtLexicon.Connection.TYPE, connectionType ); //$NON-NLS-1$
        updatePropFilters(uow);
    }

    private void updatePropFilters(UnitOfWork uow) throws KException {
        // If JDBC, do not show the "class-name" property
        if(isJdbc(uow)) {
            // add in filter to hide the constraint type
            final Filter[] updatedFilters = new Filter[ DEFAULT_FILTERS.length + 1 ];
            System.arraycopy( DEFAULT_FILTERS, 0, updatedFilters, 0, DEFAULT_FILTERS.length );
            updatedFilters[ DEFAULT_FILTERS.length ] = PROPS_FILTER;
            setFilters( updatedFilters );
        } else {
            setFilters( DEFAULT_FILTERS );
        }
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.connection.Connection#getPropertiesForServerDeployment(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.runtime.TeiidInstance)
     */
    @Override
    public Properties getPropertiesForServerDeployment(UnitOfWork transaction,
                                                       TeiidInstance teiidInstance) throws Exception {
        Properties sourceProps = new Properties();

        // Get the Property Defns for this type of source.
        Collection<TeiidPropertyDefinition> templatePropDefns = teiidInstance.getTemplatePropertyDefns(getDriverName(transaction));

        // Connection driverName and jndiName must be defined.
        String driverName = getDriverName(transaction);
        if(StringUtils.isBlank(driverName)) {
            throw new Exception( Messages.getString( Relational.CONNECTION_DRIVERNAME_NOT_DEFINED ) );
        }
        sourceProps.setProperty(TeiidInstance.DATASOURCE_DRIVERNAME,driverName);
        String jndiName = getJndiName(transaction);
        if(StringUtils.isBlank(jndiName)) {
            throw new Exception( Messages.getString( Relational.CONNECTION_JNDINAME_NOT_DEFINED ) );
        }
        sourceProps.setProperty(TeiidInstance.DATASOURCE_JNDINAME, jndiName);

        // Non-jdbc className is needed.
        if(!isJdbc(transaction)) {
            String className = getClassName(transaction);
            if(StringUtils.isBlank(className)) {
                throw new Exception( Messages.getString( Relational.CONNECTION_CLASSNAME_NOT_DEFINED ) );
            }
            sourceProps.setProperty(TeiidInstance.DATASOURCE_CLASSNAME, className);
        }

        // Iterate the datasource properties.  Compare them against the valid properties for the server source type.
        String[] propNames = getPropertyNames(transaction);
        for(String propName : propNames) {
            TeiidPropertyDefinition propDefn = getTemplatePropertyDefn(templatePropDefns,propName);
            if(propDefn!=null) {
                boolean hasDefault = propDefn.getDefaultValue()!=null ? true : false;
                String sourcePropValue = getProperty(transaction, propName).getStringValue(transaction);
                // Template has no default - set the property
                if(!hasDefault) {
                    if(sourcePropValue!=null) {
                        sourceProps.setProperty(propName, sourcePropValue);
                    }
                    // Template has default - if source property matches it, no need to provide it.
                } else {
                    String templateDefaultValue = propDefn.getDefaultValue().toString();
                    if(!templateDefaultValue.equals(sourcePropValue)) {
                        if(sourcePropValue!=null) {
                            sourceProps.setProperty(propName, sourcePropValue);
                        }
                    }
                }
            }
        }

        return sourceProps;
    }

    private TeiidPropertyDefinition getTemplatePropertyDefn(Collection<TeiidPropertyDefinition> templatePropDefns, String propName) {
        TeiidPropertyDefinition propDefn = null;
        for(TeiidPropertyDefinition aDefn : templatePropDefns) {
            if(propName.equals(aDefn.getName())) {
                propDefn = aDefn;
                break;
            }
        }
        return propDefn;
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export(UnitOfWork transaction,
                         Properties exportProperties) throws KException {

        // Get the XML result
        ConnectionNodeVisitor visitor = new ConnectionNodeVisitor(transaction, this, exportProperties);
        String xmlResult = visitor.getXml();

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("ConnectionImpl#export: transaction = {0}, xml = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         xmlResult);
        }

        return xmlResult.getBytes();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#setProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.Object[])
     */
    @Override
    public void setProperty( final UnitOfWork transaction,
                             final String propertyName,
                             final Object... values ) throws KException {

        int nsPrefixLength = (KomodoLexicon.Namespace.PREFIX+StringConstants.COLON).length();

        if( propertyName.equals(DataVirtLexicon.Connection.CLASS_NAME.substring(nsPrefixLength)) ) {
            setClassName(transaction, (String)values[0]);
        } else if ( propertyName.equals(DataVirtLexicon.Connection.DRIVER_NAME.substring(nsPrefixLength)) ) {
            setDriverName(transaction, (String)values[0]);
        } else if ( propertyName.equals(DataVirtLexicon.Connection.JNDI_NAME.substring(nsPrefixLength)) ) {
            setJndiName(transaction, (String)values[0]);
        } else {
            super.setProperty(transaction, propertyName, values);
        }
    }

    @Override
    public DeployStatus deploy(UnitOfWork uow, Teiid teiid) {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(teiid, "teiid"); //$NON-NLS-1$

        DeployStatus status = new DeployStatus();
        TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);
        
        try {
            String connName = getName(uow);
            status.addProgressMessage("Starting deployment of connection " + connName); //$NON-NLS-1$

            String jndiName = getJndiName(uow);
            String sourceType = getDriverName(uow);
            Properties properties = getPropertiesForServerDeployment(uow, teiidInstance);

            status.addProgressMessage("Attempting to deploy connection " + connName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$

            TeiidDataSource teiidDataSrc = teiidInstance.getOrCreateDataSource(connName,
                                                                               jndiName,
                                                                               sourceType,
                                                                               properties);
            if (teiidDataSrc == null) {
                status.addErrorMessage("Connection " + connName + " failed to deploy"); //$NON-NLS-1$ //$NON-NLS-2$
                return status;
            }

            status.addProgressMessage("Data source deployed " + connName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$
        } catch (Exception ex) {
            status.addErrorMessage(ex);
        }

        return status;
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
