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
package org.komodo.relational.datasource.internal;

import java.util.Collection;
import java.util.Properties;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.ExcludeQNamesFilter;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;

/**
 * Implementation of datasource instance model
 */
public class DatasourceImpl extends RelationalChildRestrictedObject implements Datasource, EventManager {

    /**
     * A filter to exclude specific, readonly properties.
     */
    private static final Filter PROPS_FILTER = new ExcludeQNamesFilter( KomodoLexicon.DataSource.CLASS_NAME );
    
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
    public DatasourceImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
        
        // Update filters based on JDBC
        updatePropFilters(uow);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Datasource.IDENTIFIER;
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
     * @see org.komodo.relational.datasource.Datasource#getJndiName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJndiName( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getJndiName", //$NON-NLS-1$
                                  KomodoLexicon.DataSource.JNDI_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getDescription", //$NON-NLS-1$
                                  KomodoLexicon.LibraryComponent.DESCRIPTION);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#getExternalLocation(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getExternalLocation( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getExternalLocation", //$NON-NLS-1$
                                  KomodoLexicon.WorkspaceItem.EXT_LOC);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#getDriverName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDriverName( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getDriverName", //$NON-NLS-1$
                                  KomodoLexicon.DataSource.DRIVER_NAME);
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#getClassName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getClassName(UnitOfWork uow) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getClassName", //$NON-NLS-1$
                                  KomodoLexicon.DataSource.CLASS_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#getProfileName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getProfileName(UnitOfWork uow) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getProfileName", //$NON-NLS-1$
                                  KomodoLexicon.DataSource.PROFILE_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#isJdbc(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isJdbc(UnitOfWork uow) throws KException {
       Boolean isJdbc = getObjectProperty(uow, PropertyValueType.BOOLEAN, "isJdbc", KomodoLexicon.DataSource.JDBC); //$NON-NLS-1$
       if ( isJdbc == null ) {
           return Datasource.DEFAULT_JDBC;
       }

       return isJdbc;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#isPreview(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isPreview(UnitOfWork uow) throws KException {
       Boolean isPreview = getObjectProperty(uow, PropertyValueType.BOOLEAN, "isPreview", KomodoLexicon.DataSource.PREVIEW); //$NON-NLS-1$
       if( isPreview == null ) {
           return Datasource.DEFAULT_PREVIEW;
       }
       return isPreview;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setDriverName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDriverName( final UnitOfWork uow,
                               final String driverName ) throws KException {
        setObjectProperty( uow, "setDriverName", KomodoLexicon.DataSource.DRIVER_NAME, driverName ); //$NON-NLS-1$
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setJndiName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setJndiName( final UnitOfWork uow,
                             final String jndiName ) throws KException {
        setObjectProperty( uow, "setJndiName", KomodoLexicon.DataSource.JNDI_NAME, jndiName ); //$NON-NLS-1$
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String description ) throws KException {
        setObjectProperty( uow, "setDescription", KomodoLexicon.LibraryComponent.DESCRIPTION, description ); //$NON-NLS-1$
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setExternalLocation(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setExternalLocation( final UnitOfWork uow,
                                     final String extLoc ) throws KException {
        setObjectProperty( uow, "setExternalLocation", KomodoLexicon.WorkspaceItem.EXT_LOC, extLoc ); //$NON-NLS-1$
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setClassName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setClassName(UnitOfWork uow,
                             String className) throws KException {
        setObjectProperty( uow, "setClassName", KomodoLexicon.DataSource.CLASS_NAME, className ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setProfileName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setProfileName(UnitOfWork uow,
                               String profileName) throws KException {
        setObjectProperty( uow, "setProfileName", KomodoLexicon.DataSource.PROFILE_NAME, profileName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setJdbc(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setJdbc(UnitOfWork uow,
                        boolean isJdbc) throws KException {
        setObjectProperty( uow, "setJdbc", KomodoLexicon.DataSource.JDBC, isJdbc ); //$NON-NLS-1$
        updatePropFilters(uow);
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setPreview(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setPreview(UnitOfWork uow,
                           boolean isPreview) throws KException {
        setObjectProperty( uow, "isPreview", KomodoLexicon.DataSource.PREVIEW, isPreview ); //$NON-NLS-1$
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
     * @see org.komodo.relational.datasource.Datasource#getPropertiesForServerDeployment(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.runtime.TeiidInstance)
     */
    @Override
    public Properties getPropertiesForServerDeployment(UnitOfWork transaction,
                                                       TeiidInstance teiidInstance) throws Exception {
        Properties sourceProps = new Properties();
        
        // Get the Property Defns for this type of source.
        Collection<TeiidPropertyDefinition> templatePropDefns = teiidInstance.getTemplatePropertyDefns(getDriverName(transaction));

        // Datasource driverName and jndiName must be defined.
        String driverName = getDriverName(transaction);
        if(StringUtils.isBlank(driverName)) {
            throw new Exception( Messages.getString( Relational.DATASOURCE_DRIVERNAME_NOT_DEFINED ) );
        }
        sourceProps.setProperty(TeiidInstance.DATASOURCE_DRIVERNAME,driverName);
        String jndiName = getJndiName(transaction);
        if(StringUtils.isBlank(jndiName)) {
            throw new Exception( Messages.getString( Relational.DATASOURCE_JNDINAME_NOT_DEFINED ) );
        }
        sourceProps.setProperty(TeiidInstance.DATASOURCE_JNDINAME, jndiName);
        
        // Non-jdbc className is needed.
        if(!isJdbc(transaction)) {
            String className = getClassName(transaction);
            if(StringUtils.isBlank(className)) {
                throw new Exception( Messages.getString( Relational.DATASOURCE_CLASSNAME_NOT_DEFINED ) );
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
        DatasourceNodeVisitor visitor = new DatasourceNodeVisitor(transaction, this, exportProperties);
        String xmlResult = visitor.getXml();

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("DataSourceImpl#export: transaction = {0}, xml = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         xmlResult);
        }
        
        return xmlResult.getBytes();
    }

    @Override
    public DocumentType getDocumentType(UnitOfWork transaction) throws KException {
        return DocumentType.TDS;
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
        
        if( propertyName.equals(KomodoLexicon.DataSource.CLASS_NAME.substring(nsPrefixLength)) ) {
            setClassName(transaction, (String)values[0]);
        } else if ( propertyName.equals(KomodoLexicon.DataSource.DRIVER_NAME.substring(nsPrefixLength)) ) {
            setDriverName(transaction, (String)values[0]);
        } else if ( propertyName.equals(KomodoLexicon.DataSource.JNDI_NAME.substring(nsPrefixLength)) ) {
            setJndiName(transaction, (String)values[0]);
        } else if ( propertyName.equals(KomodoLexicon.DataSource.PREVIEW.substring(nsPrefixLength)) ) {
            boolean isPreview = false;
            if( (String)values[0] != null ) {
                isPreview = Boolean.parseBoolean((String)values[0]);
            }
            setPreview(transaction, isPreview);
        } else if ( propertyName.equals(KomodoLexicon.DataSource.PROFILE_NAME.substring(nsPrefixLength)) ) {
            setProfileName(transaction, (String)values[0]);
        } else {
            super.setProperty(transaction, propertyName, values);
        }
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
