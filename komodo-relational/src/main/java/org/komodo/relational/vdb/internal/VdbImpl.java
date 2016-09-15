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
package org.komodo.relational.vdb.internal;

import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.modeshape.visitor.VdbNodeVisitor;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.repository.DescriptorImpl;
import org.komodo.repository.PropertyDescriptorImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyDescriptor.Type;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;

/**
 * An implementation of a virtual database manifest.
 */
public class VdbImpl extends RelationalObjectImpl implements Vdb {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { DataRole.IDENTIFIER, Entry.IDENTIFIER, Model.IDENTIFIER,
                                                                         Translator.IDENTIFIER, VdbImport.IDENTIFIER };

	/**
	 * Include the special properties into the primary type descriptor.
	 *
	 * @see SpecialProperty
	 */
    class PrimaryTypeDescriptor extends DescriptorImpl {

        private final Descriptor delegate;

        PrimaryTypeDescriptor( final Repository repository,
                               final Descriptor delegate ) {
            super( repository, delegate.getName() );
            this.delegate = delegate;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.repository.DescriptorImpl#getPropertyDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
         */
        @Override
        public PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException {
            final PropertyDescriptor[] propDescriptors = this.delegate.getPropertyDescriptors( transaction );
            final SpecialProperty[] specialProps = SpecialProperty.values();

            final PropertyDescriptor[] result = new PropertyDescriptor[ propDescriptors.length + specialProps.length ];
            System.arraycopy( propDescriptors, 0, result, 0, propDescriptors.length );

            int i = propDescriptors.length;

            for ( final SpecialProperty prop : specialProps ) {
                result[i++] = prop.getDescriptor();
            }

            return result;
        }

    }

    enum SpecialProperty {

        ALLOWED_LANGUAGES( "allowed-languages" ), //$NON-NLS-1$
        AUTHENTICATION_TYPE( "authentication-type" ), //$NON-NLS-1$
        GSS_PATTERN( "gss-pattern" ), //$NON-NLS-1$
        PASSWORD_PATTERN( "password-pattern" ), //$NON-NLS-1$
        QUERY_TIMEOUT( "query-timeout" ), //$NON-NLS-1$
        SECURITY_DOMAIN( "security-domain" ); //$NON-NLS-1$

        /**
         * @param teiidName
         *        the Teiid property name whose enumeration is being requested (can be empty)
         * @return the special property or <code>null</code> if not found
         */
        static SpecialProperty fromTeiidName( final String teiidName ) {
            for ( final SpecialProperty prop : values() ) {
                if ( prop.toTeiidName().equals( teiidName ) ) {
                    return prop;
                }
            }

            return null;
        }

        static String[] valuesAsTeiidNames() {
            final SpecialProperty[] values = values();
            final String[] result = new String[ values.length ];
            int i = 0;

            for ( final SpecialProperty prop : values ) {
                result[i++] = prop.toTeiidName();
            }

            return result;
        }

        private final String teiidName;

        private SpecialProperty( final String teiidName ) {
            this.teiidName = teiidName;
        }

        PropertyDescriptor getDescriptor() throws KException {
            final Type type = ( this == QUERY_TIMEOUT ) ? Type.LONG : Type.STRING;
            return new PropertyDescriptorImpl( false, true, false, toTeiidName(), type, null );
        }

        String toTeiidName() {
            return this.teiidName;
        }

    }

    private class VdbManifestImpl implements VdbManifest {

        private final String xml;

        VdbManifestImpl( final UnitOfWork transaction,
                         final VdbImpl vdb, final Properties exportProperties ) throws KException {
            final StringWriter writer = new StringWriter();

            try {
                final XMLOutputFactory xof = XMLOutputFactory.newInstance();
                final XMLStreamWriter xsw = xof.createXMLStreamWriter(writer);

                final VdbNodeVisitor visitor = new VdbNodeVisitor(TeiidVersionProvider.getInstance().getTeiidVersion(), xsw);
                if( exportProperties != null && !exportProperties.isEmpty() ) {
                	boolean useTabs = exportProperties.containsKey(ExportConstants.USE_TABS_PROP_KEY);
                	visitor.setShowTabs(useTabs);
                }
                visitor.visit(vdb.node(transaction));
            } catch (final Exception e) {
                throw new KException(e);
            }

            // Create an XML Document from the filled writer
            this.xml = writer.toString().trim();

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug("VdbImpl#VdbManifestImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                             transaction.getName(),
                             this.xml);
            }
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.vdb.Vdb.VdbManifest#asDocument()
         */
        @Override
        public Document asDocument() throws KException {
            return FileUtils.createDocument(this.xml);
        }

        @Override
        public String getName(UnitOfWork transaction) throws KException {
            return getVdbName(transaction);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
         */
        @Override
        public byte[] export( final UnitOfWork transaction, Properties properties) {
            return this.xml == null ? new byte[0] : this.xml.getBytes();
        }

        @Override
        public DocumentType getDocumentType(UnitOfWork transaction) throws KException {
            return DocumentType.VDB_XML;
        }
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public VdbImpl( final UnitOfWork uow,
                    final Repository repository,
                    final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Vdb.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addDataRole(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public DataRole addDataRole( final UnitOfWork transaction,
                                 final String dataRoleName ) throws KException {
        return RelationalModelFactory.createDataRole( transaction, getRepository(), this, dataRoleName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addEntry(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public Entry addEntry( final UnitOfWork transaction,
                           final String entryName,
                           final String entryPath ) throws KException {
        return RelationalModelFactory.createEntry( transaction, getRepository(), this, entryName, entryPath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public VdbImport addImport( final UnitOfWork transaction,
                                final String vdbName ) throws KException {
        return RelationalModelFactory.createVdbImport( transaction, getRepository(), this, vdbName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Model addModel( final UnitOfWork transaction,
                           final String modelName ) throws KException {
        return RelationalModelFactory.createModel( transaction, getRepository(), this, modelName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public Translator addTranslator( final UnitOfWork transaction,
                                     final String translatorName,
                                     final String translatorType ) throws KException {
        return RelationalModelFactory.createTranslator( transaction, getRepository(), this, translatorName, translatorType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#createManifest(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public VdbManifest createManifest( final UnitOfWork transaction,
                                       final Properties properties ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final VdbManifest result = new VdbManifestImpl( transaction, this, properties );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork transaction,
                          final Properties properties ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final byte[] result = createManifest( transaction, properties ).export( transaction, properties );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getAllowedLanguages(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getAllowedLanguages( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getAllowedLanguages", //$NON-NLS-1$
                                  SpecialProperty.ALLOWED_LANGUAGES.toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getAuthenticationType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getAuthenticationType( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getAuthenticationType", //$NON-NLS-1$
                                  SpecialProperty.AUTHENTICATION_TYPE.toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name ) throws KException {
        // check data roles
        KomodoObject[] kids = getDataRoles( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check entries
        kids = getEntries( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check models
        kids = getModels( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check translators
        kids = getTranslators( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check imports
        kids = getImports( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.DataRole.DATA_ROLE.equals( typeName ) ) {
            final KomodoObject[] dataRoles = getDataRoles( transaction, name );

            if ( dataRoles.length != 0 ) {
                return dataRoles[ 0 ];
            }
        } else if ( VdbLexicon.Entry.ENTRY.equals( typeName ) ) {
            final KomodoObject[] entries = getEntries( transaction, name );

            if ( entries.length != 0 ) {
                return entries[ 0 ];
            }
        } else if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( typeName ) ) {
            final KomodoObject[] imports = getImports( transaction, name );

            if ( imports.length != 0 ) {
                return imports[ 0 ];
            }
        } else if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( typeName ) ) {
            final KomodoObject[] models = getModels( transaction, name );

            if ( models.length != 0 ) {
                return models[ 0 ];
            }
        } else if ( VdbLexicon.Translator.TRANSLATOR.equals( typeName ) ) {
            final KomodoObject[] translators = getTranslators( transaction, name );

            if ( translators.length != 0 ) {
                return translators[ 0 ];
            }
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction,
                                       final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final DataRole[] dataRoles = getDataRoles( transaction, namePatterns );
        final Entry[] entries = getEntries( transaction, namePatterns );
        final VdbImport[] imports = getImports( transaction, namePatterns );
        final Model[] models = getModels( transaction, namePatterns );
        final Translator[] translators = getTranslators( transaction, namePatterns );

        final KomodoObject[] result = new KomodoObject[ dataRoles.length + entries.length + imports.length + models.length
                                                        + translators.length ];
        System.arraycopy( dataRoles, 0, result, 0, dataRoles.length );
        System.arraycopy( entries, 0, result, dataRoles.length, entries.length );
        System.arraycopy( imports, 0, result, dataRoles.length + entries.length, imports.length );
        System.arraycopy( models, 0, result, dataRoles.length + entries.length + imports.length, models.length );
        System.arraycopy( translators,
                          0,
                          result,
                          dataRoles.length + entries.length + imports.length + models.length,
                          translators.length );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( VdbLexicon.DataRole.DATA_ROLE.equals( type ) ) {
            result = getDataRoles( transaction, namePatterns );
        } else if ( VdbLexicon.Entry.ENTRY.equals( type ) ) {
            result = getEntries( transaction, namePatterns );
        } else if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( type ) ) {
            result = getImports( transaction, namePatterns );
        } else if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( type ) ) {
            result = getModels( transaction, namePatterns );
        } else if ( VdbLexicon.Translator.TRANSLATOR.equals( type ) ) {
            result = getTranslators( transaction, namePatterns );
        } else {
            result = super.getChildrenOfType(transaction, type, namePatterns);
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getConnectionType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getConnectionType( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getConnectionType", VdbLexicon.Vdb.CONNECTION_TYPE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getDataRoles(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public DataRole[] getDataRoles( final UnitOfWork transaction,
                                    final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getDataRolesGroupingNode( transaction );

        if ( grouping != null ) {
            final List< DataRole > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final DataRole dataRole = new DataRoleImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( dataRole );
            }

            return temp.toArray( new DataRole[ temp.size() ] );
        }

        return DataRole.NO_DATA_ROLES;
    }

    private KomodoObject getDataRolesGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.Vdb.DATA_ROLES );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getDescription", VdbLexicon.Vdb.DESCRIPTION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getEntries(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Entry[] getEntries( final UnitOfWork transaction,
                               final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getEntriesGroupingNode( transaction);

        if ( grouping != null ) {
            final List< Entry > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final Entry entry = new EntryImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( entry );
            }

            return temp.toArray( new Entry[ temp.size() ] );
        }

        return Entry.NO_ENTRIES;
    }

    private KomodoObject getEntriesGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.Vdb.ENTRIES );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getGssPattern(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getGssPattern( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getGssPattern", //$NON-NLS-1$
                                  SpecialProperty.GSS_PATTERN.toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getImports(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public VdbImport[] getImports( final UnitOfWork transaction,
                                   final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getImportsGroupingNode( transaction);

        if ( grouping != null ) {
            final List< VdbImport > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final VdbImport vdbImport = new VdbImportImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( vdbImport );
            }

            return temp.toArray( new VdbImport[ temp.size() ] );
        }

        return VdbImport.NO_IMPORTS;
    }

    private KomodoObject getImportsGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.Vdb.IMPORT_VDBS );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getModels(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Model[] getModels( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Model > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction,
                                                                    VdbLexicon.Vdb.DECLARATIVE_MODEL,
                                                                    namePatterns ) ) {
            final Model model = new ModelImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( model );
        }

        if ( result.isEmpty() ) {
            return Model.NO_MODELS;
        }

        return result.toArray( new Model[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOriginalFilePath( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getOriginalFilePath", VdbLexicon.Vdb.ORIGINAL_FILE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getPasswordPattern(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getPasswordPattern( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getPasswordPattern", //$NON-NLS-1$
                                  SpecialProperty.PASSWORD_PATTERN.toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getPropertyDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                                     final String propName ) throws KException {
        final SpecialProperty prop = SpecialProperty.fromTeiidName( propName );

        if ( prop == null ) {
            return super.getPropertyDescriptor( transaction, propName );
        }

        return prop.getDescriptor();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor getPrimaryType( final UnitOfWork transaction ) throws KException {
        return new PrimaryTypeDescriptor( getRepository(), super.getPrimaryType( transaction ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getQueryTimeout(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getQueryTimeout( final UnitOfWork transaction ) throws KException {
        final Integer value = getObjectProperty( transaction,
                                                 PropertyValueType.INTEGER,
                                                 "getQueryTimeout", //$NON-NLS-1$
                                                 SpecialProperty.QUERY_TIMEOUT.toTeiidName() );
        return ( value == null ) ? -1 : value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getSecurityDomain(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getSecurityDomain( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getSecurityDomain", //$NON-NLS-1$
                                  SpecialProperty.SECURITY_DOMAIN.toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getTranslators(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Translator[] getTranslators( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getTranslatorsGroupingNode( transaction );

        if ( grouping != null ) {
            final List< Translator > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final Translator translator = new TranslatorImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( translator );
            }

            return temp.toArray( new Translator[ temp.size() ] );
        }

        return Translator.NO_TRANSLATORS;
    }

    private KomodoObject getTranslatorsGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.Vdb.TRANSLATORS );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
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
     * @see org.komodo.relational.vdb.Vdb#getVdbName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getVdbName( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getVdbName", VdbLexicon.Vdb.NAME); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.INTEGER, "getVersion", VdbLexicon.Vdb.VERSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        return ( ( getDataRoles( transaction, name ).length != 0 )
                 || ( getEntries( transaction, name ).length != 0 )
                 || ( getImports( transaction, name ).length != 0 )
                 || ( getModels( transaction, name ).length != 0 )
                 || ( getTranslators( transaction, name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.DataRole.DATA_ROLE.equals( typeName ) ) {
            return ( getDataRoles( transaction, name ).length != 0 );
        }

        if ( VdbLexicon.Entry.ENTRY.equals( typeName ) ) {
            return ( getEntries( transaction, name ).length != 0 );
        }

        if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( typeName ) ) {
            return ( getImports( transaction, name ).length != 0 );
        }

        if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( typeName ) ) {
            return ( getModels( transaction, name ).length != 0 );
        }

        if ( VdbLexicon.Translator.TRANSLATOR.equals( typeName ) ) {
            return ( getTranslators( transaction, name ).length != 0 );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork transaction ) throws KException {
        if ( super.hasChildren( transaction ) ) {
            return ( ( getDataRoles( transaction ).length != 0 )
            || ( getEntries( transaction ).length != 0 )
            || ( getImports( transaction ).length != 0 )
            || ( getModels( transaction ).length != 0 )
            || ( getTranslators( transaction ).length != 0 ) );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#isPreview(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isPreview( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isPreview", VdbLexicon.Vdb.PREVIEW); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeDataRole(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeDataRole( final UnitOfWork transaction,
                                final String dataRoleToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( dataRoleToRemove, "dataRoleToRemove" ); //$NON-NLS-1$

        final DataRole[] dataRoles = getDataRoles( transaction, dataRoleToRemove );

        if ( dataRoles.length == 0 ) {
            throw new KException( Messages.getString( Relational.DATA_ROLE_NOT_FOUND_TO_REMOVE, dataRoleToRemove ) );
        }

        // remove first occurrence
        dataRoles[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeEntry(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeEntry( final UnitOfWork transaction,
                             final String entryToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( entryToRemove, "entryToRemove" ); //$NON-NLS-1$

        final Entry[] entries = getEntries( transaction, entryToRemove );

        if ( entries.length == 0 ) {
            throw new KException( Messages.getString( Relational.ENTRY_NOT_FOUND_TO_REMOVE, entryToRemove ) );
        }

        // remove first occurrence
        entries[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeImport( final UnitOfWork transaction,
                              final String importToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( importToRemove, "importToRemove" ); //$NON-NLS-1$

        final VdbImport[] vdbImports = getImports( transaction, importToRemove );

        if ( vdbImports.length == 0 ) {
            throw new KException( Messages.getString( Relational.VDB_IMPORT_NOT_FOUND_TO_REMOVE, importToRemove ) );
        }

        // remove first occurrence
        vdbImports[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeModel( final UnitOfWork transaction,
                             final String modelToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( modelToRemove, "modelToRemove" ); //$NON-NLS-1$

        final Model[] models = getModels( transaction, modelToRemove );

        if ( models.length == 0 ) {
            throw new KException( Messages.getString( Relational.MODEL_NOT_FOUND_TO_REMOVE, modelToRemove ) );
        }

        // remove first occurrence
        models[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTranslator( final UnitOfWork transaction,
                                  final String translatorToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( translatorToRemove, "translatorToRemove" ); //$NON-NLS-1$

        final Translator[] translators = getTranslators( transaction, translatorToRemove );

        if ( translators.length == 0 ) {
            throw new KException( Messages.getString( Relational.TRANSLATOR_NOT_FOUND_TO_REMOVE, translatorToRemove ) );
        }

        // remove first occurrence
        translators[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void rename( final UnitOfWork transaction,
                        final String newName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( newName, "newName" ); //$NON-NLS-1$

        super.rename( transaction, newName );
        String shortName = newName;
        if(newName.contains(StringConstants.FORWARD_SLASH)) {
            shortName = newName.substring(shortName.lastIndexOf(StringConstants.FORWARD_SLASH)+1);
        }
        setVdbName( transaction, shortName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setAllowedLanguages(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setAllowedLanguages( final UnitOfWork transaction,
                                     final String newAllowedLanguages ) throws KException {
        setObjectProperty( transaction,
                           "setAllowedLanguages", //$NON-NLS-1$
                           SpecialProperty.ALLOWED_LANGUAGES.toTeiidName(),
                           newAllowedLanguages );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setAuthenticationType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setAuthenticationType( final UnitOfWork transaction,
                                       final String newAuthenticationType ) throws KException {
        setObjectProperty( transaction,
                           "setAuthenticationType", //$NON-NLS-1$
                           SpecialProperty.AUTHENTICATION_TYPE.toTeiidName(),
                           newAuthenticationType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setConnectionType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setConnectionType( final UnitOfWork uow,
                                   final String newConnectionType ) throws KException {
        setObjectProperty(uow, "setConnectionType", VdbLexicon.Vdb.CONNECTION_TYPE, newConnectionType); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String newDescription ) throws KException {
        setObjectProperty(uow, "setDescription", VdbLexicon.Vdb.DESCRIPTION, newDescription); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setGssPattern(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setGssPattern( final UnitOfWork transaction,
                               final String newGssPattern ) throws KException {
        setObjectProperty( transaction, "setGssPattern", SpecialProperty.GSS_PATTERN.toTeiidName(), newGssPattern ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setOriginalFilePath( final UnitOfWork uow,
                                     final String newOriginalFilePath ) throws KException {
        ArgCheck.isNotEmpty(newOriginalFilePath, "newOriginalFilePath"); //$NON-NLS-1$
        setObjectProperty(uow, "setOriginalFilePath", VdbLexicon.Vdb.ORIGINAL_FILE, newOriginalFilePath); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setPasswordPattern(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setPasswordPattern( final UnitOfWork transaction,
                                    final String newPasswordPattern ) throws KException {
        setObjectProperty( transaction, "setPasswordPattern", SpecialProperty.PASSWORD_PATTERN.toTeiidName(), newPasswordPattern ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setPreview(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setPreview( final UnitOfWork uow,
                            final boolean newPreview ) throws KException {
        setObjectProperty(uow, "setPreview", VdbLexicon.Vdb.PREVIEW, newPreview); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setQueryTimeout(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setQueryTimeout( final UnitOfWork transaction,
                                 final int newQueryTimeout ) throws KException {
        // setting new value to null if timeout is less than zero to delete property
        Object newValue = null;

        if ( newQueryTimeout > -1 ) {
            newValue = newQueryTimeout;
        }

        setObjectProperty( transaction, "setQueryTimeout", SpecialProperty.QUERY_TIMEOUT.toTeiidName(), newValue ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setSecurityDomain(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setSecurityDomain( final UnitOfWork transaction,
                                   final String newSecurityDomain ) throws KException {
        setObjectProperty( transaction, "setSecurityDomain", SpecialProperty.SECURITY_DOMAIN.toTeiidName(), newSecurityDomain ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setVdbName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setVdbName( final UnitOfWork uow,
                            final String newVdbName ) throws KException {
        setObjectProperty(uow, "setVdbName", VdbLexicon.Vdb.NAME, newVdbName); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setVersion(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setVersion( final UnitOfWork uow,
                            final int newVersion ) throws KException {
        setObjectProperty(uow, "setVersion", VdbLexicon.Vdb.VERSION, newVersion); //$NON-NLS-1$
    }

    @Override
    public DocumentType getDocumentType(UnitOfWork transaction) throws KException {
        return DocumentType.VDB_XML;
    }
    
    @Override
    public DeployStatus deploy(UnitOfWork uow, Teiid teiid) {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(teiid, "teiid"); //$NON-NLS-1$

        DeployStatus status = new DeployStatus();
        TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);
        
        try {
            String vdbName = getName(uow);
            status.addProgressMessage("Starting deployment of vdb " + vdbName); //$NON-NLS-1$

            status.addProgressMessage("Attempting to deploy VDB " + vdbName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$

            // Get VDB content
            byte[] vdbXml = export(uow, null);
            if (vdbXml == null || vdbXml.length == 0) {
                status.addErrorMessage("VDB " + vdbName + " content is empty"); //$NON-NLS-1$ //$NON-NLS-2$
                return status;
            }

            String vdbToDeployName = getName(uow);
            String vdbDeploymentName = vdbToDeployName + VDB_DEPLOYMENT_SUFFIX;
            teiidInstance.deployDynamicVdb(vdbDeploymentName, new ByteArrayInputStream(vdbXml));

            status.addProgressMessage("VDB deployed " + vdbName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$
        } catch (Exception ex) {
            status.addErrorMessage(ex);
        }

        
        return status;
    }
    
}
