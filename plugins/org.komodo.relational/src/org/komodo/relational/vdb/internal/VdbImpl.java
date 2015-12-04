/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import javax.jcr.RepositoryException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.modeshape.visitor.VdbNodeVisitor;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
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
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyDescriptor.Type;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * An implementation of a virtual database manifest.
 */
public final class VdbImpl extends RelationalObjectImpl implements Vdb {

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
            String xmlText = this.xml.replaceAll(NEW_LINE, SPACE);
            xmlText = xmlText.replaceAll(">[\\s]+<", CLOSE_ANGLE_BRACKET + OPEN_ANGLE_BRACKET); //$NON-NLS-1$
            xmlText = xmlText.replaceAll("[\\s]+", SPACE); //$NON-NLS-1$
            xmlText = xmlText.replaceAll("CDATA\\[[\\s]+", "CDATA["); //$NON-NLS-1$ //$NON-NLS-2$
            xmlText = xmlText.replaceAll("; \\]\\]", ";]]"); //$NON-NLS-1$ //$NON-NLS-2$

            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setIgnoringElementContentWhitespace(true);
            dbf.setIgnoringComments(true);

            try {
                final DocumentBuilder db = dbf.newDocumentBuilder();
                final Document doc = db.parse(new InputSource(new StringReader(xmlText)));
                doc.setXmlStandalone(true);
                doc.normalizeDocument();

                return doc;
            } catch (final Exception e) {
                throw new KException(e);
            }
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
         */
        @Override
        public String export( final UnitOfWork transaction, Properties properties) {
            return this.xml;
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
    public String export( final UnitOfWork transaction,
                          final Properties properties ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String result = createManifest( transaction, properties ).export( transaction, properties );
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
            result = KomodoObject.EMPTY_ARRAY;
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

        DataRole[] result = null;

        if ( hasChild( transaction, VdbLexicon.Vdb.DATA_ROLES, VdbLexicon.Vdb.DATA_ROLES ) ) {
            final KomodoObject grouping = getChild( transaction, VdbLexicon.Vdb.DATA_ROLES, VdbLexicon.Vdb.DATA_ROLES );
            final List< DataRole > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction,
                                                                           VdbLexicon.DataRole.DATA_ROLE,
                                                                           namePatterns ) ) {
                final DataRole dataRole = new DataRoleImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( dataRole );
            }

            result = temp.toArray( new DataRole[ temp.size() ] );
        } else {
            result = DataRole.NO_DATA_ROLES;
        }

        return result;
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

        Entry[] result = null;

        if ( hasChild( transaction, VdbLexicon.Vdb.ENTRIES, VdbLexicon.Vdb.ENTRIES ) ) {
            final KomodoObject grouping = getChild( transaction, VdbLexicon.Vdb.ENTRIES, VdbLexicon.Vdb.ENTRIES );
            final List< Entry > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction, VdbLexicon.Entry.ENTRY, namePatterns ) ) {
                final Entry entry = new EntryImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( entry );
            }

            result = temp.toArray( new Entry[ temp.size() ] );
        } else {
            result = Entry.NO_ENTRIES;
        }

        return result;
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

        VdbImport[] result = null;

        if ( hasChild( transaction, VdbLexicon.Vdb.IMPORT_VDBS, VdbLexicon.Vdb.IMPORT_VDBS ) ) {
            final KomodoObject grouping = getChild( transaction, VdbLexicon.Vdb.IMPORT_VDBS, VdbLexicon.Vdb.IMPORT_VDBS );
            final List< VdbImport > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction,
                                                                           VdbLexicon.ImportVdb.IMPORT_VDB,
                                                                           namePatterns ) ) {
                final VdbImport vdbImport = new VdbImportImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( vdbImport );
            }

            result = temp.toArray( new VdbImport[ temp.size() ] );
        } else {
            result = VdbImport.NO_IMPORTS;
        }

        return result;
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

        Translator[] result = null;

        if ( hasChild( transaction, VdbLexicon.Vdb.TRANSLATORS, VdbLexicon.Vdb.TRANSLATORS ) ) {
            final KomodoObject grouping = getChild( transaction, VdbLexicon.Vdb.TRANSLATORS, VdbLexicon.Vdb.TRANSLATORS );
            final List< Translator > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction,
                                                                           VdbLexicon.Translator.TRANSLATOR,
                                                                           namePatterns ) ) {
                final Translator translator = new TranslatorImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( translator );
            }

            result = temp.toArray( new Translator[ temp.size() ] );
        } else {
            result = Translator.NO_TRANSLATORS;
        }

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

        if ( VdbLexicon.Vdb.DATA_ROLES.equals( name ) ) {
            if ( !VdbLexicon.Vdb.DATA_ROLES.equals( typeName ) ) {
                throw new KException( Messages.getString( Relational.INVALID_GROUPING_NODE_TYPE,
                                                          VdbLexicon.Vdb.DATA_ROLES,
                                                          typeName ) );
            }

            try {
                return node( transaction ).hasNode( VdbLexicon.Vdb.DATA_ROLES );
            } catch ( final RepositoryException e ) {
                throw new KException( e );
            }
        } else if ( VdbLexicon.Vdb.ENTRIES.equals( name ) ) {
            if ( !VdbLexicon.Vdb.ENTRIES.equals( typeName ) ) {
                throw new KException( Messages.getString( Relational.INVALID_GROUPING_NODE_TYPE,
                                                          VdbLexicon.Vdb.ENTRIES,
                                                          typeName ) );
            }

            try {
                return node( transaction ).hasNode( VdbLexicon.Vdb.ENTRIES );
            } catch ( final RepositoryException e ) {
                throw new KException( e );
            }
        } else if ( VdbLexicon.Vdb.SOURCES.equals( name ) ) {
            if ( !VdbLexicon.Vdb.SOURCES.equals( typeName ) ) {
                throw new KException( Messages.getString( Relational.INVALID_GROUPING_NODE_TYPE,
                                                          VdbLexicon.Vdb.SOURCES,
                                                          typeName ) );
            }

            try {
                return node( transaction ).hasNode( VdbLexicon.Vdb.SOURCES );
            } catch ( final RepositoryException e ) {
                throw new KException( e );
            }
        } else if ( VdbLexicon.Vdb.TRANSLATORS.equals( name ) ) {
            if ( !VdbLexicon.Vdb.TRANSLATORS.equals( typeName ) ) {
                throw new KException( Messages.getString( Relational.INVALID_GROUPING_NODE_TYPE,
                                                          VdbLexicon.Vdb.TRANSLATORS,
                                                          typeName ) );
            }

            try {
                return node( transaction ).hasNode( VdbLexicon.Vdb.TRANSLATORS );
            } catch ( final RepositoryException e ) {
                throw new KException( e );
            }
        } else if ( VdbLexicon.Vdb.IMPORT_VDBS.equals( name ) ) {
            if ( !VdbLexicon.Vdb.IMPORT_VDBS.equals( typeName ) ) {
                throw new KException( Messages.getString( Relational.INVALID_GROUPING_NODE_TYPE,
                                                          VdbLexicon.Vdb.IMPORT_VDBS,
                                                          typeName ) );
            }

            try {
                return node( transaction ).hasNode( VdbLexicon.Vdb.IMPORT_VDBS );
            } catch ( final RepositoryException e ) {
                throw new KException( e );
            }
        }

        return super.hasChild( transaction, name, typeName );
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

}
