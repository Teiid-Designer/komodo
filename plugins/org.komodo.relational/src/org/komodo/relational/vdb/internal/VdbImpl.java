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
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.modeshape.visitor.VdbNodeVisitor;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * An implementation of a virtual database manifest.
 */
public final class VdbImpl extends RelationalObjectImpl implements Vdb {

    private class VdbManifestImpl implements VdbManifest {

        private final String xml;

        VdbManifestImpl( final UnitOfWork transaction,
                         final VdbImpl vdb ) throws KException {
            final StringWriter writer = new StringWriter();

            try {
                final XMLOutputFactory xof = XMLOutputFactory.newInstance();
                final XMLStreamWriter xsw = xof.createXMLStreamWriter(writer);

                final VdbNodeVisitor visitor = new VdbNodeVisitor(TeiidVersionProvider.getInstance().getTeiidVersion(), xsw);
                visitor.visit(vdb.node(transaction));
            } catch (final Exception e) {
                throw new KException(e);
            }

            // Create an XML Document from the filled writer
            this.xml = writer.toString();

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
         * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork)
         */
        @Override
        public String export( final UnitOfWork transaction ) {
            return this.xml;
        }

    }

    /**
     * The resolver of a {@link Vdb}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        @Override
        public Class<? extends KomodoObject> owningClass() {
            return VdbImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType(transaction, repository, kobject, VdbLexicon.Vdb.VIRTUAL_DATABASE);
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Vdb resolve( final UnitOfWork transaction,
                            final Repository repository,
                            final KomodoObject kobject ) throws KException {
            return new VdbImpl(transaction, repository, kobject.getAbsolutePath());
        }

        @Override
        public Vdb create(UnitOfWork transaction,
                                                      KomodoObject parent,
                                                      String id,
                                                      RelationalProperties properties) throws KException {
            Object origFilePathValue = properties.getValue(VdbLexicon.Vdb.ORIGINAL_FILE);
            String origFilePath = origFilePathValue == null ? null : origFilePathValue.toString();
            return RelationalModelFactory.createVdb(transaction, parent.getRepository(), parent.getAbsolutePath(), id, origFilePath);
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
        return KomodoType.VDB;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addDataRole(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public DataRole addDataRole( final UnitOfWork uow,
                                 final String dataRoleName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-addDataRole", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addDataRole: transaction = {0}, dataRoleName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         dataRoleName);
        }

        try {
            final DataRole result = RelationalModelFactory.createDataRole(transaction, getRepository(), this, dataRoleName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addEntry(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public Entry addEntry( final UnitOfWork uow,
                           final String entryName,
                           final String entryPath ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-addEntry", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addEntry: transaction = {0}, entryName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         entryName);
        }

        try {
            final Entry result = RelationalModelFactory.createEntry(transaction, getRepository(), this, entryName, entryPath);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public VdbImport addImport( final UnitOfWork uow,
                                final String vdbName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-addImport", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addImport: transaction = {0}, vdbName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         vdbName);
        }

        try {
            final VdbImport result = RelationalModelFactory.createVdbImport(transaction, getRepository(), this, vdbName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Model addModel( final UnitOfWork uow,
                           final String modelName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-addModel", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addModel: transaction = {0}, modelName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         modelName);
        }

        try {
            final Model result = RelationalModelFactory.createModel(transaction, getRepository(), this, modelName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public Translator addTranslator( final UnitOfWork uow,
                                     final String translatorName,
                                     final String translatorType ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-addTranslator", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addTranslator: transaction = {0}, translatorName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         translatorName);
        }

        try {
            final Translator result = RelationalModelFactory.createTranslator(transaction,
                                                                              getRepository(),
                                                                              this,
                                                                              translatorName,
                                                                              translatorType);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#createManifest(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public VdbManifest createManifest( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-createManifest", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("createManifest: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final VdbManifest result = new VdbManifestImpl(transaction, this);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String export( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-export", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("vdbimpl-export: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final String result = createManifest(transaction).export(transaction);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("vdbimpl-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final DataRole[] dataRoles = getDataRoles(transaction);
            final Entry[] entries = getEntries(transaction);
            final VdbImport[] imports = getImports(transaction);
            final Model[] models = getModels(transaction);
            final Translator[] translators = getTranslators(transaction);

            final KomodoObject[] result = new KomodoObject[dataRoles.length + entries.length + imports.length + models.length
                                                           + translators.length];
            System.arraycopy(dataRoles, 0, result, 0, dataRoles.length);
            System.arraycopy(entries, 0, result, dataRoles.length, entries.length);
            System.arraycopy(imports, 0, result, dataRoles.length + entries.length, imports.length);
            System.arraycopy(models, 0, result, dataRoles.length + entries.length + imports.length, models.length);
            System.arraycopy(translators,
                             0,
                             result,
                             dataRoles.length + entries.length + imports.length + models.length,
                             translators.length);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("dataroleimpl-getChildrenOfType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject[] result = null;

            if (VdbLexicon.DataRole.DATA_ROLE.equals(type)) {
                result = getDataRoles(transaction);
            } else if (VdbLexicon.Entry.ENTRY.equals(type)) {
                result = getEntries(transaction);
            } else if (VdbLexicon.ImportVdb.IMPORT_VDB.equals(type)) {
                result = getImports(transaction);
            } else if (VdbLexicon.Vdb.DECLARATIVE_MODEL.equals(type)) {
                result = getModels(transaction);
            } else if (VdbLexicon.Translator.TRANSLATOR.equals(type)) {
                result = getTranslators(transaction);
            } else {
                result = KomodoObject.EMPTY_ARRAY;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getConnectionType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getConnectionType( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getConnectionType", VdbLexicon.Vdb.CONNECTION_TYPE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getDataRoles(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public DataRole[] getDataRoles( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getDataRoles", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getDataRoles: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            DataRole[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.DATA_ROLES)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.DATA_ROLES);
                final List< DataRole > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.DataRole.DATA_ROLE)) {
                    final DataRole dataRole = new DataRoleImpl(transaction, getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getDataRoles: transaction = {0}, found data role = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    temp.add(dataRole);
                }

                result = temp.toArray(new DataRole[temp.size()]);
            } else {
                result = DataRole.NO_DATA_ROLES;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getDescription", VdbLexicon.Vdb.DESCRIPTION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getEntries(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Entry[] getEntries( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getEntries", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getEntries: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Entry[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.ENTRIES)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.ENTRIES);
                final List< Entry > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.Entry.ENTRY)) {
                    final Entry entry = new EntryImpl(transaction, getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getEntries: transaction = {0}, found entry = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    temp.add(entry);
                }

                result = temp.toArray(new Entry[temp.size()]);
            } else {
                result = Entry.NO_ENTRIES;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getImports(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public VdbImport[] getImports( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getImports", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getImports: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            VdbImport[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.IMPORT_VDBS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.IMPORT_VDBS);
                final List< VdbImport > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.ImportVdb.IMPORT_VDB)) {
                    final VdbImport vdbImport = new VdbImportImpl(transaction, getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getImports: transaction = {0}, found VDB import = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    temp.add(vdbImport);
                }

                result = temp.toArray(new VdbImport[temp.size()]);
            } else {
                result = VdbImport.NO_IMPORTS;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getModels(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Model[] getModels( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getModels", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getModels: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Model > result = new ArrayList<>();

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
                final Model model = new ModelImpl(transaction, getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getModels: transaction = {0}, found model = {1}", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(model);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return Model.NO_MODELS;
            }

            return result.toArray(new Model[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOriginalFilePath( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getOriginalFilePath", VdbLexicon.Vdb.ORIGINAL_FILE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getTranslators(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Translator[] getTranslators( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getTranslators", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getTranslators: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Translator[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.TRANSLATORS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.TRANSLATORS);
                final List< Translator > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.Translator.TRANSLATOR)) {
                    final Translator translator = new TranslatorImpl(transaction, getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getTranslators: transaction = {0}, found translator = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    temp.add(translator);
                }

                result = temp.toArray(new Translator[temp.size()]);
            } else {
                result = Translator.NO_TRANSLATORS;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
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
        return getObjectProperty(uow, Property.ValueType.STRING, "getVdbName", VdbLexicon.Vdb.NAME); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.INTEGER, "getVersion", VdbLexicon.Vdb.VERSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#isPreview(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isPreview( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.BOOLEAN, "isPreview", VdbLexicon.Vdb.PREVIEW); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeDataRole(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeDataRole( final UnitOfWork uow,
                                final String dataRoleToRemove ) throws KException {
        ArgCheck.isNotEmpty(dataRoleToRemove, "dataRoleToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-removeDataRole", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeDataRole: transaction = {0}, dataRoleToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         dataRoleToRemove);
        }

        boolean found = false;

        try {
            final DataRole[] dataRoles = getDataRoles(transaction);

            if (dataRoles.length != 0) {
                for (final DataRole dataRole : dataRoles) {
                    if (dataRoleToRemove.equals(dataRole.getName(transaction))) {
                        final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.DATA_ROLES);
                        grouping.removeChild(transaction, dataRoleToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.DATA_ROLE_NOT_FOUND_TO_REMOVE, dataRoleToRemove));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeEntry(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeEntry( final UnitOfWork uow,
                             final String entryToRemove ) throws KException {
        ArgCheck.isNotEmpty(entryToRemove, "entryToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-removeEntry", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeEntry: transaction = {0}, entryToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         entryToRemove);
        }

        boolean found = false;

        try {
            final Entry[] entries = getEntries(transaction);

            if (entries.length != 0) {
                for (final Entry entry : entries) {
                    if (entryToRemove.equals(entry.getName(transaction))) {
                        final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.ENTRIES);
                        grouping.removeChild(transaction, entryToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.ENTRY_NOT_FOUND_TO_REMOVE, entryToRemove));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeImport( final UnitOfWork uow,
                              final String importToRemove ) throws KException {
        ArgCheck.isNotEmpty(importToRemove, "importToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-removeImport", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeImport: transaction = {0}, importToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         importToRemove);
        }

        boolean found = false;

        try {
            final VdbImport[] vdbImports = getImports(transaction);

            if (vdbImports.length != 0) {
                for (final VdbImport vdbImport : vdbImports) {
                    if (importToRemove.equals(vdbImport.getName(transaction))) {
                        final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.IMPORT_VDBS);
                        grouping.removeChild(transaction, importToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.VDB_IMPORT_NOT_FOUND_TO_REMOVE, importToRemove));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeModel( final UnitOfWork uow,
                             final String modelToRemove ) throws KException {
        ArgCheck.isNotEmpty(modelToRemove, "modelToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-removeModel", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeModel: transaction = {0}, modelToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         modelToRemove);
        }

        boolean found = false;

        try {
            final Model[] models = getModels(transaction);

            if (models.length != 0) {
                for (final Model model : models) {
                    if (modelToRemove.equals(model.getName(transaction))) {
                        removeChild(transaction, modelToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.MODEL_NOT_FOUND_TO_REMOVE, modelToRemove));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTranslator( final UnitOfWork uow,
                                  final String translatorToRemove ) throws KException {
        ArgCheck.isNotEmpty(translatorToRemove, "translatorToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-removeTranslator", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeTranslator: transaction = {0}, translatorToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         translatorToRemove);
        }

        boolean found = false;

        try {
            final Translator[] translators = getTranslators(transaction);

            if (translators.length != 0) {
                for (final Translator translator : translators) {
                    if (translatorToRemove.equals(translator.getName(transaction))) {
                        final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.TRANSLATORS);
                        grouping.removeChild(transaction, translatorToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.TRANSLATOR_NOT_FOUND_TO_REMOVE, translatorToRemove));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
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
