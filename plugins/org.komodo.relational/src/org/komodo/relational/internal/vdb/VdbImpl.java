/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a virtual database manifest.
 */
public final class VdbImpl extends RelationalObjectImpl implements Vdb {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public VdbImpl( final Repository repository,
                    final String workspacePath ) throws KException {
        super(repository, workspacePath);
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
            LOGGER.debug("addDataRole: transaction = '{0}', dataRoleName = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("addEntry: transaction = '{0}', entryName = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("addImport: transaction = '{0}', vdbName = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("addTranslator: transaction = '{0}', translatorName = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.vdb.Vdb#getConnectionType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getConnectionType( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getConnectionType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, VdbLexicon.Vdb.CONNECTION_TYPE);

            if (property != null) {
                result = property.getStringValue();
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
            LOGGER.debug("getDataRoles: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            DataRole[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.DATA_ROLES)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.DATA_ROLES);
                final List< DataRole > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.DataRole.DATA_ROLE)) {
                    final DataRole dataRole = new DataRoleImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getDataRoles: transaction = '{0}', found data role = '{1}'", //$NON-NLS-1$
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
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getDescription", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, VdbLexicon.Vdb.DESCRIPTION);

            if (property != null) {
                result = property.getStringValue();
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
            LOGGER.debug("getEntries: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Entry[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.ENTRIES)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.ENTRIES);
                final List< Entry > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.Entry.ENTRY)) {
                    final Entry entry = new EntryImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getEntries: transaction = '{0}', found entry = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("getImports: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            VdbImport[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.IMPORT_VDBS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.IMPORT_VDBS);
                final List< VdbImport > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.ImportVdb.IMPORT_VDB)) {
                    final VdbImport vdbImport = new VdbImportImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getImports: transaction = '{0}', found VDB import = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.vdb.Vdb#getOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOriginalFilePath( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getOriginalFilePath", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, VdbLexicon.Vdb.ORIGINAL_FILE);

            if (property != null) {
                result = property.getStringValue();
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
            LOGGER.debug("getTranslators: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Translator[] result = null;

            if (hasChild(transaction, VdbLexicon.Vdb.TRANSLATORS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.TRANSLATORS);
                final List< Translator > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction, VdbLexicon.Translator.TRANSLATOR)) {
                    final Translator translator = new TranslatorImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getTranslators: transaction = '{0}', found translator = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.vdb.Vdb#getVdbName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getVdbName( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-getVdbName", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, VdbLexicon.Vdb.NAME);

            if (property != null) {
                result = property.getStringValue();
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
     * @see org.komodo.relational.vdb.Vdb#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbImpl-getVersion", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            int result = Vdb.DEFAULT_VERSION;
            final Property property = getProperty(transaction, VdbLexicon.Vdb.VERSION);

            if (property != null) {
                result = (int)property.getLongValue();
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
     * @see org.komodo.relational.vdb.Vdb#isPreview(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isPreview( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-isPreview", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Vdb.DEFAULT_PREVIEW;
            final Property property = getProperty(transaction, VdbLexicon.Vdb.PREVIEW);

            if (property != null) {
                result = property.getBooleanValue();
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
            LOGGER.debug("removeDataRole: transaction = '{0}', dataRoleToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         dataRoleToRemove);
        }

        boolean found = false;

        try {
            if (hasChild(transaction, VdbLexicon.Vdb.DATA_ROLES)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.DATA_ROLES);

                if (grouping.hasChild(transaction, dataRoleToRemove)) {
                    grouping.removeChild(transaction, dataRoleToRemove);
                    found = true;
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
            LOGGER.debug("removeEntry: transaction = '{0}', entryToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         entryToRemove);
        }

        boolean found = false;

        try {
            if (hasChild(transaction, VdbLexicon.Vdb.ENTRIES)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.ENTRIES);

                if (grouping.hasChild(transaction, entryToRemove)) {
                    grouping.removeChild(transaction, entryToRemove);
                    found = true;
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
            LOGGER.debug("removeImport: transaction = '{0}', importToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         importToRemove);
        }

        boolean found = false;

        try {
            if (hasChild(transaction, VdbLexicon.Vdb.IMPORT_VDBS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.IMPORT_VDBS);

                if (grouping.hasChild(transaction, importToRemove)) {
                    grouping.removeChild(transaction, importToRemove);
                    found = true;
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
            LOGGER.debug("removeTranslator: transaction = '{0}', translatorToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         translatorToRemove);
        }

        boolean found = false;

        try {
            if (hasChild(transaction, VdbLexicon.Vdb.TRANSLATORS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.Vdb.TRANSLATORS);

                if (grouping.hasChild(transaction, translatorToRemove)) {
                    grouping.removeChild(transaction, translatorToRemove);
                    found = true;
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
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-setConnectionType", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setConnectionType: transaction = '{0}', newConnectionType = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newConnectionType);
        }

        try {
            setProperty(transaction,
                        VdbLexicon.Vdb.CONNECTION_TYPE,
                        StringUtils.isBlank(newConnectionType) ? null : newConnectionType);

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
     * @see org.komodo.relational.vdb.Vdb#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String newDescription ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-setDescription", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setDescription: transaction = '{0}', newDescription = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newDescription);
        }

        try {
            setProperty(transaction, VdbLexicon.Vdb.DESCRIPTION, StringUtils.isBlank(newDescription) ? null : newDescription);

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
     * @see org.komodo.relational.vdb.Vdb#setOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setOriginalFilePath( final UnitOfWork uow,
                                     final String newOriginalFilePath ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-setOriginalFilePath", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setOriginalFilePath: transaction = '{0}', newOriginalFilePath = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newOriginalFilePath);
        }

        try {
            ArgCheck.isNotEmpty(newOriginalFilePath, "newOriginalFilePath"); //$NON-NLS-1$
            setProperty(transaction, VdbLexicon.Vdb.ORIGINAL_FILE, newOriginalFilePath);

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
     * @see org.komodo.relational.vdb.Vdb#setPreview(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setPreview( final UnitOfWork uow,
                            final boolean newPreview ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setPreview", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setPreview: transaction = '{0}', newPreview = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newPreview);
        }

        try {
            setProperty(transaction, VdbLexicon.Vdb.PREVIEW, newPreview);

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
     * @see org.komodo.relational.vdb.Vdb#setVdbName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setVdbName( final UnitOfWork uow,
                            final String newVdbName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-setVdbName", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setVdbName: transaction = '{0}', newVdbName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newVdbName);
        }

        try {
            setProperty(transaction, VdbLexicon.Vdb.NAME, StringUtils.isBlank(newVdbName) ? null : newVdbName);

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
     * @see org.komodo.relational.vdb.Vdb#setVersion(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setVersion( final UnitOfWork uow,
                            final int newVersion ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimpl-setVersion", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setVersion: transaction = '{0}', newVersion = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newVersion);
        }

        try {
            setProperty(transaction, VdbLexicon.Vdb.VERSION, newVersion);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
