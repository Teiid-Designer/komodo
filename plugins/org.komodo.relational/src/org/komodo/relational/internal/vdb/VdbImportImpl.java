/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a referenced VDB.
 */
public class VdbImportImpl extends RelationalObjectImpl implements VdbImport {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public VdbImportImpl( final Repository repository,
                          final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimportimpl-getVersion", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            int result = Vdb.DEFAULT_VERSION;
            final Property property = getProperty(transaction, VdbLexicon.ImportVdb.VERSION);

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
     * @see org.komodo.relational.vdb.VdbImport#isImportDataPolicies(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isImportDataPolicies( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimportimpl-isImportDataPolicies", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = VdbImport.DEFAULT_IMPORT_DATA_POLICIES;
            final Property property = getProperty(transaction, VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);

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
     * @see org.komodo.relational.vdb.VdbImport#setImportDataPolicies(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setImportDataPolicies( final UnitOfWork uow,
                                       final boolean newImportDataPolicies ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimportimpl-setImportDataPolicies", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setImportDataPolicies: transaction = '{0}', newImportDataPolicies = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newImportDataPolicies);
        }

        try {
            setProperty(transaction, VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES, newImportDataPolicies);

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
     * @see org.komodo.relational.vdb.VdbImport#setVersion(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setVersion( final UnitOfWork uow,
                            final int newVersion ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimportimpl-setVersion", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setVersion: transaction = '{0}', newVersion = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newVersion);
        }

        try {
            setProperty(transaction, VdbLexicon.ImportVdb.VERSION, newVersion);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
