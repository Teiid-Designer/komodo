/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import org.komodo.relational.internal.RelationalObjectImpl;
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
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public VdbImportImpl( final UnitOfWork uow,
                          final Repository repository,
                          final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.INTEGER, "getVersion", VdbLexicon.ImportVdb.VERSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#isImportDataPolicies(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isImportDataPolicies( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.BOOLEAN, "isImportDataPolicies", //$NON-NLS-1$
                                 VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#setImportDataPolicies(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setImportDataPolicies( final UnitOfWork uow,
                                       final boolean newImportDataPolicies ) throws KException {
        setObjectProperty(uow, "setImportDataPolicies", VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES, newImportDataPolicies); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#setVersion(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setVersion( final UnitOfWork uow,
                            final int newVersion ) throws KException {
        setObjectProperty(uow, "setVersion", VdbLexicon.ImportVdb.VERSION, newVersion); //$NON-NLS-1$
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
        validateType(uow, path, VdbLexicon.ImportVdb.IMPORT_VDB);
    }

}
