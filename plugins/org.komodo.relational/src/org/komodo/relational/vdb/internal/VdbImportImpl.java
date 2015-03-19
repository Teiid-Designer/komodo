/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.PropertyValueType;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a referenced VDB.
 */
public class VdbImportImpl extends RelationalObjectImpl implements VdbImport {

    /**
     * The resolver of a {@link VdbImport}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public VdbImport create( final UnitOfWork transaction,
                                 final Repository repository,
                                 final KomodoObject parent,
                                 final String id,
                                 final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final Vdb parentVdb = adapter.adapt( transaction, parent, Vdb.class );
            return RelationalModelFactory.createVdbImport( transaction, repository, parentVdb, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< VdbImportImpl > owningClass() {
            return VdbImportImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.ImportVdb.IMPORT_VDB );
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
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public VdbImport resolve( final UnitOfWork transaction,
                                  final KomodoObject kobject ) throws KException {
            return new VdbImportImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
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
    public VdbImportImpl( final UnitOfWork uow,
                          final Repository repository,
                          final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return KomodoType.VDB_IMPORT;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("vdbimportimpl-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = super.getParent(transaction);
            final KomodoObject result = resolveType(transaction, grouping.getParent(transaction));

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
     * @see org.komodo.relational.vdb.VdbImport#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.INTEGER, "getVersion", VdbLexicon.ImportVdb.VERSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#isImportDataPolicies(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isImportDataPolicies( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isImportDataPolicies", //$NON-NLS-1$
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

}
