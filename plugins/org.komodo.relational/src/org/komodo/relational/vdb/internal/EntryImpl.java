/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.vdb.Entry;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB entry.
 */
public final class EntryImpl extends RelationalObjectImpl implements Entry {

    /**
     * The resolver of a {@link Entry}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

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
                ObjectImpl.validateType(transaction, repository, kobject, VdbLexicon.Entry.ENTRY);
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
        public Entry resolve( final UnitOfWork transaction,
                              final Repository repository,
                              final KomodoObject kobject ) throws KException {
            return new EntryImpl(transaction, repository, kobject.getAbsolutePath());
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
    public EntryImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Entry#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getDescription", VdbLexicon.Entry.DESCRIPTION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork transaction ) throws KException {
        final KomodoObject grouping = super.getParent(transaction);
        return resolveType(transaction, grouping.getParent(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Entry#getPath(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getPath( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getPath", VdbLexicon.Entry.PATH); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Entry#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String newDescription ) throws KException {
        setObjectProperty(uow, "setDescription", VdbLexicon.Entry.DESCRIPTION, newDescription); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Entry#setPath(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setPath( final UnitOfWork uow,
                         final String newPath ) throws KException {
        ArgCheck.isNotEmpty(newPath, "newPath"); //$NON-NLS-1$
        setObjectProperty(uow, "setPath", VdbLexicon.Entry.PATH, newPath); //$NON-NLS-1$
    }

}
