/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.vdb.internal.EntryImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Represents a VDB entry.
 */
public interface Entry extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Entry.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_ENTRY;

    /**
     * An empty array of VDB entries.
     */
    Entry[] NO_ENTRIES = new Entry[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Vdb getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link Entry}.
     */
    static final TypeResolver< Entry > RESOLVER = new TypeResolver< Entry >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Entry create( final UnitOfWork transaction,
                             final Repository repository,
                             final KomodoObject parent,
                             final String id,
                             final RelationalProperties properties ) throws KException {
            final Object entryPathValue = properties.getValue( VdbLexicon.Entry.PATH );
            final String entryPath = entryPathValue == null ? null : entryPathValue.toString();
            final AdapterFactory adapter = new AdapterFactory( );
            final Vdb parentVdb = adapter.adapt( transaction, parent, Vdb.class );

            if ( parentVdb == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          Entry.class.getSimpleName() ) );
            }

            return parentVdb.addEntry( transaction, id, entryPath );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< EntryImpl > owningClass() {
            return EntryImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.Entry.ENTRY );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Entry resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Entry.TYPE_ID ) {
                return ( Entry )kobject;
            }

            return new EntryImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>file path</code> property (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getPath( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPath
     *        the new value of the <code>file path</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setPath( final UnitOfWork transaction,
                  final String newPath ) throws KException;

}
