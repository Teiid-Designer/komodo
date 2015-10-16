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
import org.komodo.relational.vdb.internal.VdbImportImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Represents a referenced VDB.
 */
public interface VdbImport extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = VdbImport.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_IMPORT;

    /**
     * The default value indicating if the data policies should be imported. Value is {@value} .
     */
    boolean DEFAULT_IMPORT_DATA_POLICIES = true;

    /**
     * An empty array of VDB imports.
     */
    VdbImport[] NO_IMPORTS = new VdbImport[0];

    /**
     * The resolver of a {@link VdbImport}.
     */
    public static final TypeResolver< VdbImport > RESOLVER = new TypeResolver< VdbImport >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public VdbImport create( final UnitOfWork transaction,
                                 final Repository repository,
                                 final KomodoObject parent,
                                 final String id,
                                 final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Vdb parentVdb = adapter.adapt( transaction, parent, Vdb.class );
    
            if ( parentVdb == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          VdbImport.class.getSimpleName() ) );
            }
    
            return parentVdb.addImport( transaction, id );
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
        public Class< VdbImportImpl > owningClass() {
            return VdbImportImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.ImportVdb.IMPORT_VDB );
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public VdbImport resolve( final UnitOfWork transaction,
                                  final KomodoObject kobject ) throws KException {
            return new VdbImportImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    
    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see Vdb#DEFAULT_VERSION
     */
    int getVersion( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if data policies should be imported
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_IMPORT_DATA_POLICIES
     */
    boolean isImportDataPolicies( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newImportDataPolicies
     *        the new value for the <code>import data policies</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_IMPORT_DATA_POLICIES
     */
    void setImportDataPolicies( final UnitOfWork transaction,
                                final boolean newImportDataPolicies ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newVersion
     *        the new value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see Vdb#DEFAULT_VERSION
     */
    void setVersion( final UnitOfWork transaction,
                     final int newVersion ) throws KException;

}
