/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.Messages;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.internal.VirtualProcedureImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;

/**
 * Represents a virtual procedure (CREATE VIRTUAL PROCEDURE).
 */
public interface VirtualProcedure extends Procedure {

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.VIRTUAL_PROCEDURE;

    /**
     * An empty array of virtual procedures.
     */
    VirtualProcedure[] NO_PROCEDURES = new VirtualProcedure[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = VirtualProcedure.class.hashCode();

    /**
     * The resolver of a {@link VirtualProcedure}.
     */
    public static final TypeResolver< VirtualProcedure > RESOLVER = new TypeResolver< VirtualProcedure >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public VirtualProcedure create( final UnitOfWork transaction,
                                        final Repository repository,
                                        final KomodoObject parent,
                                        final String id,
                                        final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Model parentModel = adapter.adapt( transaction, parent, Model.class );
    
            if ( parentModel == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          VirtualProcedure.class.getSimpleName() ) );
            }
    
            return parentModel.addVirtualProcedure( transaction, id );
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
        public Class< VirtualProcedureImpl > owningClass() {
            return VirtualProcedureImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.PROCEDURE_STATEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
                                                        kobject,
                                                        SchemaElement.TYPE,
                                                        SchemaElementType.VIRTUAL.name() );
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public VirtualProcedure resolve( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
            return new VirtualProcedureImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    
    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the AS clause <code>statement</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getAsClauseStatement( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newStatement
     *        the new AS clause statement (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAsClauseStatement( final UnitOfWork transaction,
                               final String newStatement ) throws KException;

}
