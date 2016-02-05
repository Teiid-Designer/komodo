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
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Represents a VDB model source.
 */
public interface ModelSource extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = ModelSource.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_MODEL_SOURCE;

    /**
     * An empty array of model sources.
     */
    ModelSource[] NO_SOURCES = new ModelSource[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Model getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link ModelSource}.
     */
    public static final TypeResolver< ModelSource > RESOLVER = new TypeResolver< ModelSource >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public ModelSource create( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject parent,
                                   final String id,
                                   final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Model parentModel = adapter.adapt( transaction, parent, Model.class );

            if ( parentModel == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          ModelSource.class.getSimpleName() ) );
            }

            return parentModel.addSource( transaction, id );
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
        public Class< ModelSourceImpl > owningClass() {
            return ModelSourceImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.Source.SOURCE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public ModelSource resolve( final UnitOfWork transaction,
                                    final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == ModelSource.TYPE_ID ) {
                return ( ModelSource )kobject;
            }

            return new ModelSourceImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>JNDI name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJndiName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>translator name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getTranslatorName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newJndiName
     *        the new value of the <code>JNDI name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setJndiName( final UnitOfWork transaction,
                      final String newJndiName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newTranslatorName
     *        the new value of the <code>translator name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setTranslatorName( final UnitOfWork transaction,
                            final String newTranslatorName ) throws KException;

}
