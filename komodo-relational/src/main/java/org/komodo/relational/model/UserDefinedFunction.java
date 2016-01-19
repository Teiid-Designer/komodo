/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.internal.UserDefinedFunctionImpl;
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
 * Represents a user-defined function (CREATE VIRTUAL FUNCTION).
 */
public interface UserDefinedFunction extends Function {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.USER_DEFINED_FUNCTION;

    /**
     * An empty array of UDFs.
     */
    UserDefinedFunction[] NO_UDFS = new UserDefinedFunction[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = UserDefinedFunction.class.hashCode();
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Model getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link UserDefinedFunction}.
     */
    public static final TypeResolver< UserDefinedFunction > RESOLVER = new TypeResolver< UserDefinedFunction >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public UserDefinedFunction create( final UnitOfWork transaction,
                                           final Repository repository,
                                           final KomodoObject parent,
                                           final String id,
                                           final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Model parentModel = adapter.adapt( transaction, parent, Model.class );

            if ( parentModel == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          UserDefinedFunction.class.getSimpleName() ) );
            }

            return parentModel.addUserDefinedFunction( transaction, id );
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
        public Class< UserDefinedFunctionImpl > owningClass() {
            return UserDefinedFunctionImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.FUNCTION_STATEMENT )
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
        public UserDefinedFunction resolve( final UnitOfWork transaction,
                                            final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == UserDefinedFunction.TYPE_ID ) {
                return ( UserDefinedFunction )kobject;
            }

            return new UserDefinedFunctionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>category</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getCategory( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>Java class name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJavaClass( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>Java method name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJavaMethod( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCategory
     *        the new value of the <code>category</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setCategory( final UnitOfWork transaction,
                      final String newCategory ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newJavaClass
     *        the new value of the <code>Java class name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setJavaClass( final UnitOfWork transaction,
                       final String newJavaClass ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newJavaMethod
     *        the new value of the <code>Java method name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setJavaMethod( final UnitOfWork transaction,
                        final String newJavaMethod ) throws KException;

}
