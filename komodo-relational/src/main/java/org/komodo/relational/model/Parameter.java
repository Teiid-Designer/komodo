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
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.internal.ParameterImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants.TeiidNonReservedWord;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants.TeiidReservedWord;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * Represents a relational model procedure parameter.
 */
public interface Parameter extends OptionContainer, RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Parameter.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.PARAMETER;

    /**
     * Represents a parameter direction.
     */
    public enum Direction {

        /**
         * Input parameters have this direction.
         */
        IN( TeiidReservedWord.IN.toDdl() ),

        /**
         * Input/output parameters have this direction.
         */
        IN_OUT( TeiidReservedWord.INOUT.toDdl() ),

        /**
         * Output parameters have this direction.
         */
        OUT( TeiidReservedWord.OUT.toDdl() ),

        /**
         * Variable number of parameters have this direction.
         */
        VARIADIC( TeiidNonReservedWord.VARIADIC.toDdl() );

        /**
         * The default direction. Value is {@value} .
         */
        public static final Direction DEFAULT_VALUE = IN;

        /**
         * @param value
         *        the value whose <code>Direction</code> is being requested (can be empty)
         * @return the corresponding <code>Direction</code> or the default value if not found
         * @see #DEFAULT_VALUE
         */
        public static Direction fromValue( final String value ) {
            for (final Direction nullable : values()) {
                if (nullable.value.equals(value)) {
                    return nullable;
                }
            }

            return DEFAULT_VALUE;
        }

        private final String value;

        private Direction( final String value ) {
            this.value = value;
        }

        /**
         * @return the Teiid direction value (never empty)
         */
        public String toValue() {
            return this.value;
        }

    }

    /**
     * The default value for the <code>is result</code> property. Value is {@value} .
     */
    boolean DEFAULT_RESULT = false;

    /**
     * An empty array of parameters.
     */
    Parameter[] NO_PARAMETERS = new Parameter[0];
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    AbstractProcedure getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link Parameter}.
     */
    public static final TypeResolver< Parameter > RESOLVER = new TypeResolver< Parameter >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Parameter create( final UnitOfWork transaction,
                                 final Repository repository,
                                 final KomodoObject parent,
                                 final String id,
                                 final RelationalProperties properties ) throws KException {
            final Class< ? extends AbstractProcedure > clazz = AbstractProcedure.Utils.getProcedureType( transaction, parent );
            final AdapterFactory adapter = new AdapterFactory( );
            final AbstractProcedure parentProc = adapter.adapt( transaction, parent, clazz );

            if ( parentProc == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          Parameter.class.getSimpleName() ) );
            }

            return parentProc.addParameter( transaction, id );
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
        public Class< ParameterImpl > owningClass() {
            return ParameterImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.PARAMETER );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Parameter resolve( final UnitOfWork transaction,
                                  final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Parameter.TYPE_ID ) {
                return ( Parameter )kobject;
            }

            return new ParameterImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>data type name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDatatypeName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>default value</code> (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDefaultValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>direction</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Direction#DEFAULT_VALUE
     */
    Direction getDirection( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype length</code> property
     * @throws KException
     *         if an error occurs
     */
    long getLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>nullable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    Nullable getNullable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     */
    long getPrecision( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     */
    long getScale( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this parameter is the procedure result
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_RESULT
     */
    boolean isResult( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newTypeName
     *        the new datatype name (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setDatatypeName( final UnitOfWork transaction,
                          final String newTypeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDefaultValue
     *        the new default value (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setDefaultValue( final UnitOfWork transaction,
                          final String newDefaultValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDirection
     *        the new direction (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Direction#DEFAULT_VALUE
     */
    void setDirection( final UnitOfWork transaction,
                       final Direction newDirection ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newLength
     *        the new value of the <code>datatype length</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLength( final UnitOfWork transaction,
                    final long newLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNullable
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    void setNullable( final UnitOfWork transaction,
                      final Nullable newNullable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPrecision
     *        the new value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     */
    void setPrecision( final UnitOfWork transaction,
                       final long newPrecision ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newResult
     *        the new value for the <code>is result</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_RESULT
     */
    void setResult( final UnitOfWork transaction,
                    final boolean newResult ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newScale
     *        the new value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     */
    void setScale( final UnitOfWork transaction,
                   final long newScale ) throws KException;

}
