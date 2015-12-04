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
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.internal.DataTypeResultSetImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * Represents a data type result set.
 */
public interface DataTypeResultSet extends ProcedureResultSet, ResultSetColumn {

    /**
     * The valid data types.
     */
    enum Type {

        BIGDECIMAL,
        BIGINT,
        BIGINTEGER,
        BLOB,
        BOOLEAN,
        BYTE,
        CHAR,
        CLOB,
        DATE,
        DECIMAL,
        DOUBLE,
        FLOAT,
        INTEGER,
        LONG,
        OBJECT,
        REAL,
        SHORT,
        SMALLINT,
        STRING,
        TIME,
        TIMESTAMP,
        TINYINT,
        VARBINARY,
        VARCHAR,
        XML;

        /**
         * The default data type.
         */
        public static final Type DEFAULT_VALUE = STRING;
    }

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.DATA_TYPE_RESULT_SET;

    /**
     * The type identifier.
     */
    int TYPE_ID = DataTypeResultSet.class.hashCode();

    /**
     * The resolver of a {@link DataTypeResultSet}.
     */
    public static final TypeResolver< DataTypeResultSet > RESOLVER = new TypeResolver< DataTypeResultSet >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public DataTypeResultSet create( final UnitOfWork transaction,
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
                                                          DataTypeResultSet.class.getSimpleName() ) );
            }

            if ( parentProc instanceof StoredProcedure ) {
                return ( ( StoredProcedure )parentProc ).setResultSet( transaction, DataTypeResultSet.class );
            }

            if ( parentProc instanceof PushdownFunction ) {
                return ( ( PushdownFunction )parentProc ).setResultSet( transaction, DataTypeResultSet.class );
            }

            throw new KException( Messages.getString( Relational.UNEXPECTED_RESULT_SET_TYPE, clazz.getName() ) );
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
        public Class< DataTypeResultSetImpl > owningClass() {
            return DataTypeResultSetImpl.class;
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
            // must have the right name
            if ( CreateProcedure.RESULT_SET.equals( kobject.getName( transaction ) ) ) {
                return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.RESULT_DATA_TYPE );
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public DataTypeResultSet resolve( final UnitOfWork transaction,
                                          final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == DataTypeResultSet.TYPE_ID ) {
                return ( DataTypeResultSet )kobject;
            }

            return new DataTypeResultSetImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the data type display string
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    String getDisplayString( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>data type</code> property (never empty)
     * @throws KException
     *         if an error occurs
     * @see Type#DEFAULT_VALUE
     */
    Type getType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if an array type
     * @throws KException
     *         if an error occurs
     */
    boolean isArray( final UnitOfWork transaction ) throws KException;

    /**
     * <p>
     * <strong><em>Rename is not allowed!!</em></strong>
     *
     * @see org.komodo.spi.repository.KomodoObject#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException if called
     */
    @Override
    public void rename( final UnitOfWork transaction,
                        final String newName ) throws UnsupportedOperationException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newArray
     *        <code>true</code> if an array type
     * @throws KException
     *         if an error occurs
     */
    void setArray( final UnitOfWork transaction,
                   final boolean newArray ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newType
     *        the new value for the <code>data type</code> property (can be <code>null</code> when setting to default)
     * @throws KException
     *         if an error occurs
     * @see Type#DEFAULT_VALUE
     */
    void setType( final UnitOfWork transaction,
                  final Type newType ) throws KException;

}
