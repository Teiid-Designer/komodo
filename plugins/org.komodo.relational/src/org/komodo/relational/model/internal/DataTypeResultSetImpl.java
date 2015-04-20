/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * An implementation of a relational model procedure data type result set.
 */
public final class DataTypeResultSetImpl extends RelationalObjectImpl implements DataTypeResultSet {

    private static final String ARRAY_SUFFIX = "[]"; //$NON-NLS-1$

    /**
     * The resolver of a {@link DataTypeResultSet}.
     */
    public static final TypeResolver< DataTypeResultSet > RESOLVER = new TypeResolver< DataTypeResultSet >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public DataTypeResultSet create( final UnitOfWork transaction,
                                         final Repository repository,
                                         final KomodoObject parent,
                                         final String id,
                                         final RelationalProperties properties ) throws KException {
            final Class< ? extends AbstractProcedure > clazz = AbstractProcedureImpl.getProcedureType( transaction, parent );
            final AdapterFactory adapter = new AdapterFactory( repository );
            final AbstractProcedure parentProc = adapter.adapt( transaction, parent, clazz );
            return RelationalModelFactory.createDataTypeResultSet( transaction, repository, parentProc );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return DataTypeResultSet.IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< DataTypeResultSetImpl > owningClass() {
            return DataTypeResultSetImpl.class;
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
                // must have the right name
                if (CreateProcedure.RESULT_SET.equals( kobject.getName( transaction ) )) {
                    ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.RESULT_DATA_TYPE );
                    return true;
                }
            } catch (final KException e) {
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
        public DataTypeResultSet resolve( final UnitOfWork transaction,
                                          final KomodoObject kobject ) throws KException {
            return new DataTypeResultSetImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a procedure result set
     */
    public DataTypeResultSetImpl( final UnitOfWork uow,
                                  final Repository repository,
                                  final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.DataTypeResultSet#getDisplayString(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDisplayString( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction( "datatyperesultsetimpl-getDisplayString", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "getDisplayString: transaction = {0}", transaction.getName() ); //$NON-NLS-1$
        }

        try {
            final StringBuilder result = new StringBuilder( getType( transaction ).toString() );
            final long length = getLength( transaction );

            if (length != 0) {
                result.append( '(' ).append( length ).append( ')' );
            }

            if (isArray( transaction )) {
                result.append( "[]" ); //$NON-NLS-1$
            }

            if (uow == null) {
                transaction.commit();
            }

            return result.toString();
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.DataTypeResultSet#getLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLength( final UnitOfWork uow ) throws KException {
        final Long value = getObjectProperty( uow, PropertyValueType.LONG, "getLength", //$NON-NLS-1$
                                              StandardDdlLexicon.DATATYPE_LENGTH );

        if (value == null) {
            return RelationalConstants.DEFAULT_LENGTH;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.DataTypeResultSet#getType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Type getType( final UnitOfWork uow ) throws KException {
        String value = getObjectProperty( uow, PropertyValueType.STRING, "getDataType", StandardDdlLexicon.DATATYPE_NAME ); //$NON-NLS-1$

        if (StringUtils.isBlank( value )) {
            return Type.DEFAULT_VALUE;
        }

        final int index = value.indexOf( ARRAY_SUFFIX );

        if (index != -1) {
            value = value.substring( 0, index );
        }

        final Type result = Type.valueOf( value );

        if (result == null) {
            return Type.DEFAULT_VALUE;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork uow ) {
        return RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.DataTypeResultSet#isArray(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isArray( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty( uow, PropertyValueType.STRING, "getDataType", StandardDdlLexicon.DATATYPE_NAME ); //$NON-NLS-1$

        if (StringUtils.isBlank( value )) {
            return false;
        }

        return ( value.indexOf( ARRAY_SUFFIX ) != -1 );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.DataTypeResultSet#setArray(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setArray( final UnitOfWork uow,
                          final boolean newArray ) throws KException {
        final String value = getObjectProperty( uow, PropertyValueType.STRING, "getDataType", StandardDdlLexicon.DATATYPE_NAME ); //$NON-NLS-1$

        if (StringUtils.isBlank( value )) {
            setObjectProperty( uow, "setArray", StandardDdlLexicon.DATATYPE_NAME, ( Type.DEFAULT_VALUE.name() + ARRAY_SUFFIX ) ); //$NON-NLS-1$
        } else if (!value.endsWith( ARRAY_SUFFIX )) {
            setObjectProperty( uow, "setArray", StandardDdlLexicon.DATATYPE_NAME, ( value + ARRAY_SUFFIX ) ); //$NON-NLS-1$
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.DataTypeResultSet#setLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setLength( final UnitOfWork uow,
                           final long newLength ) throws KException {
        setObjectProperty( uow, "setLength", StandardDdlLexicon.DATATYPE_LENGTH, newLength ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.DataTypeResultSet#setType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.DataTypeResultSet.Type)
     */
    @Override
    public void setType( final UnitOfWork uow,
                         final Type newType ) throws KException {
        String newValue = null;

        if (newType == null) {
            newValue = Type.DEFAULT_VALUE.name();
        } else {
            newValue = newType.name();
        }

        if (isArray( uow )) {
            newValue += ARRAY_SUFFIX;
        }

        setObjectProperty( uow, "setArray", StandardDdlLexicon.DATATYPE_NAME, newValue ); //$NON-NLS-1$
    }

}
