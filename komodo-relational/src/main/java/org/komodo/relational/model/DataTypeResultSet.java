/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.model.internal.DataTypeResultSetImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateProcedure;

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
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    AbstractProcedure getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link DataTypeResultSet}.
     */
    TypeResolver< DataTypeResultSet > RESOLVER = new TypeResolver< DataTypeResultSet >() {

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
