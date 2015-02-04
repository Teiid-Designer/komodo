/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model column.
 */
public interface Column extends RelationalObject {
    /*
      - teiidddl:autoIncrement (boolean) = 'false'
      - ddl:nullable (string) = 'NULL' mandatory autocreated < 'NULL', 'NOT NULL'
      - ddl:datatypeName (STRING)
      - ddl:datatypeLength (LONG)
      - ddl:datatypePrecision (LONG)
      - ddl:datatypeScale (LONG)
    TODO - ddl:defaultOption (STRING)
        < 'LITERAL', 'DATETIME', 'USER', 'CURRENT_USER', 'SESSION_USER', 'SYSTEM_USER', 'NULL'
      - ddl:defaultValue (STRING)
    TODO - ddl:defaultPrecision (LONG)
      - ddl:collationName (STRING)
    TODO + ddl:dropBehavior (ddl:simpleProperty) = ddl:simpleProperty
    TODO + ddl:columnAttribute (ddl:simpleProperty) = ddl:simpleProperty sns
     */

    /**
     * An empty array of columns.
     */
    Column[] NO_COLUMNS = new Column[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getCollationName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>datatype name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_DATATYPE_NAME
     */
    String getDatatypeName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDefaultValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    long getLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>nullable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    Nullable getNullable( final UnitOfWork transaction ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Table getParent( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    int getPrecision( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    int getScale( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if auto-incremented
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_AUTO_INCREMENTED
     */
    boolean isAutoIncremented( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAutoIncremented
     *        the new value for the <code>auto-incremented</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_AUTO_INCREMENTED
     */
    void setAutoIncremented( final UnitOfWork transaction,
                             final boolean newAutoIncremented ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newCollationName
     *        the new value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setCollationName( final UnitOfWork transaction,
                           final String newCollationName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newTypeName
     *        the new value of the <code>datatype name</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_DATATYPE_NAME
     */
    void setDatatypeName( final UnitOfWork transaction,
                          final String newTypeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDefaultValue
     *        the new value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setDefaultValue( final UnitOfWork transaction,
                          final String newDefaultValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newLength
     *        the new value of the <code>length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    void setLength( final UnitOfWork transaction,
                    final long newLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newNullable
     *        the new value of the <code>nullable</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    void setNullable( final UnitOfWork transaction,
                      final Nullable newNullable ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newPrecision
     *        the new value of the <code>precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    void setPrecision( final UnitOfWork transaction,
                       final int newPrecision ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newScale
     *        the new value of the <code>scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    void setScale( final UnitOfWork transaction,
                   final int newScale ) throws KException;

}
