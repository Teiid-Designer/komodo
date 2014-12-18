/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model procedure result set.
 */
public interface ProcedureResultSet extends Table {

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addAccessPattern(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public AccessPattern addAccessPattern( final UnitOfWork transaction,
                                           final String accessPatternName ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addForeignKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, org.komodo.relational.model.Table)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public ForeignKey addForeignKey( final UnitOfWork transaction,
                                     final String foreignKeyName,
                                     final Table referencedTable ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addUniqueConstraint(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public UniqueConstraint addUniqueConstraint( final UnitOfWork transaction,
                                                 final String constraintName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the parent <code>procedure</code> (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Procedure getProcedure( final UnitOfWork transaction ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeAccessPattern(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public void removeAccessPattern( final UnitOfWork transaction,
                                     final String accessPatternToRemove ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeForeignKey(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public void removeForeignKey( final UnitOfWork transaction,
                                  final String foreignKeyToRemove ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeUniqueConstraint(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public void removeUniqueConstraint( final UnitOfWork transaction,
                                        final String constraintToRemove ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setPrimaryKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public PrimaryKey setPrimaryKey( final UnitOfWork transaction,
                                     final String newPrimaryKeyName ) throws KException;

}
