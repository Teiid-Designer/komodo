/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants.OnCommit;
import org.komodo.relational.RelationalConstants.TemporaryType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model table.
 */
public interface Table extends OptionContainer, RelationalObject, SchemaElement {

    /**
     * An empty array of tables.
     */
    Table[] NO_TABLES = new Table[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param accessPatternName
     *        the name of the access pattern being added (cannot be empty)
     * @return the new access pattern (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    AccessPattern addAccessPattern( final UnitOfWork transaction,
                                    final String accessPatternName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param columnName
     *        the name of the column being added (cannot be empty)
     * @return the new column (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Column addColumn( final UnitOfWork transaction,
                      final String columnName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param foreignKeyName
     *        the name of the foreign key being added (cannot be empty)
     * @param referencedTable
     *        the table referenced by this foreign key (cannot be <code>null</code>)
     * @return the new foreign key (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ForeignKey addForeignKey( final UnitOfWork transaction,
                              final String foreignKeyName,
                              final Table referencedTable ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param indexName
     *        the name of the index being added (cannot be empty)
     * @return the new index (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Index addIndex( final UnitOfWork transaction,
                    final String indexName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param constraintName
     *        the name of the unique constraint being added (cannot be empty)
     * @return the new unique constraint (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    UniqueConstraint addUniqueConstraint( final UnitOfWork transaction,
                                          final String constraintName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the access patterns (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    AccessPattern[] getAccessPatterns( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the columns (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Column[] getColumns( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the foreign keys (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    ForeignKey[] getForeignKeys( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the indexes (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Index[] getIndexes( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the on commit value (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    OnCommit getOnCommitValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the primary key (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    PrimaryKey getPrimaryKey( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the query expression (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getQueryExpression( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the temporary table type or <code>null</code> if not a temporary table
     * @throws KException
     *         if an error occurs
     */
    TemporaryType getTemporaryTableType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the unique constraints (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    UniqueConstraint[] getUniqueConstraints( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param accessPatternToRemove
     *        the name of the access pattern being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeAccessPattern( final UnitOfWork transaction,
                              final String accessPatternToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param columnToRemove
     *        the name of the column being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeColumn( final UnitOfWork transaction,
                       final String columnToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param foreignKeyToRemove
     *        the name of the foreign key being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeForeignKey( final UnitOfWork transaction,
                           final String foreignKeyToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param indexToRemove
     *        the name of the index being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeIndex( final UnitOfWork transaction,
                      final String indexToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @throws KException
     *         if an error occurs
     */
    void removePrimaryKey( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param constraintToRemove
     *        the name of the unique constraint being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeUniqueConstraint( final UnitOfWork transaction,
                                 final String constraintToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newOnCommit
     *        the new value for the <code>on commit value</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setOnCommitValue( final UnitOfWork transaction,
                           final OnCommit newOnCommit ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newPrimaryKeyName
     *        the name of the new <code>primary key</code> child (cannot be empty)
     * @return the new primary key (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    PrimaryKey setPrimaryKey( final UnitOfWork transaction,
                              final String newPrimaryKeyName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newQueryExpression
     *        the new value for the <code>query expression</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setQueryExpression( final UnitOfWork transaction,
                             final String newQueryExpression ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newTempType
     *        the new value for the <code>temporary table type</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setTemporaryTableType( final UnitOfWork transaction,
                                final TemporaryType newTempType ) throws KException;

}
