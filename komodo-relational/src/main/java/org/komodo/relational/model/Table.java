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

import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.model.internal.TableImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;

/**
 * Represents a relational model table.
 */
public interface Table extends Exportable, OptionContainer, RelationalObject, SchemaElement {

    /**
     * The type identifier.
     */
    int TYPE_ID = Table.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.TABLE;

    /**
     * The on commit value.
     */
    public enum OnCommit {

        /**
         * Delete rows on commit.
         */
        DELETE_ROWS( "DELETE ROWS" ), //$NON-NLS-1$

        /**
         * Preserve rows on commit.
         */
        PRESERVE_ROWS( "PRESERVE ROWS" ); //$NON-NLS-1$

        /**
         * @param value
         *        the value whose <code>OnCommit</code> is being requested (can be empty)
         * @return the corresponding <code>OnCommit</code> or <code>null</code> if not found
         */
        public static OnCommit fromValue( final String value ) {
            if (DELETE_ROWS.value.equals(value)) {
                return DELETE_ROWS;
            }

            if (PRESERVE_ROWS.value.equals(value)) {
                return PRESERVE_ROWS;
            }

            return null;
        }

        private final String value;

        private OnCommit( final String value ) {
            this.value = value;
        }

        /**
         * @return the Teiid value (cannot be empty)
         */
        public String toValue() {
            return this.value;
        }

    }

    /**
     * Temporary table types.
     */
    public enum TemporaryType {

        /**
         * A globally-scoped temporary table.
         */
        GLOBAL,

        /**
         * A locally-scoped temporary table.
         */
        LOCAL;

        /**
         * @param value
         *        the value whose <code>TemporaryType</code> is being requested (can be empty)
         * @return the corresponding <code>TemporaryType</code> or <code>null</code> if not found
         */
        public static TemporaryType fromValue( final String value ) {
            if (GLOBAL.name().equals(value)) {
                return GLOBAL;
            }

            if (LOCAL.name().equals(value)) {
                return LOCAL;
            }

            return null;
        }

    }

    /**
     * The default value of this table's cardinality. Value is {@value} .
     */
    long DEFAULT_CARDINALITY = -1;

    /**
     * The default value indicating if this table is materialized. Value is {@value} .
     */
    boolean DEFAULT_MATERIALIZED = false;

    /**
     * The default value indicating if this table is updatable. Value is {@value} .
     */
    boolean DEFAULT_UPDATABLE = true;

    /**
     * An empty array of tables.
     */
    Table[] NO_TABLES = new Table[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Model getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link Table}.
     */
    TypeResolver< Table > RESOLVER = new TypeResolver< Table >() {

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
        public Class< TableImpl > owningClass() {
            return TableImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateTable.TABLE_STATEMENT );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Table resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Table.TYPE_ID ) {
                return ( Table )kobject;
            }

            return new TableImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the access patterns (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    AccessPattern[] getAccessPatterns( final UnitOfWork transaction,
                                       final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the cardinality
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CARDINALITY
     */
    long getCardinality( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the columns (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Column[] getColumns( final UnitOfWork transaction,
                         final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the foreign keys (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    ForeignKey[] getForeignKeys( final UnitOfWork transaction,
                                 final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the indexes (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Index[] getIndexes( final UnitOfWork transaction,
                        final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the materialized table name (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getMaterializedTable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>name in source</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNameInSource( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the on commit value (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    OnCommit getOnCommitValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the primary key (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    PrimaryKey getPrimaryKey( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the query expression (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getQueryExpression( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the temporary table type or <code>null</code> if not a temporary table
     * @throws KException
     *         if an error occurs
     */
    TemporaryType getTemporaryTableType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the unique constraints (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    UniqueConstraint[] getUniqueConstraints( final UnitOfWork transaction,
                                             final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>UUID</code> option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getUuid( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this is a materialized table
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_MATERIALIZED
     */
    boolean isMaterialized( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this table is updatable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    boolean isUpdatable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param accessPatternToRemove
     *        the name of the access pattern being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeAccessPattern( final UnitOfWork transaction,
                              final String accessPatternToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnToRemove
     *        the name of the column being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeColumn( final UnitOfWork transaction,
                       final String columnToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param foreignKeyToRemove
     *        the name of the foreign key being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeForeignKey( final UnitOfWork transaction,
                           final String foreignKeyToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param indexToRemove
     *        the name of the index being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeIndex( final UnitOfWork transaction,
                      final String indexToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @throws KException
     *         if an error occurs
     */
    void removePrimaryKey( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param constraintToRemove
     *        the name of the unique constraint being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeUniqueConstraint( final UnitOfWork transaction,
                                 final String constraintToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCardinality
     *        the new value for the <code>cardinality</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CARDINALITY
     */
    void setCardinality( final UnitOfWork transaction,
                         long newCardinality ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newMaterialized
     *        the new value for the <code>materialized</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_MATERIALIZED
     */
    void setMaterialized( final UnitOfWork transaction,
                          final boolean newMaterialized ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newMaterializedTable
     *        the new value for the <code>materialized table</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setMaterializedTable( final UnitOfWork transaction,
                               final String newMaterializedTable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNameInSource
     *        the new name in source (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNameInSource( final UnitOfWork transaction,
                          final String newNameInSource ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newOnCommit
     *        the new value for the <code>on commit value</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setOnCommitValue( final UnitOfWork transaction,
                           final OnCommit newOnCommit ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newQueryExpression
     *        the new value for the <code>query expression</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setQueryExpression( final UnitOfWork transaction,
                             final String newQueryExpression ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newTempType
     *        the new value for the <code>temporary table type</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setTemporaryTableType( final UnitOfWork transaction,
                                final TemporaryType newTempType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newUpdatable
     *        the new value for the <code>updatable</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    void setUpdatable( final UnitOfWork transaction,
                       final boolean newUpdatable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newUuid
     *        the new value of the <code>UUID</code> option (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setUuid( final UnitOfWork transaction,
                  final String newUuid ) throws KException;

}
