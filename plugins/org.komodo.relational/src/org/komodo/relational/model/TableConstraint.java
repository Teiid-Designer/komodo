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
import org.modeshape.sequencer.ddl.DdlConstants;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants;

/**
 * Represents a relational model table constraint.
 */
public interface TableConstraint extends RelationalObject {

    /**
     * The types of table constraints.
     */
    enum ConstraintType {

        ACCESS_PATTERN( TeiidDdlConstants.TeiidNonReservedWord.ACCESSPATTERN.toDdl() ),
        FOREIGN_KEY( DdlConstants.FOREIGN_KEY ),
        INDEX( TeiidDdlConstants.TeiidNonReservedWord.INDEX.toDdl() ),
        PRIMARY_KEY( DdlConstants.PRIMARY_KEY ),
        UNIQUE( TeiidDdlConstants.TeiidReservedWord.UNIQUE.toDdl() );

        final String type;

        private ConstraintType( final String constraintType ) {
            this.type = constraintType;
        }

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return this.type;
        }

    }

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param columnToAdd
     *        the column being added (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void addColumn( final UnitOfWork transaction,
                    final Column columnToAdd ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the columns contained in this key (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Column[] getColumns( final UnitOfWork transaction ) throws KException;

    /**
     * @return the constraint type (never <code>null</code>)
     */
    ConstraintType getConstraintType();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the parent <code>table</code> (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Table getTable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param columnToRemove
     *        the column being removed (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void removeColumn( final UnitOfWork transaction,
                       final Column columnToRemove ) throws KException;

}
