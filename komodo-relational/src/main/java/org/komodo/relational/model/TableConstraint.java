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
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.ddl.DdlConstants;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlConstants;

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
         * @return the Teiid value (never empty)
         */
        public String toValue() {
            return this.type;
        }

    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnToAdd
     *        the column being added (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void addColumn( final UnitOfWork transaction,
                    final Column columnToAdd ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the parent <code>table</code> (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Table getTable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnToRemove
     *        the column being removed (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void removeColumn( final UnitOfWork transaction,
                       final Column columnToRemove ) throws KException;

}
