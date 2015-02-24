/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.repository.KomodoType;

/**
 * Represents a relational model unique constraint.
 */
public interface UniqueConstraint extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = UniqueConstraint.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.UNIQUE_CONSTRAINT;

    /**
     * The constraint type for a unique constraint. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.UNIQUE;

    /**
     * An empty collection of unique constraints.
     */
    UniqueConstraint[] NO_UNIQUE_CONSTRAINTS = new UniqueConstraint[0];

}
