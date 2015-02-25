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
 * Represents a relational model primary key.
 */
public interface PrimaryKey extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = PrimaryKey.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.PRIMARY_KEY;

    /**
     * The constraint type for a primary key. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.PRIMARY_KEY;

}
