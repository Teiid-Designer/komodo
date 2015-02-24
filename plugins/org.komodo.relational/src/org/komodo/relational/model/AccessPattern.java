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
 * Represents a relational model access pattern.
 */
public interface AccessPattern extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = AccessPattern.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.ACCESS_PATTERN;

    /**
     * The constraint type for an access pattern. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.ACCESS_PATTERN;

    /**
     * An empty collection of access pattern constraints.
     */
    AccessPattern[] NO_ACCESS_PATTERNS = new AccessPattern[0];

}
