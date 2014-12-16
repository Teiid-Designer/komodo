/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

/**
 * Represents a relational model primary key.
 */
public interface PrimaryKey extends TableConstraint {

    /**
     * The constraint type for a primary key. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.PRIMARY_KEY;

}
