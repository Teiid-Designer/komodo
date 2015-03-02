/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.model;

/**
 * Represents a relational model procedure (stored and virtual).
 */
public interface Procedure extends AbstractProcedure {

    /**
     * An empty array of procedures.
     */
    Procedure[] NO_PROCEDURES = new Procedure[0];

}
