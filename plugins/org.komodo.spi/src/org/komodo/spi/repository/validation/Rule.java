/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.repository.validation;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;

/**
 * A rule used in validation of {@link KomodoObject}'s.
 */
public interface Rule {

    /**
     * @param kobject
     *        the object being evaluated (cannot be <code>null</code>)
     * @return the result (never <code>null</code>)
     * @throws KException
     *         if the object is not of the right type to be evaluated or if an error occurs
     */
    Result evaluate( final KomodoObject kobject ) throws KException;

    /**
     * @return the localized rule description (never empty)
     */
    String getDescription();

    /**
     * @return the unique rule identifier (never empty)
     */
    String getId();

    /**
     * @return the fully qualified JCR property names validated by this rule (never <code>null</code> but can be empty)
     */
    String[] getValidatedProperties();

    /**
     * @return the fully qualified JCR node type validated by this rule (never empty)
     */
    String getValidatedType();

    /**
     * @return <code>true</code> if rule is enabled and should be run
     */
    boolean isEnabled();

}
