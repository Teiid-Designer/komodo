/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

import org.komodo.spi.KException;

/**
 * A {@link KomodoObject Komodo object} type definition.
 */
public interface Descriptor {

    /**
     * @return the child {@link KomodoObject Komodo object} {@link Descriptor type descriptors} (never <code>null</code> but can
     *         be empty)
     * @throws KException
     *         if an error occurs
     */
    Descriptor[] getChildDescriptors() throws KException;

    /**
     * @return the type name (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getName() throws KException;

    /**
     * @return the {@link KomodoObject Komodo object's} {@link Property property} {@link PropertyDescriptor descriptors} (never
     *         <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    PropertyDescriptor[] getPropertyDescriptors() throws KException;

}
