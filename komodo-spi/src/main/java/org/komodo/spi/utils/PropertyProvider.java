/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.utils;

import java.beans.PropertyChangeListener;

/**
 * Provides properties and notifies registered listeners of property changes.
 */
public interface PropertyProvider extends PropertyChangeListener {

    /**
     * @param listener
     *        the listener being added (cannot be <code>null</code>)
     * @return <code>true</code> if the listener was added
     */
    boolean addPropertyChangeListener( final PropertyChangeListener listener );

    /**
     * @param propertyName
     *        the name of the property whose value is being requested (can be empty)
     * @return the property value (can be <code>null</code> if the property name is empty, if the value is <code>null</code>, or
     *         if the property does not exist)
     */
    Object getProperty( final String propertyName );

    /**
     * @param propertyName
     *        the name of the property whose existence is being checked (can be empty)
     * @return <code>true</code> if the property name is not empty, the property exists, and the property has a non-
     *         <code>null</code> value
     */
    boolean hasProperty( final String propertyName );

    /**
     * @param listener
     *        the listener being removed (can be <code>null</code>)
     * @return <code>true</code> if the listener was removed
     */
    boolean removePropertyChangeListener( final PropertyChangeListener listener );

}
