/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.core;

/**
 * A Komodo event.
 */
public interface KEvent {

    // TODO implement KEvent interface

    /**
     * Events types can be enums that extend this interface.
     */
    interface Type {
        // nothing to do
    }

    /**
     * @return the event source (never <code>null</code>)
     */
    Object getSource();

    /**
     * @return the event type (never <code>null</code>)
     */
    Type getType();

}
