/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.core;

/**
 * A listener for Komodo-related events.
 */
public interface KListener {

    /**
     * @return the unique listener identifier (cannot be empty)
     */
    String getId();

    /**
     * Exceptions should not be thrown by the listener.
     * @param event the event being processed (never <code>null</code>)
     */
    void process(final KEvent event);

}
