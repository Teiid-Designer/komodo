/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.core.event;

import org.komodo.core.KEngine;
import org.komodo.spi.repository.Repository;

/**
 * A Komodo event.
 *
 * @param <T> class of the source object
 */
public class KEvent<T> {

    /**
     * Events types
     */
    public enum Type {
        /**
         * repository added
         */
        REPOSITORY_ADDED,

        /**
         * repository removed
         */
        REPOSITORY_REMOVED,

        /**
         * engine started
         */
        ENGINE_STARTED,

        /**
         * engine shutdown
         */
        ENGINE_SHUTDOWN
    }

    private final T source;
    private final Type type;

    /**
     * Constructor
     * @param source event source
     * @param type event type
     */
    public KEvent(T source, Type type) {
        this.source = source;
        this.type = type;
    }

    /**
     * @return the event source (never <code>null</code>)
     */
    public T getSource() {
        return source;
    }

    /**
     * @return the event type (never <code>null</code>)
     */
    public Type getType() {
        return type;
    }

    /**
     * @param repository added
     * @return repository added event
     */
    public static KEvent repositoryAddedEvent(Repository repository) {
        return new KEvent<Repository>(repository, Type.REPOSITORY_ADDED);
    }

    /**
     * @param repository removed
     * @return repository removed event
     */
    public static KEvent repositoryRemovedEvent(Repository repository) {
        return new KEvent<Repository>(repository, Type.REPOSITORY_REMOVED);
    }

    /**
     * @return engine started event
     */
    public static KEvent engineStartedEvent() {
        return new KEvent<KEngine>(KEngine.getInstance(), Type.ENGINE_STARTED);
    }

    /**
     * @return engine shutdown event
     */
    public static KEvent engineShutdownEvent() {
        return new KEvent<KEngine>(KEngine.getInstance(), Type.REPOSITORY_ADDED);
    }
}
