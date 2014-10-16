/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.core;

/**
 * A Komodo repository.
 */
public interface KRepository {

    /**
     * The repository state.
     */
    public enum State {

        /**
         * The repository cannot be reached.
         */
        NOT_REACHABLE,

        /**
         * The repository can be communicated with.
         */
        REACHABLE,

        /**
         * Reaching the repository has not be tried.
         */
        UNKNOWN

    }

    /**
     * The repository type.
     */
    public enum Type {

        /**
         * The local workspace repository.
         */
        LOCAL,

        /**
         * A shared repository.
         */
        SHARED

    }

    /**
     * A repository identifier.
     */
    public interface Id {

        /**
         * @return the repository name (cannot be empty)
         */
        String getName();

        /**
         * @return the repository URL (cannot be empty)
         */
        String getUrl();

    }

    /**
     * @return the repository identifier (never <code>null</code>)
     */
    Id getId();

    /**
     * @return the repository's running state (never <code>null</code>)
     */
    State getState();

    /**
     * @return the repository's type (never <code>null</code>)
     */
    Type getType();

    /**
     * @return <code>true</code> if the repository can be communicated with
     */
    boolean ping();

}
