/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.spi.repository;

/**
 * A repository is a data store containing artifacts
 * generated while modelling VDBs
 */
public interface IRepository {

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
