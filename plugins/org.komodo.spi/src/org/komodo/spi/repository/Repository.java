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

import java.io.File;
import java.net.URL;
import java.util.List;
import org.komodo.spi.KException;

/**
 * A repository is a data store containing artifacts generated while modeling VDBs
 */
public interface Repository {

    /**
     * A repository identifier.
     */
    public interface Id {

        /**
         * @return the repository configuration location
         */
        URL getConfiguration();

        /**
         * @return the repository URL (cannot be empty)
         */
        String getUrl();

        /**
         * @return the repository workspace name (cannot be empty)
         */
        String getWorkspaceName();

    }

    /**
     * Library and workspace searches using keywords will use one of these criteria.
     */
    enum KeywordCriteria {

        /**
         * All keywords must be present in the search result.
         */
        ALL,

        /**
         * At least one of the keywords must be present in the search result. This is the default.
         */
        ANY,

        /**
         * None of the keywords may be present in the search result.
         */
        NONE;

        /**
         * @return the default keyword to use (never <code>null</code>)
         */
        public static KeywordCriteria getDefault() {
            return ANY;
        }

    }

    /**
     * The repository state.
     */
    enum State {

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
    enum Type {

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
     * Represents one or more operations grouped together forming a {@link Repository repository} transaction.
     */
    interface UnitOfWork {

        /**
         * Saves all changes made during the transaction. If this is a roll back transaction then {@link #rollback()} is called.
         */
        void commit();

        /**
         * @return the listener being notified when the transaction is finished (can be <code>null</code>)
         */
        UnitOfWorkListener getCallback();

        /**
         * @return the name of the transaction (never <code>null</code>)
         */
        String getName();

        /**
         * @return <code>true</code> if only rollback is allowed
         */
        boolean isRollbackOnly();

        /**
         * Discards all current changes made during this transaction.
         */
        void rollback();

    }

    /**
     * A listener notified when a unit of work completes.
     */
    interface UnitOfWorkListener {

        /**
         * @param error
         *        the error that occurred processing the transaction (never <code>null</code>)
         */
        void errorOccurred( final Throwable error );

        /**
         * @param results
         *        the results of the work (can be <code>null</code>)
         */
        void respond( final Object results );

    }

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param parentPath
     *        the parent path where the workspace object is created (can be empty if adding at the root of the workspace)
     * @param name
     *        the name of the object (cannot be empty)
     * @param primaryType
     *        the primary type of the child or <code>null</code> if type is <code>nt:unstructured</code>
     * @return the new workspace object (never <code>null</code>)
     * @throws KException
     *         if the parent path does not exist or an error occurs
     */
    KomodoObject add( final UnitOfWork transaction,
                      final String parentPath,
                      final String name,
                      final String primaryType ) throws KException;

    /**
     * Add an {@link RepositoryClient} to receive notifications from
     *
     * @param client
     */
    void addClient( RepositoryClient client );

    /**
     * @param observer
     *        the observer to be added
     */
    void addObserver( RepositoryObserver observer );

    /**
     * @param name
     *        a name for the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if the transaction should only be rolled back
     * @param callback
     *        a listener that is notified when the transaction is finished (can be <code>null</code>
     * @return a unit of work transaction that must be either committed or rolled back (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    UnitOfWork createTransaction( final String name,
                                  final boolean rollbackOnly,
                                  final UnitOfWorkListener callback ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param keywords
     *        words that should be matched to words found in the description (can be empty)
     * @param criteria
     *        the search criteria (can be <code>null</code> if the default criteria should be used)
     * @param artifactTypes
     *        the artifact type(s) (can be empty if all artifacts of any type are wanted)
     * @return the requested artifacts (never <code>null</code> but can be empty)
     * @throws KException
     *         if parent path does not exist or an error occurs
     */
    ArtifactDescriptor[] find( final UnitOfWork transaction,
                               final List< String > keywords,
                               final KeywordCriteria criteria,
                               final String... artifactTypes ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param artifactTypes
     *        the artifact type(s) (can be empty if all artifacts of any type are wanted)
     * @return the requested artifacts (never <code>null</code> but can be empty)
     * @throws KException
     *         if parent path does not exist or an error occurs
     */
    ArtifactDescriptor[] find( final UnitOfWork transaction,
                               final String... artifactTypes ) throws KException;

    /**
     * Get an object from the workspace part of the repository.
     *
     * The path can be workspace relative or absolute.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param path
     *        the path to the workspace object being requested (can be empty if the workspace root)
     * @return the requested workspace Komodo object (can be <code>null</code> if it does not exist)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject getFromWorkspace( final UnitOfWork transaction,
                                   final String path ) throws KException;

    /**
     * Gets the {@link KomodoObject} with the specified identifier.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param jcrUuid
     *        the value of the <code>jcr:uuid</code> property (cannot be empty)
     * @return the requested workspace Komodo object or <code>null</code> if not found
     * @throws KException
     *         if an error occurs
     */
    KomodoObject getUsingId( final UnitOfWork transaction,
                             final String jcrUuid ) throws KException;

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
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param file
     *        the file being added to the workspace (cannot be <code>null</code>)
     * @param name
     *        the name of the Komodo object to create (cannot be empty)
     * @param parentPath
     *        the path to where the object will be created (can be empty if creating at the workspace root)
     * @return the Komodo object for the file (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject importFile( final UnitOfWork transaction,
                             final File file,
                             final String name,
                             final String parentPath ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param url
     *        the resource being added to the workspace (cannot be <code>null</code>)
     * @param name
     *        the name of the Komodo object to create (cannot be empty)
     * @param parentPath
     *        the path to where the Komodo object will be created (can be empty if importing to the root of the workspace)
     * @return the Komodo object for the resource (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject importResource( final UnitOfWork transaction,
                                 final URL url,
                                 final String name,
                                 final String parentPath ) throws KException;

    /**
     * Notify the repository of the given {@link RepositoryClientEvent}
     *
     * @param event
     */
    void notify( RepositoryClientEvent event );

    /**
     * @return <code>true</code> if the repository can be communicated with
     */
    boolean ping();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param overwrite
     *        <code>true</code> if existing artifacts should be updated
     * @param descriptor
     *        the artifact descriptor (cannot be <code>null</code>)
     * @param komodoObject
     *        the Komodo object being added to the library (cannot be <code>null</code>)
     * @throws KException
     *         if artifact already exists and not in overwrite mode or an error occurs
     */
    void publish( final UnitOfWork transaction,
                  final boolean overwrite,
                  final ArtifactDescriptor descriptor,
                  final KomodoObject komodoObject ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param paths
     *        the paths of the workspace objects being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if a workspace path does not exist or an error occurs
     */
    void remove( final UnitOfWork transaction,
                 final String... paths ) throws KException;

    /**
     * Remove an {@link RepositoryClient} that we no longer wish to receive notifications from
     *
     * @param client
     */
    void removeClient( RepositoryClient client );

    /**
     * @param observer
     *        the observer to be removed
     */
    void removeObserver( RepositoryObserver observer );

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param artifactPaths
     *        the paths of the artifacts being requested (cannot be <code>null</code> or empty)
     * @return the requested artifacts (never <code>null</code>)
     * @throws KException
     *         if an artifact does not exist or an error occurs
     */
    Artifact[] retrieve( final UnitOfWork transaction,
                         final String... artifactPaths ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     * @param artifactPaths
     *        the paths of the the artifacts being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an artifact does not exist in the library or an error occurs
     */
    void unpublish( final UnitOfWork transaction,
                    final String... artifactPaths ) throws KException;

    /**
     * The komodo library in the repository, ie. /tko:komodo/tko:library
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     *
     * @return the komodo library
     * @throws KException if an error occurs
     */
    KomodoObject komodoLibrary( final UnitOfWork transaction) throws KException;

    /**
     * The komodo workspace in the repository, ie. /tko:komodo/tko:workspace
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     *
     * @return the komodo workspace
     * @throws KException if an error occurs
     */
    KomodoObject komodoWorkspace( final UnitOfWork transaction) throws KException;
}
