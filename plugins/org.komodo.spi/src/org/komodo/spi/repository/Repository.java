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
         * @return the repository name (cannot be empty)
         */
        String getName();

        /**
         * @return the repository URL (cannot be empty)
         */
        String getUrl();
    }

    /**
     * Library and workspace searches using keywords will use one of these criteria.
     */
    public enum KeywordCriteria {

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
     * Represents one or more operations grouped together forming a {@link Repository repository} transaction.
     */
    public interface UnitOfWork {

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
    public interface UnitOfWorkListener {

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
     * Creates then commits a transaction containing only this operation.
     *
     * @param parentPath
     *        the parent path where the workspace object is created (can be empty if adding at the root of the workspace)
     * @param name
     *        the name of the object (cannot be empty)
     * @return the new workspace object (never <code>null</code>)
     * @throws KException
     *         if the parent path does not exist or an error occurs
     */
    public KomodoObject add( final String parentPath,
                             final String name ) throws KException;

    /**
     * The transaction must be committed or rolled back by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param parentPath
     *        the parent path where the workspace object is created (can be empty if adding at the root of the workspace)
     * @param name
     *        the name of the object (cannot be empty)
     * @return the new workspace object (never <code>null</code>)
     * @throws KException
     *         if the parent path does not exist or an error occurs
     */
    public KomodoObject add( final UnitOfWork transaction,
                             final String parentPath,
                             final String name ) throws KException;

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
    public UnitOfWork createTransaction( final String name,
                                         final boolean rollbackOnly,
                                         final UnitOfWorkListener callback ) throws KException;

    /**
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
    public ArtifactDescriptor[] find( final List< String > keywords,
                                      final KeywordCriteria criteria,
                                      final String... artifactTypes ) throws KException;

    /**
     * @param artifactTypes
     *        the artifact type(s) (can be empty if all artifacts of any type are wanted)
     * @return the requested artifacts (never <code>null</code> but can be empty)
     * @throws KException
     *         if parent path does not exist or an error occurs
     */
    public ArtifactDescriptor[] find( final String... artifactTypes ) throws KException;

    /**
     * @param parentPath
     *        the path to the workspace container whose contents are being requested (can be empty if the workspace roots are
     *        being requested)
     * @return the requested workspace Komodo objects (never <code>null</code> but can be empty)
     * @throws Exception
     *         if an error occurs
     */
    public KomodoObject[] get( final String parentPath ) throws Exception;

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
     * Creates then commits a transaction containing only this operation.
     *
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
    public KomodoObject importFile( final File file,
                                    final String name,
                                    final String parentPath ) throws KException;

    /**
     * The transaction must be committed or rolled back by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
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
    public KomodoObject importFile( final UnitOfWork transaction,
                                    final File file,
                                    final String name,
                                    final String parentPath ) throws KException;

    /**
     * The transaction must be committed or rolled back by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
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
    public KomodoObject importResource( final UnitOfWork transaction,
                                        final URL url,
                                        final String name,
                                        final String parentPath ) throws KException;

    /**
     * Creates then commits a transaction containing only this operation.
     *
     * @param url
     *        the resource being added to the workspace (cannot be <code>null</code>)
     * @param name
     *        the name of the Komodo object to create (cannot be empty)
     * @param parentPath
     *        the path to where the Komodo object will be created (can be empty if creating at the workspace root)
     * @return the Komodo object for the resource (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public KomodoObject importResource( final URL url,
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
     * Creates then commits a transaction containing only this operation.
     *
     * @param overwrite
     *        <code>true</code> if existing artifacts should be updated
     * @param descriptor
     *        the artifact descriptor (cannot be <code>null</code>)
     * @param komodoObject
     *        the Komodo object being added to the library (cannot be <code>null</code>)
     * @throws KException
     *         if artifact already exists and not in overwrite mode or an error occurs
     */
    public void publish( final boolean overwrite,
                         final ArtifactDescriptor descriptor,
                         final KomodoObject komodoObject ) throws KException;

    /**
     * The transaction must be committed or rolled back by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param overwrite
     *        <code>true</code> if existing artifacts should be updated
     * @param descriptor
     *        the artifact descriptor (cannot be <code>null</code>)
     * @param komodoObject
     *        the Komodo object being added to the library (cannot be <code>null</code>)
     * @throws KException
     *         if artifact already exists and not in overwrite mode or an error occurs
     */
    public void publish( final UnitOfWork transaction,
                         final boolean overwrite,
                         final ArtifactDescriptor descriptor,
                         final KomodoObject komodoObject ) throws KException;

    /**
     * Creates then commits a transaction containing only this operation.
     *
     * @param paths
     *        the paths of the workspace objects being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if a workspace path does not exist or an error occurs
     */
    public void remove( final String... paths ) throws KException;

    /**
     * The transaction must be committed or rolled back by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param paths
     *        the paths of the workspace objects being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if a workspace path does not exist or an error occurs
     */
    public void remove( final UnitOfWork transaction,
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
     * @param artifactPaths
     *        the paths of the artifacts being requested (cannot be <code>null</code> or empty)
     * @return the requested artifacts (never <code>null</code>)
     * @throws KException
     *         if an artifact does not exist or an error occurs
     */
    public Artifact[] retrieve( final String... artifactPaths ) throws KException;

    /**
     * Creates then commits a transaction containing only this operation.
     *
     * @param artifactPaths
     *        the paths of the the artifacts being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an artifact does not exist in the library or an error occurs
     */
    public void unpublish( final String... artifactPaths ) throws KException;

    /**
     * The transaction must be committed or rolled back by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param artifactPaths
     *        the paths of the the artifacts being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an artifact does not exist in the library or an error occurs
     */
    public void unpublish( final UnitOfWork transaction,
                           final String... artifactPaths ) throws KException;

}
