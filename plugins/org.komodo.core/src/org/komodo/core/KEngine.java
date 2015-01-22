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
package org.komodo.core;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.komodo.core.event.KEvent;
import org.komodo.core.event.KListener;
import org.komodo.modeshape.lib.LogConfigurator;
import org.komodo.repository.LocalRepository;
import org.komodo.spi.KErrorHandler;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

/**
 * The Komodo engine. It is responsible for persisting and retriever user session data and Teiid artifacts.
 */
public final class KEngine implements RepositoryClient, StringConstants {

    private static KEngine _instance;

    private static final String PREFIX = KEngine.class.getSimpleName() + DOT;

    /**
     * @return the shared engine (never <code>null</code>)
     */
    public static KEngine getInstance() {
        if (_instance == null)
            _instance = new KEngine();

        return _instance;
    }

    private final Set<KListener> listeners = new HashSet<KListener>();

    private final Set<Repository> repositories = new HashSet<Repository>();

    private State state = State.NOT_STARTED;

    private LocalRepository defaultRepository;

    private KomodoErrorHandler errorHandler = new KomodoErrorHandler();

    private KEngine() {
        // Initialise the logging system
        try {
            LogConfigurator.getInstance();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * @return the defaultRepository
     */
    public Repository getDefaultRepository() {
        if (this.defaultRepository == null)
            initDefaultRespository();

        return this.defaultRepository;
    }

    /**
     * @param listener the listener being registered (cannot be <code>null</code>)
     * @throws KException if the listener was not added
     */
    public void add(final KListener listener) throws KException {
        ArgCheck.isNotNull(listener, "listener"); //$NON-NLS-1$

        if (this.listeners.add(listener)) {
            KLog.getLogger().debug(Messages.getString(Messages.KEngine.Added_Listener, PREFIX, listener.getId()));
        } else {
            throw new KException(Messages.getString(Messages.KEngine.Added_Listener_Failure, listener.getId()));
        }
    }

    /**
     * Adds the repository to the engine. Does nothing if repository has already been added
     *
     * @param repository the repository being added (cannot be <code>null</code>)
     * @throws KException if error occurs
     */
    public void add(final Repository repository) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$

        if (this.repositories.add(repository)) {
            repository.addClient(this);
            KLog.getLogger().debug(Messages.getString(Messages.KEngine.Added_Repository, PREFIX, repository.getId().getUrl()));
            notifyListeners(KEvent.repositoryAddedEvent(repository));

            // Notify this repository if it has started
            if (State.STARTED == state)
                repository.notify(RepositoryClientEvent.createStartedEvent(this));

        }
    }

    @Override
    public State getState() {
        return this.state;
    }

    /**
     * @return the registered repositories (never <code>null</code> but can be empty)
     */
    public Set<Repository> getRepositories() {
        return Collections.unmodifiableSet(this.repositories);
    }

    private void notifyListeners(final KEvent event) {
        ArgCheck.isNotNull(event);

        for (final KListener listener : this.listeners) {
            try {
                listener.process(event);
            } catch (final Exception e) {
               getErrorHandler().error(Messages.getString(Messages.KEngine.Notify_Listeners, PREFIX, listener.getId()));
            }
        }
    }

    private void notifyRepositories(final RepositoryClientEvent event) {
        ArgCheck.isNotNull(event);

        for (Repository repository : getRepositories()) {
            repository.notify(event);
        }
    }

    /**
     * @param listener the listener being unregistered (cannot be <code>null</code>)
     * @throws KException if the listener was not removed
     */
    public void remove(final KListener listener) throws KException {
        ArgCheck.isNotNull(listener, "listener"); //$NON-NLS-1$

        if (this.listeners.remove(listener)) {
            KLog.getLogger().debug(Messages.getString(Messages.KEngine.Removed_Listener, PREFIX, listener.getId()));
        } else {
            throw new KException(Messages.getString(Messages.KEngine.Removed_Listener_Failure, listener.getId()));
        }
    }

    /**
     * @param repository the repository being removed (cannot be <code>null</code>)
     *
     * @throws KException if the repository was not removed
     */
    public void remove(final Repository repository) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$

        if (this.repositories.remove(repository)) {
            repository.removeClient(this);
            KLog.getLogger().debug(Messages.getString(Messages.KEngine.Removed_Repository, PREFIX, repository.getId().getUrl()));
            notifyListeners(KEvent.repositoryRemovedEvent(repository));
        } else {
            throw new KException(Messages.getString(Messages.KEngine.Removed_Repository_Failure, repository.getId().getUrl()));
        }
    }

    /**
     * @throws KException if there is an error during engine shutdown
     */
    public void shutdown() throws KException {
        try {
            // TODO implement shutdown (write saved state, disconnect to repos, etc.)
            this.state = State.SHUTDOWN;
            KLog.getLogger().debug("Komodo engine successfully shutdown"); //$NON-NLS-1$

            // Notify any registered repositories that this engine has shutdown
            notifyRepositories(RepositoryClientEvent.createShuttingDownEvent(this));

            // Notify any 3rd-party listeners that this engine has shutdown
            notifyListeners(KEvent.engineShutdownEvent());

        } catch (final Exception e) {
            this.state = State.ERROR;
            throw new KException(Messages.getString(Messages.KEngine.Shutdown_Failure), e);
        }
    }

    /**
     * Initialise the local repository
     */
    private void initDefaultRespository() {
        defaultRepository = LocalRepository.getInstance();
        try {
            add(defaultRepository);
        } catch (Exception ex) {
            getErrorHandler().error(ex);
        }
    }

    /**
     * @throws KException if there is an error starting the engine
     */
    public void start() throws KException {
        try {
            // Initialise the local repository
            initDefaultRespository();

            // TODO implement start (read any saved session state, connect to repos if auto-connect, etc.)
            this.state = State.STARTED;
            KLog.getLogger().debug("Komodo engine successfully started"); //$NON-NLS-1$

            // Notify any registered repositories that this engine has started
            notifyRepositories(RepositoryClientEvent.createStartedEvent(this));

            // Notify any 3rd-party listeners that this engine has started
            notifyListeners(KEvent.engineStartedEvent());

        } catch (final Exception e) {
            this.state = State.ERROR;
            throw new KException(Messages.getString(Messages.KEngine.Startup_Failure), e);
        }
    }

    /**
     * @return the error handler
     */
    public KErrorHandler getErrorHandler() {
        return this.errorHandler;
    }

    /**
     * Sets the error handler implementation of the engine
     *
     * @param errorHandler
     */
    public void addErrorHandler(KErrorHandler errorHandler) {
        this.errorHandler.add(errorHandler);
    }
}
