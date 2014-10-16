/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.core;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

/**
 * The Komodo engine. It is responsible for persisting and retriever user session data and Teiid artifacts.
 */
public final class KEngine implements Iterable<KRepository> {

    /**
     * The engine state.
     */
    public enum State {

        /**
         * The initial state.
         */
        NOT_STARTED,

        /**
         * Engine has been successfully started.
         */
        STARTED,

        /**
         * Engine has been successfully shutdown.
         */
        SHUTDOWN,

        /**
         * There was an error starting or shutting down the engine.
         */
        ERROR

    }

    private static final String PREFIX = KEngine.class.getSimpleName() + '.';
    private static KEngine _instance;

    /**
     * @return the shared engine (never <code>null</code>)
     */
    public static KEngine getInstance() {
        if (_instance == null) _instance = new KEngine();
        return _instance;
    }

    private final Set<KListener> listeners = new HashSet<KListener>();
    private final Set<KRepository> repos = new HashSet<KRepository>();
    private State state = State.NOT_STARTED;

    private KEngine() {
        // nothing to do
    }

    /**
     * @param listener the listener being registered (cannot be <code>null</code>)
     * @throws KException if the listener was not added
     */
    public void add(final KListener listener) throws KException {
        ArgCheck.isNotNull(listener, "listener"); //$NON-NLS-1$

        if (this.listeners.add(listener)) {
            KLog.getLogger().debug(String.format("%s added listener '{0}'", PREFIX, listener.getId())); //$NON-NLS-1$
        } else {
            // TODO i18n this
            throw new KException(String.format("Listener '%s' was not added", listener.getId())); //$NON-NLS-1$
        }
    }

    /**
     * @param repository the repository being added (cannot be <code>null</code>)
     * @throws KException if the repository was not added
     */
    public void add(final KRepository repository) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$

        if (this.repos.add(repository)) {
            KLog.getLogger().debug(String.format("%s added repository '{0}'", PREFIX, repository.getId())); //$NON-NLS-1$
            notifyListeners(null); // TODO create event
        } else {
            // TODO i18n this
            throw new KException(String.format("Repository '%s' was not added", repository.getId())); //$NON-NLS-1$
        }
    }

    /**
     * @return the registered repositories (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    public KRepository[] getRepositories() throws KException {
        return this.repos.toArray(new KRepository[this.repos.size()]);
    }

    /**
     * @return the engine state (never <code>null</code>)
     */
    public State getState() {
        return this.state;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<KRepository> iterator() {
        final Set<KRepository> copy = new HashSet<KRepository>(this.repos);
        return copy.iterator();
    }

    private void notifyListeners(final KEvent event) throws KException {
        assert (event != null);

        for (final KListener listener : this.listeners) {
            try {
                listener.process(event);
            } catch (final Exception e) {
                remove(listener);
                // TODO i18n this
                KLog.getLogger().error(String.format("%s unregistered listener '{0}' because it threw and exception", PREFIX, listener.getId())); //$NON-NLS-1$
            }
        }
    }

    /**
     * @param listener the listener being unregistered (cannot be <code>null</code>)
     * @throws KException if the listener was not removed
     */
    public void remove(final KListener listener) throws KException {
        ArgCheck.isNotNull(listener, "listener"); //$NON-NLS-1$

        if (this.listeners.remove(listener)) {
            KLog.getLogger().debug(String.format("%s removed listener '{0}'", PREFIX, listener.getId())); //$NON-NLS-1$
        } else {
            // TODO i18n this
            throw new KException(String.format("Listener '%s' was not removed", listener.getId())); //$NON-NLS-1$
        }
    }

    /**
     * @param repository the repository being removed (cannot be <code>null</code>)
     * @throws KException if the repository was not removed
     */
    public void remove(final KRepository repository) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$

        if (this.repos.remove(repository)) {
            KLog.getLogger().debug(String.format("%s removed repository '{0}'", PREFIX, repository.getId())); //$NON-NLS-1$
            notifyListeners(null); // TODO create event
        } else {
            // TODO i18n this
            throw new KException(String.format("Repository '%s' was not removed", repository.getId())); //$NON-NLS-1$
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
            notifyListeners(null); // TODO create event
        } catch (final Exception e) {
            this.state = State.ERROR;
            // TODO i18n this
            throw new KException("Error during KEngine shutdown", e); //$NON-NLS-1$
        }
    }

    /**
     * @throws KException if there is an error starting the engine
     */
    public void start() throws KException {
        try {
            // TODO implement start (add local repo, read any saved session state, connect to repos if auto-connect, etc.)
            this.state = State.STARTED;
            KLog.getLogger().debug("Komodo engine successfully shutdown"); //$NON-NLS-1$
            notifyListeners(null); // TODO create event
        } catch (final Exception e) {
            this.state = State.ERROR;
            // TODO i18n this
            throw new KException("Error during KEngine startup", e); //$NON-NLS-1$
        }
    }

}
