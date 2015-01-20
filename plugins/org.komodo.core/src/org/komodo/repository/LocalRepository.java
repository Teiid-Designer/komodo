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
package org.komodo.repository;

import java.net.URL;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.jcr.Session;
import org.komodo.repository.internal.ModeshapeEngineThread;
import org.komodo.repository.internal.ModeshapeEngineThread.Request;
import org.komodo.repository.internal.ModeshapeEngineThread.RequestCallback;
import org.komodo.repository.internal.ModeshapeEngineThread.RequestType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

/**
 * A repository installed on the local machine, using the modeshape engine and repository.
 */
public class LocalRepository extends RepositoryImpl {

    private static String LOCAL_REPOSITORY_CONFIG = "local-repository-config.json"; //$NON-NLS-1$

    private static LocalRepository instance;

    private static class LocalRepositoryId implements Id {

        private final URL configPath;
        private final String workspaceName;

        private LocalRepositoryId(final String configPath,
                                  final String workspaceName ) {
            this.configPath = LocalRepository.class.getResource(configPath);
            this.workspaceName = workspaceName;
        }

        private LocalRepositoryId(final URL configPathUrl,
                                  final String workspaceName ) {
            this.configPath = configPathUrl;
            this.workspaceName = workspaceName;
        }

        @Override
        public URL getConfiguration() {
            return configPath;
        }

        @Override
        public String getUrl() {
            return this.configPath.toString();
        }

        @Override
        public String getWorkspaceName() {
            return this.workspaceName;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.configPath == null) ? 0 : this.configPath.hashCode());
            result = prime * result + ((this.workspaceName == null) ? 0 : this.workspaceName.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            LocalRepositoryId other = (LocalRepositoryId)obj;
            if (this.configPath == null) {
                if (other.configPath != null)
                    return false;
            } else if (!this.configPath.equals(other.configPath))
                return false;
            if (this.workspaceName == null) {
                if (other.workspaceName != null)
                    return false;
            } else if (!this.workspaceName.equals(other.workspaceName))
                return false;
            return true;
        }

        @Override
        public String toString() {
            return "LocalRepositoryId [configPath=" + this.configPath + ", workspaceName=" + this.workspaceName + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
    }

    /**
     * @return singleton instance
     */
    public static LocalRepository getInstance() {
        if (instance == null) instance = new LocalRepository();

        return instance;
    }

    private State state = State.UNKNOWN;

    private ModeshapeEngineThread engineThread;

    /**
     * Create an instance if a local repository using the specified configuration file.
     *
     * @param configPathUrl
     *        the URL of the configuration file (cannot be empty)
     * @param workspaceName
     *        the name of the repository workspace in the configuration file (cannot be empty)
     */
    public LocalRepository( final URL configPathUrl,
                            final String workspaceName ) {
        super(Type.LOCAL, new LocalRepositoryId(configPathUrl, workspaceName));
    }

    /**
     * Create shared instance of local repository using default configuration file and workspace name.
     */
    private LocalRepository() {
        super(Type.LOCAL, new LocalRepositoryId(LOCAL_REPOSITORY_CONFIG, DEFAULT_LOCAL_WORKSPACE_NAME));
    }

    @Override
    public State getState() {
        return state;
    }

    @Override
    public boolean ping() {
        return ((this.engineThread != null) && this.engineThread.isEngineStarted());
    }

    private Session createSession() throws KException {
        final CountDownLatch latch = new CountDownLatch(1);

        class CreateSessionCallback implements RequestCallback {

            private Throwable error = null;
            private Session result = null;

            @Override
            public void errorOccurred( final Throwable e ) {
                this.error = e;
            }

            Throwable getError() {
                return this.error;
            }

            Session getSession() {
                return this.result;
            }

            @Override
            public void respond( final Object results ) {
                this.result = (Session)results;
                latch.countDown();
            }

        }

        final CreateSessionCallback callback = new CreateSessionCallback();
        this.engineThread.accept(new Request(RequestType.CREATE_SESSION, callback));

        boolean timeout = false;

        try {
            timeout = !latch.await(5, TimeUnit.SECONDS);
        } catch (final Exception e) {
            throw new KException(e);
        }

        if (timeout) {
            throw new KException(Messages.getString(Messages.LocalRepository.Unable_To_Create_Session));
        }

        if (callback.getError() != null) {
            throw new KException(callback.getError());
        }

        return callback.getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#createTransaction(java.lang.String, boolean,
     *      org.komodo.spi.repository.Repository.UnitOfWorkListener)
     */
    @Override
    public UnitOfWork createTransaction( final String name,
                                         final boolean rollbackOnly,
                                         final UnitOfWorkListener callback ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        LOGGER.debug("creating transaction '{0}' with rollbackOnly = {1}", name, rollbackOnly); //$NON-NLS-1$
        return new LocalRepositoryTransaction(name, createSession(), rollbackOnly, callback);
    }

    class LocalRepositoryTransaction extends RepositoryImpl.UnitOfWorkImpl {

        LocalRepositoryTransaction( final String uowName,
                                    final Session uowSession,
                                    final boolean uowRollbackOnly,
                                    final UnitOfWorkListener listener ) {
            super(uowName, uowSession, uowRollbackOnly, listener);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.repository.RepositoryImpl.UnitOfWorkImpl#commit()
         */
        @Override
        public void commit() {
            if (isRollbackOnly()) {
                rollback();
            } else {
                final CountDownLatch latch = new CountDownLatch(1);

                class CommitCallback implements RequestCallback {

                    @Override
                    public void errorOccurred( final Throwable error ) {
                        if (getCallback() == null) {
                            KLog.getLogger().error(error.getMessage(), error);
                            return; // No callback so error can only be logged
                        }

                        getCallback().errorOccurred(error);
                    }

                    @Override
                    public void respond( final Object results ) {
                        latch.countDown();
                    }

                }

                final CommitCallback callback = new CommitCallback();
                LocalRepository.this.engineThread.accept(new ModeshapeEngineThread.SessionRequest(RequestType.COMMIT_SESSION,
                                                                                                  callback, getSession(),
                                                                                                  getName()));

                boolean noTimeout = false;

                try {
                    noTimeout = latch.await(3, TimeUnit.MINUTES);
                } catch (final Exception e) {
                    callback.equals(e);
                }

                if (noTimeout) {
                    callback.respond(null);
                } else {
                    callback.errorOccurred(new KException(Messages.getString(Messages.LocalRepository.Commit_Timeout, getName())));
                }
            }
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.repository.RepositoryImpl.UnitOfWorkImpl#rollback()
         */
        @Override
        public void rollback() {
            final CountDownLatch latch = new CountDownLatch(1);

            class RollbackCallback implements RequestCallback {

                @Override
                public void errorOccurred( final Throwable error ) {
                    if (getCallback() == null) {
                        KLog.getLogger().error(error.getMessage(), error);
                        return; // No callback so error can only be logged
                    }

                    getCallback().errorOccurred(error);
                }

                @Override
                public void respond( final Object results ) {
                    latch.countDown();
                }

            }

            final RollbackCallback callback = new RollbackCallback();
            LocalRepository.this.engineThread.accept(new ModeshapeEngineThread.SessionRequest(RequestType.ROLLBACK_SESSION,
                                                                                              callback,
                                                                                              getSession(),
                                                                                              getName()));

            boolean noTimeout = false;

            try {
                noTimeout = latch.await(3, TimeUnit.MINUTES);
            } catch (final Exception e) {
                callback.equals(e);
            }

            if (noTimeout) {
                callback.respond(null);
            } else {
                callback.errorOccurred(new KException(Messages.getString(Messages.LocalRepository.Rollback_Timeout, getName())));
            }
        }

    }

    private void createEngineThread() {
        if (engineThread != null && engineThread.isAlive()) return;

        if (engineThread != null && !engineThread.isAlive()) throw new RuntimeException(
                                                                                        Messages.getString(Messages.LocalRepository.EngineThread_Died));

        engineThread = new ModeshapeEngineThread(getId().getConfiguration(), getId().getWorkspaceName());
        engineThread.start();
    }

    private void startRepository() {
        if (this.state == State.REACHABLE) return;

        createEngineThread();

        RequestCallback callback = new RequestCallback() {

            @Override
            public void errorOccurred( final Throwable error ) {
                throw new RuntimeException(error);
            }

            @Override
            public void respond( final Object results ) {
                if (engineThread.isEngineStarted()) {
                    LocalRepository.this.state = State.REACHABLE;
                    notifyObservers();
                }
            }
        };

        engineThread.accept(new Request(RequestType.START, callback));
    }

    @Override
    public void notify( RepositoryClientEvent event ) {
        if (event.getType() == RepositoryClientEvent.EventType.STARTED) {
            // Start the modeshape engine if not already started
            startRepository();
        } else if (event.getType() == RepositoryClientEvent.EventType.SHUTTING_DOWN) {
            stopRepository();
        }
    }

    private void stopRepository() {
        RequestCallback callback = new RequestCallback() {

            /**
             * {@inheritDoc}
             *
             * @see org.komodo.repository.internal.ModeshapeEngineThread.RequestCallback#errorOccurred(java.lang.Throwable)
             */
            @Override
            public void errorOccurred( final Throwable error ) {
                throw new RuntimeException(error);
            }

            /**
             * {@inheritDoc}
             *
             * @see org.komodo.repository.internal.ModeshapeEngineThread.RequestCallback#respond(java.lang.Object)
             */
            @Override
            public void respond( final Object results ) {
                if (!engineThread.isEngineStarted()) {
                    LocalRepository.this.state = State.NOT_REACHABLE;
                    notifyObservers();
                }
            }
        };

        this.engineThread.accept(new Request(RequestType.STOP, callback));
    }

}
