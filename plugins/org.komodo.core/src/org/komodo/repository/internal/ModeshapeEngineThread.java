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
package org.komodo.repository.internal;

import java.net.URL;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import javax.jcr.Session;
import org.komodo.repository.Messages;
import org.komodo.spi.KException;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.modeshape.common.collection.Problem;
import org.modeshape.common.collection.Problems;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.RepositoryConfiguration;

/**
 * The thread the ModeShape engine uses for local repositories.
 */
public class ModeshapeEngineThread extends Thread {

    /**
     * Request types to send to this engine thread
     */
    public enum RequestType {

        /**
         * Request to save the specified session.
         *
         * @see SessionRequest
         */
        COMMIT_SESSION,

        /**
         * Request to create a session.
         */
        CREATE_SESSION,

        /**
         * Request to rollback the specified session.
         *
         * @see SessionRequest
         */
        ROLLBACK_SESSION,

        /**
         * Request starting of the engine
         */
        START,

        /**
         * Request stopping of the engine
         */
        STOP;

        static boolean isSessionRequest(final RequestType requestType) {
            return ((requestType == CREATE_SESSION) || (requestType == COMMIT_SESSION) || (requestType == ROLLBACK_SESSION));
        }

    }

    /**
     * Callback interface that can be implemented by third-parties and incorporated into requests to the engine thread.
     */
    public interface RequestCallback {

        /**
         * @param error
         *        the error that occurred (never <code>null</code>)
         */
        void errorOccurred( final Throwable error );

        /**
         * Respond to the request being successfully completed.
         *
         * @param results
         *        the results (can be <code>null</code>)
         */
        void respond( final Object results );

    }

    /**
     * Request crafted by wishing to be notified of a request outcome.
     */
    public static class Request {

        private RequestType requestType;

        private RequestCallback callback;

        /**
         * @param requestType
         *        type of request (cannot be <code>null</code>)
         * @param callback
         *        callback for execution after change of engine state (can be <code>null</code>)
         */
        public Request( RequestType requestType,
                        RequestCallback callback ) {
            ArgCheck.isNotNull(requestType, "requestType"); //$NON-NLS-1$
            this.requestType = requestType;
            this.callback = callback;
        }

        /**
         * @return the requestType
         */
        public RequestType getRequestType() {
            return this.requestType;
        }

        /**
         * @return the callback
         */
        public RequestCallback getCallback() {
            return this.callback;
        }
    }

    /**
     * A request to save or rollback a session.
     */
    public static class SessionRequest extends Request {

        private final String name;
        private final Session session;

        /**
         * @param requestType
         *        the request type (cannot be <code>null</code> and must be a {@link SessionRequest session request}.
         * @param callback
         *        the callback (can be <code>null</code>)
         * @param requestSession
         *        the session the request pertains to (cannot be <code>null</code>)
         * @param requestName
         *        a name given to the request for logging purposes
         */
        public SessionRequest( final RequestType requestType,
                               final RequestCallback callback,
                               final Session requestSession,
                               final String requestName ) {
            super(requestType, callback);

            ArgCheck.isTrue(RequestType.isSessionRequest(requestType), "request type is not a session request"); //$NON-NLS-1$
            ArgCheck.isNotNull(requestSession, "requestSession"); //$NON-NLS-1$
            ArgCheck.isNotEmpty(requestName, "requestName"); //$NON-NLS-1$

            this.session = requestSession;
            this.name = requestName;
        }

        String getName() {
            return this.name;
        }

        Session getSession() {
            return this.session;
        }

    }

    private final static ModeShapeEngine msEngine = new ModeShapeEngine();
    private static final KLog LOGGER = KLog.getLogger();

    private JcrRepository repository;

    private BlockingQueue< Request > queue = new LinkedBlockingQueue< Request >();

    private volatile boolean stop = false;

    private final URL configPath;
    private final String workspaceName;

    /**
     * Create this thread and give it a name
     *
     * @param configPath
     *        path to configuration file
     * @param workspaceName
     *        the repository workspace name (cannot be empty)
     */
    public ModeshapeEngineThread( URL configPath,
                                  final String workspaceName ) {
        super("Modeshape Engine Thread"); //$NON-NLS-1$
        this.configPath = configPath;
        this.workspaceName = workspaceName;
        setDaemon(true);
    }

    private void commitSession( final Request request ) {
        ArgCheck.isTrue(request.getRequestType() == RequestType.COMMIT_SESSION,
                        "commitSession called when request is not a commit session"); //$NON-NLS-1$
        final SessionRequest commitRequest = (SessionRequest)request;
        final Session session = commitRequest.getSession();
        LOGGER.debug("commit session for request '{0}'", commitRequest.getName()); //$NON-NLS-1$

        try {
            session.save();
            LOGGER.debug("commit session request '{0}' has been saved", commitRequest.getName()); //$NON-NLS-1$

            if (request.getCallback() != null) {
                request.getCallback().respond(null);
            }
        } catch (final Exception e) {
            if (request.getCallback() == null) {
                LOGGER.error(Messages.getString(Messages.Komodo.ERROR_TRYING_TO_COMMIT, e, commitRequest.getName()));
                rollbackSession(request);
            } else {
                request.getCallback().errorOccurred(e);
            }
        } finally {
            session.logout();
        }
    }

    private Session createSession() throws Exception {
        if (isEngineStopped()) {
            throw new KException(Messages.getString(Messages.LocalRepository.Engine_Is_Stopped));
        }

        // the workspace name must agree with the config file
        return this.repository.login(null, this.workspaceName);
    }

    private boolean isEngineStopped() {
        return ModeShapeEngine.State.NOT_RUNNING.equals(msEngine.getState());
    }

    /**
     * @return is modeshape engine started
     */
    public boolean isEngineStarted() {
        return ModeShapeEngine.State.RUNNING.equals(msEngine.getState());
    }

    private void rollbackSession( final Request request ) {
        ArgCheck.isTrue(request.getRequestType() == RequestType.ROLLBACK_SESSION,
                        "rollbackSession called when request is not a rollback session"); //$NON-NLS-1$
        final SessionRequest rollbackRequest = (SessionRequest)request;
        final Session session = rollbackRequest.getSession();
        LOGGER.debug("rollback session for request '{0}'", rollbackRequest.getName()); //$NON-NLS-1$

        try {
            session.refresh(false);
            LOGGER.debug("rollback session request '{0}' has been rolled back", rollbackRequest.getName()); //$NON-NLS-1$

            if (request.getCallback() != null) {
                request.getCallback().respond(null);
            }
        } catch (final Exception e) {
            if (request.getCallback() == null) {
                LOGGER.error(Messages.getString(Messages.Komodo.ERROR_TRYING_TO_ROLLBACK, e, rollbackRequest.getName()));
            } else {
                request.getCallback().errorOccurred(e);
            }
        } finally {
            session.logout();
        }
    }

    private synchronized void startEngine() throws Exception {
        if (!isEngineStopped()) return;

        // start the ModeShape Engine
        msEngine.start();

        // start the local repository
        final RepositoryConfiguration config = RepositoryConfiguration.read(configPath);

        //
        // Validate the configuration for any errors
        //
        Problems problems = config.validate();
        if (problems.hasProblems()) {

            for (Problem problem : problems) {
                KLog.getLogger().error(Messages.getString(Messages.LocalRepository.Configuration_Problem,
                                                          problem.getMessageString()),
                                       problem.getThrowable());
            }

            // Catastrophic error if the configuration is not valid!
            throw new Exception(Messages.getString(Messages.LocalRepository.Configuration_Failure));
        }

        // Deploy configuration to engine
        repository = msEngine.deploy(config);

        //
        // Check for errors in startup
        //
        problems = repository.getStartupProblems();
        if (problems.hasErrors() || problems.hasWarnings()) {
            Iterator< Problem > iterator = problems.iterator();
            while (iterator.hasNext()) {
                Problem problem = iterator.next();
                switch (problem.getStatus()) {
                    case ERROR:
                        throw new Exception(Messages.getString(Messages.LocalRepository.Deployment_Failure,
                                                               problem.getMessageString()), problem.getThrowable());
                    default:
                        KLog.getLogger().warn(problem.getMessageString(), problem.getThrowable());
                }
            }
        }
    }

    private synchronized void stopEngine() throws Exception {
        try {
            msEngine.shutdown();
        } finally {
            repository = null;
        }
    }

    @Override
    public void run() {
        while (!stop) {
            try {
                Request request = queue.poll(1000L, TimeUnit.MILLISECONDS);

                if (request == null) continue;

                final RequestCallback callback = request.getCallback();
                Throwable error = null;
                Object results = null;

                try {
                    switch (request.getRequestType()) {
                        case START:
                            startEngine();
                            break;
                        case STOP:
                            stopEngine();
                            stop = true;
                            break;
                        case CREATE_SESSION:
                            results = createSession();
                            break;
                        case COMMIT_SESSION:
                            commitSession(request);
                            break;
                        case ROLLBACK_SESSION:
                            rollbackSession(request);
                            break;
                        default:
                            break;
                    }
                } catch (final Exception e) {
                    error = e;
                }

                if (callback != null) {
                    if (error == null) {
                        callback.respond(results);
                    } else {
                        callback.errorOccurred(error);
                    }
                }
            } catch (final Exception e) {
                stop = true;
                KLog.getLogger().error(Messages.getString(Messages.LocalRepository.General_Exception), e, e.getLocalizedMessage());
            }
        }
    }

    /**
     * Pass a request to the engine
     *
     * @param request
     *        the request made to the engine
     */
    public void accept( Request request ) {
        try {
            queue.put(request);
        } catch (InterruptedException ex) {
            KLog.getLogger().error(Messages.getString(Messages.LocalRepository.General_Exception), ex, ex.getLocalizedMessage());
        }
    }
}
