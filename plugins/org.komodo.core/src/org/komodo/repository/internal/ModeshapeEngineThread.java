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
import org.komodo.repository.Messages;
import org.komodo.utils.KLog;
import org.modeshape.common.collection.Problem;
import org.modeshape.common.collection.Problems;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.RepositoryConfiguration;

/**
 *
 */
public class ModeshapeEngineThread extends Thread {

    /**
     * Request types to send to this engine thread
     */
    public enum RequestType {
        /**
         * Request starting of the engine
         */
        START,

        /**
         * Request stopping of the engine
         */
        STOP
    }

    /**
     * Callback interface that can be implemented by third-parties
     * and incorporated into requests to the engine thread
     */
    public interface RequestCallback {

        /**
         * Execute custom function after a change of engine state.
         *
         * Note. Implementations are responsible for checking the
         * state of the engine during execution as state functions
         * will execute at their conclusion regardless of success / failure
         */
        public void execute();
    }

    /**
     * Request crafted by
     * wishing to be notified of a change of engine state
     */
    public static class Request {

        private RequestType requestType;

        private RequestCallback callback;

        /**
         * @param requestType type of request
         * @param callback callback for execution after change of engine state
         */
        public Request(RequestType requestType, RequestCallback callback) {
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

    private final static ModeShapeEngine msEngine = new ModeShapeEngine();

    private JcrRepository repository;

    private BlockingQueue<Request> queue = new LinkedBlockingQueue<Request>();

    private volatile boolean stop = false;

    private final URL configPath;

    /**
     * Create this thread and give it a name
     * @param configPath path to configuration file
     */
    public ModeshapeEngineThread(URL configPath) {
        super("Modeshape Engine Thread"); //$NON-NLS-1$
        this.configPath = configPath;
        setDaemon(true);
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

    private synchronized void startEngine(RequestCallback requestCallback) throws Exception {
        try {
            if (!isEngineStopped())
                return;

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
                    KLog.getLogger().error(
                                           Messages.getString(
                                                              Messages.LocalRepository.Configuration_Problem, problem.getMessageString()),
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
                Iterator<Problem> iterator = problems.iterator();
                while(iterator.hasNext()) {
                    Problem problem = iterator.next();
                    switch (problem.getStatus()) {
                        case ERROR:
                            throw new Exception(
                                                       Messages.getString(
                                                                          Messages.LocalRepository.Deployment_Failure, problem.getMessageString()),
                                                                          problem.getThrowable());
                        default:
                            KLog.getLogger().warn(problem.getMessageString(), problem.getThrowable());
                    }
                }
            }
        } catch (Exception ex) {
            KLog.getLogger().error(
                                   Messages.getString(
                                                      Messages.LocalRepository.General_Exception), ex);
            throw ex;
        } finally {
            if (requestCallback != null)
                requestCallback.execute();
        }
    }

    private synchronized void stopEngine(RequestCallback requestCallback) throws Exception {
        try {
            msEngine.shutdown();
        } finally {
            repository = null;

            if (requestCallback != null)
                requestCallback.execute();
        }
    }

    @Override
    public void run() {
        try {
            while (!stop) {
                Request request = queue.poll(1000L, TimeUnit.MILLISECONDS);
                if (request == null)
                    continue;

                switch(request.getRequestType()) {
                    case START:
                        startEngine(request.getCallback());
                        break;
                    case STOP:
                        stopEngine(request.getCallback());
                        stop = true;
                        break;
                }


            }
        } catch (Exception ex) {
            KLog.getLogger().error(
                                   Messages.getString(
                                                      Messages.LocalRepository.General_Exception), ex);
        }
    }

    /**
     * Pass a request to the engine
     *
     * @param request the request made to the engine
     */
    public void accept(Request request) {
        try {
            queue.put(request);
        } catch (InterruptedException ex) {
            KLog.getLogger().error(
                                   Messages.getString(
                                                      Messages.LocalRepository.General_Exception), ex);
        }
    }
}
