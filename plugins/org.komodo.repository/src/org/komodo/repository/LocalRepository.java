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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.komodo.spi.repository.IRepository;
import org.komodo.spi.repository.IRepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.utils.KLog;
import org.modeshape.common.collection.Problem;
import org.modeshape.common.collection.Problems;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.RepositoryConfiguration;

/**
 * A repository installed on the local machine, using the modeshape
 * engine and repository.
 */
public class LocalRepository implements IRepository {

    private static String LOCAL_REPOSITORY_CONFIG = "local-repository-config.json"; //$NON-NLS-1$

    private static LocalRepository instance;

    private final static ModeShapeEngine msEngine = new ModeShapeEngine();

    private JcrRepository repository;

    /**
     * @return singleton instance
     */
    public static LocalRepository getInstance() {
        if (instance == null)
            instance = new LocalRepository();

        return instance;
    }

    private State state;

    private List<IRepositoryClient> clients = new ArrayList<IRepositoryClient>();

    /**
     * Create instance
     */
    private LocalRepository() {}

    @Override
    public Id getId() {
        return null;
    }

    @Override
    public State getState() {
        return state;
    }

    @Override
    public Type getType() {
        return null;
    }

    @Override
    public boolean ping() {
        return false;
    }

    @Override
    public void add(IRepositoryClient client) {
        clients .add(client);
    }

    @Override
    public void remove(IRepositoryClient client) {
        clients.remove(client);
    }

    private void startRepository() {
        if (this.state == State.REACHABLE)
            return;

        try {
            // start the ModeShape Engine
            msEngine.start();
         
            // start the local repository
            URL repoConfigPath = getClass().getResource(LOCAL_REPOSITORY_CONFIG);
            final RepositoryConfiguration config = RepositoryConfiguration.read(repoConfigPath);
            Problems problems = config.validate();
            if (problems.hasProblems()) {

                for (Problem problem : problems) {
                    KLog.getLogger().error(
                                           Messages.getString(
                                                              Messages.LocalRepository.Configuration_Problem, problem.getMessageString()),
                                                              problem.getThrowable());
                }

                // Catastrophic error if the configuration is not valid!
                throw new RuntimeException(Messages.getString(Messages.LocalRepository.Configuration_Failure));
            }

            repository = msEngine.deploy(config);
            problems = repository.getStartupProblems();
            if (problems.hasErrors() || problems.hasWarnings()) {
                Iterator<Problem> iterator = problems.iterator();
                while(iterator.hasNext()) {
                    Problem problem = iterator.next();
                    switch (problem.getStatus()) {
                        case ERROR:
                            throw new RuntimeException(
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
            throw new RuntimeException(ex);
        }
        
        this.state = State.REACHABLE;
    }
    
    @Override
    public void notify(RepositoryClientEvent event) {
        if (event.getType() == RepositoryClientEvent.EventType.STARTED) {
            // Start the modeshape engine if not already started
            startRepository();
        }
    }

}
