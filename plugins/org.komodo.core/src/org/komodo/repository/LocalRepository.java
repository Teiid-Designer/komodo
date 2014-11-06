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
import java.util.List;
import org.komodo.repository.internal.ModeshapeEngineThread;
import org.komodo.repository.internal.ModeshapeEngineThread.Request;
import org.komodo.repository.internal.ModeshapeEngineThread.RequestCallback;
import org.komodo.repository.internal.ModeshapeEngineThread.RequestType;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.IRepository;
import org.komodo.spi.repository.IRepositoryClient;
import org.komodo.spi.repository.IRepositoryObserver;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.utils.KLog;

/**
 * A repository installed on the local machine, using the modeshape
 * engine and repository.
 */
public class LocalRepository implements IRepository, StringConstants {

    private static String LOCAL_REPOSITORY_CONFIG = "local-repository-config.json"; //$NON-NLS-1$

    private static LocalRepository instance;

    private static class LocalRepositoryId implements Id {

        private final URL configPath = LocalRepository.class.getResource(LOCAL_REPOSITORY_CONFIG);

        @Override
        public String getName() {
            return LOCAL_REPOSITORY;
        }

        @Override
        public String getUrl() {
            return LOCAL_REPOSITORY;
        }

        @Override
        public URL getConfiguration() {
            return configPath;
        }
        
    }

    /**
     * @return singleton instance
     */
    public static LocalRepository getInstance() {
        if (instance == null)
            instance = new LocalRepository();

        return instance;
    }

    private final LocalRepositoryId id = new LocalRepositoryId();

    private State state;

    private List<IRepositoryObserver> observers = new ArrayList<IRepositoryObserver>();
    
    private List<IRepositoryClient> clients = new ArrayList<IRepositoryClient>();

    private ModeshapeEngineThread engineThread;

    /**
     * Create instance
     */
    private LocalRepository() {}

    @Override
    public Id getId() {
        return id;
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
    public void addClient(IRepositoryClient client) {
        clients .add(client);
    }

    @Override
    public void removeClient(IRepositoryClient client) {
        clients.remove(client);
    }

    @Override
    public void addObserver(IRepositoryObserver observer) {
        observers.add(observer);
    }

    @Override
    public void removeObserver(IRepositoryObserver observer) {
        observers.remove(observer);
    }

    private void notifyObservers() {
        for (IRepositoryObserver observer : observers) {
            try {
                // Ensure all observers are informed even if one
                // throws an exception
                observer.stateChanged();
            } catch (Exception ex) {
                KLog.getLogger().error(
                                       Messages.getString(
                                                          Messages.LocalRepository.General_Exception), ex);
            }
        }
    }

    private void createEngineThread() {
        if (engineThread != null && engineThread.isAlive())
            return;

        if (engineThread != null && !engineThread.isAlive())
            throw new RuntimeException(Messages.getString(Messages.LocalRepository.EngineThread_Died));

        engineThread = new ModeshapeEngineThread(getId().getConfiguration());
        engineThread.start();
    }

    private void startRepository() {
        if (this.state == State.REACHABLE)
            return;

        createEngineThread();

        RequestCallback callback = new RequestCallback() {
            @Override
            public void execute() {
                if (engineThread.isEngineStarted()) {
                    LocalRepository.this.state = State.REACHABLE;
                    notifyObservers();
                }
            }
        };

        engineThread.accept(new Request(RequestType.START, callback));
    }

    @Override
    public void notify(RepositoryClientEvent event) {
        if (event.getType() == RepositoryClientEvent.EventType.STARTED) {
            // Start the modeshape engine if not already started
            startRepository();
        }
    }

}
