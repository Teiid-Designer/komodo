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

import org.komodo.spi.repository.IRepository;
import org.komodo.spi.repository.IRepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.modeshape.jcr.ModeShapeEngine;

/**
 * A repository installed on the local machine, using the modeshape
 * engine and repository.
 */
public class LocalRepository implements IRepository {

    private static LocalRepository instance;

    private final static ModeShapeEngine engine = new ModeShapeEngine();

    public static LocalRepository getInstance() {
        if (instance == null)
            instance = new LocalRepository();

        return instance;
    }

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
        return null;
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
    }

    @Override
    public void remove(IRepositoryClient client) {
    }

    @Override
    public void notify(RepositoryClientEvent event) {
        if (event.getType() == RepositoryClientEvent.EventType.STARTED) {
            // Start the modeshape engine if not already started
            // TODO
        }
    }

}
