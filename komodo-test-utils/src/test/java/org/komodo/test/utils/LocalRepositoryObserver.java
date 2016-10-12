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
package org.komodo.test.utils;

import java.util.concurrent.CountDownLatch;
import org.komodo.repository.LocalRepository;
import org.komodo.spi.repository.RepositoryObserver;

/**
 * A {@link RepositoryObserver} containing a latch that can be used to hold
 * a thread until a state change in the {@link LocalRepository} has occurred
 */
public class LocalRepositoryObserver implements RepositoryObserver {

    private CountDownLatch latch;

    private Throwable error;

    /**
     * Constructor
     */
    public LocalRepositoryObserver() {
        resetLatch();
    }

    /**
     * Reset the latch
     */
    public void resetLatch() {
        latch = new CountDownLatch(1);
    }

    /**
     * @return the latch
     */
    public CountDownLatch getLatch() {
        return this.latch;
    }

    public Throwable getError() {
        return error;
    }

    @Override
    public void eventOccurred() {
        latch.countDown();
    }

    @Override
    public void errorOccurred(Throwable e) {
        error = e;
        latch.countDown();
    }
}