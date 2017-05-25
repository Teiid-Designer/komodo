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

import javax.jcr.Session;
import org.komodo.repository.internal.KSequencers;


/**
 * Listener for {@link KSequencers} events
 */
public interface KSequencerListener {

    /**
     * @return unique id of the events to listen for. This id should match
     *                the id
     */
    String id();

    /**
     * @return the session associated with the sequence listener
     */
    Session session();

    /**
     * Will be called when all sequencers have been executed
     */
    void sequencingCompleted();

    /**
     * Will be called if the sequencers encounter an error
     *
     * @param exception error encountered
     */
    void sequencingError(Exception exception);

    /**
     * Called to abort the listener operation if an error has occurred in
     * the parent transaction
     */
    void abort();
}
