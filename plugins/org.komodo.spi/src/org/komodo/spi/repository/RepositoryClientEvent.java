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
package org.komodo.spi.repository;


/**
 * Event sent by a repository client
 */
public final class RepositoryClientEvent {

    /**
     * Event types describing the repository client
     */
    public enum EventType {
        /**
         * Sent when an {@link IRepositoryClient} has just started
         */
        STARTED,

        /**
         * Sent just prior to an {@link IRepositoryClient} shutting down 
         */
        SHUTTING_DOWN,
    }

    private final EventType eventType;

    private final IRepositoryClient source;

    /**
     * @param eventType type of this event
     * @param source the source of this event
     * 
     */
    public RepositoryClientEvent(EventType eventType, IRepositoryClient source) {
        this.eventType = eventType;
        this.source = source;
    }

    /**
     * @return the type of this event
     */
    public EventType getType() {
        return eventType;
    }

    /**
     * @return the source
     */
    public IRepositoryClient getSource() {
        return this.source;
    }

    /**
     * @param source the source of the new event
     *
     * @return event representing the given client has started
     */
    public static RepositoryClientEvent createStartedEvent(IRepositoryClient source) {
        return new RepositoryClientEvent(EventType.STARTED, source);
    }
}
