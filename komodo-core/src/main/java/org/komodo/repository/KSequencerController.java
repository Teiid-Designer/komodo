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

import org.komodo.repository.internal.KSequencers;


/**
 * Controls the execution of the sequencers
 */
public interface KSequencerController {

    /**
     * The Sequencers executed by {@link KSequencers}
     */
    public static enum SequencerType {
        /**
         * VDB Sequencer
         */
        VDB("VDB Dynamic Sequencer"), //$NON-NLS-1$

        /**
         * DDL Sequencer
         */
        DDL("DDL Sequencer"), //$NON-NLS-1$

        /**
         * Connection Sequencer
         */
        CONNECTION("Connection Sequencer"), //$NON-NLS-1$

        /**
         * Data Service Sequencer
         */
        DATA_SERVICE("Data Service Sequencer"), //$NON-NLS-1$

        /**
         * Teiid SQL Sequencer
         */
        TSQL("Teiid SQL Sequencer"); //$NON-NLS-1$

        private String id;

        private SequencerType(String id) {
            this.id = id;
        }

        @Override
        public String toString() {
            return id;
        }
    }

    /**
     * Dispose of this instance
     */
    void dispose();

    /**
     * @param listener the listener to add
     *
     * @throws Exception if error occurs
     */
    void addSequencerListener(KSequencerListener listener) throws Exception;

}
