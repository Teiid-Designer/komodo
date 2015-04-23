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
import org.komodo.repository.KSequencerListener;
import org.komodo.repository.KSequencers;
import org.komodo.utils.KLog;

/**
 * Listener that checks new nodes to see if they equals given paths
 */
public class SequencerLatchListener implements KSequencerListener {

    private final CountDownLatch latch = new CountDownLatch(1);

    private final KSequencers sequencers;

    private Exception sequencerException = null;

    /**
     * @param sequencers the sequencers
     */
    public SequencerLatchListener(KSequencers sequencers) {
        this.sequencers = sequencers;
        this.sequencers.addSequencerListener(this);
    }

    /**
     * @return latch
     */
    public CountDownLatch getLatch() {
        return latch;
    }

    @Override
    public void sequencingCompleted() {
        latch.countDown();
    }

    @Override
    public void sequencingError(Exception ex) {
        sequencerException = ex;
        KLog.getLogger().error("Test Sequencer failure: "); //$NON-NLS-1$
        sequencerException.printStackTrace();

        latch.countDown();
    }

    /**
     * @return true if exception occurred
     */
    public boolean exceptionOccurred() {
        return sequencerException != null;
    }
}