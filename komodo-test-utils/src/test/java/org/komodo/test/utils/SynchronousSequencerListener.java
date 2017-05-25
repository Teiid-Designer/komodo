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
import java.util.concurrent.TimeUnit;
import javax.jcr.Session;
import org.komodo.repository.KSequencerController;
import org.komodo.repository.KSequencerListener;
import org.komodo.utils.KLog;

/**
 * Listener that will wait for the sequencers to complete prior to letting
 * the thread calling {@link #wait()} to continue.
 */
public class SynchronousSequencerListener implements KSequencerListener {

    private final CountDownLatch latch = new CountDownLatch(1);

    private final KSequencerController sequencers;

    private final String listenerId;

    private final Session session;

    private Exception sequencerException = null;

    /**
     * @param listenerId the id of this listener
     * @param session the session
     * @param sequencers the sequencers
     * @throws Exception if error occurs
     */
    public SynchronousSequencerListener(String listenerId, Session session, KSequencerController sequencers) throws Exception {
        this.listenerId = listenerId;
        this.session = session;
        this.sequencers = sequencers;
        this.sequencers.addSequencerListener(this);
    }

    /**
     * Wait for the completion of the sequencers
     *
     * @param timeout the maximum time to wait
     * @param unit the time unit of the {@code timeout} argument
     * @return {@code true} if the count reached zero and {@code false}
     *         if the waiting time elapsed before the count reached zero
     * @throws Exception if error occurs
     */
    public boolean await(long timeout, TimeUnit unit) throws Exception {
        return latch.await(timeout, unit);
    }

    @Override
    public String id() {
        return listenerId;
    }

    @Override
    public Session session() {
        return session;
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

    /**
     * @return any exception that may have occurred or null.
     */
    public Exception exception() {
        return sequencerException;
    }

    @Override
    public void abort() {
        throw new UnsupportedOperationException();
    }
}