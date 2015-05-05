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

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;

/**
 * Makes an asynchronous transaction commit operation synchronous.
 *
 * <p>
 * Add an instance of this class to a transaction:
 * <p>
 * {@code SynchronousCallback callback = new SynchronousCallback();
                 UnitOfWork transaction = repo.createTransaction("the-transaction", false, callback);
    }
 * <p>
 * <p>
 * At the point of committing the transaction, do the following:
 * <p>
 * {@code transaction.commit();
                 if (! callback.wait(3, TimeUnit.MINUTES)) throw someException
                 if (callback.hasError()) throw someException
    }
 * <p>
 * <p>
 * This will hold the thread committing the transaction until it has completely
 * finished.
 */
public class SynchronousCallback implements UnitOfWorkListener {

    private final CountDownLatch latch = new CountDownLatch(1);

    private Throwable error;

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
    public void respond(Object results) {
        latch.countDown();
    }

    @Override
    public void errorOccurred(Throwable error) {
        this.error = error;
        latch.countDown();
    }

    /**
     * @return the error
     */
    public Throwable error() {
        return this.error;
    }

    /**
     * @return true if error occurred
     */
    public boolean hasError() {
        return this.error != null;
    }

}
