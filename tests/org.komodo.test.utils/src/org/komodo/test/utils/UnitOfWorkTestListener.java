/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.test.utils;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.Assert;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;

@SuppressWarnings( {"javadoc"} )
public class UnitOfWorkTestListener implements UnitOfWorkListener {

    protected final CountDownLatch latch;
    protected final long numUnits;
    protected final TimeUnit unit;

    public UnitOfWorkTestListener() {
        this( new CountDownLatch( 1 ), 3, TimeUnit.SECONDS );
    }

    public UnitOfWorkTestListener( final CountDownLatch testLatch,
                                   final long testNumUnits,
                                   final TimeUnit testUnit ) {
        this.latch = testLatch;
        this.numUnits = testNumUnits;
        this.unit = testUnit;
    }

    public boolean await() throws InterruptedException {
        return this.latch.await( this.numUnits, this.unit );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository.UnitOfWorkListener#errorOccurred(java.lang.Throwable)
     */
    @Override
    public void errorOccurred( final Throwable error ) {
        Assert.fail();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository.UnitOfWorkListener#respond(java.lang.Object)
     */
    @Override
    public void respond( final Object results ) {
        this.latch.countDown();
    }

}
