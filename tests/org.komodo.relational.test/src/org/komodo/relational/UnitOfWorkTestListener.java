/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational;

import java.util.concurrent.CountDownLatch;
import org.junit.Assert;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;

@SuppressWarnings( {"javadoc"} )
public class UnitOfWorkTestListener implements UnitOfWorkListener {

    protected final CountDownLatch latch;

    public UnitOfWorkTestListener(final CountDownLatch testLatch) {
        this.latch = testLatch;
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
