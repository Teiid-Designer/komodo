/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.Rule;
import org.junit.rules.TestName;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( { "javadoc" } )
public class RelationalModelTest extends AbstractLocalRepositoryTest {

    class Callback implements UnitOfWorkListener {

        private final CountDownLatch latch;

        Callback( final CountDownLatch latch ) {
            this.latch = latch;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWorkListener#awaitSequencerCompletion()
         */
        @Override
        public boolean awaitSequencerCompletion() {
            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWorkListener#errorOccurred(java.lang.Throwable)
         */
        @Override
        public void errorOccurred( final Throwable error ) {
            this.latch.countDown();
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

    private CountDownLatch latch;

    @Rule
    public TestName name = new TestName();
    private int secondsToWait = 3;

    protected void commit( final UnitOfWork uow ) throws Exception {
        uow.commit();
        assertThat( this.latch.await( this.secondsToWait, TimeUnit.SECONDS ), is( true ) );
        assertThat( uow.getError(), is( nullValue() ) );
        assertThat( uow.getState(), is( State.COMMITTED ) );
    }

    protected UnitOfWork createWriteTransaction() throws Exception {
        return createWriteTransaction( 3 );
    }

    protected UnitOfWork createWriteTransaction( final int secondsToWait ) throws Exception {
        this.secondsToWait = secondsToWait;
        this.latch = new CountDownLatch( 1 );
        return _repo.createTransaction( this.name.getMethodName(), false, new Callback( this.latch ) );
    }

}
