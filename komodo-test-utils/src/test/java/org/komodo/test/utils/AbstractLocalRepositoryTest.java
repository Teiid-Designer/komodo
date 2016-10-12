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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

import java.net.URL;
import java.util.concurrent.TimeUnit;

import javax.jcr.Node;
import javax.jcr.PropertyIterator;
import javax.jcr.Session;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.rules.TestName;
import org.komodo.repository.LocalRepository;
import org.komodo.repository.LocalRepository.LocalRepositoryId;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.repository.SynchronousCallback;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.utils.KLog;

/**
 * Provides framework for testing an instance of the local repository
 * which is only cached in memory hence should be persisted between
 * tests.
 *
 * The initLocalRepository method will be called prior to any tests executing
 * ensuring that the _repo is initialised and reachable. This can be added to
 * the singleton KEngine instance using KEngine.setDefaultRepository if required.
 *
 * When tests are complete, destroyLocalRepository will be called and attempt
 * to stop and close down the repository. Since the repository is in-memory only
 * then nullifying it will destroy all data hence clearance between test classes
 * should be unnecessary. Sub-classes using KEngine should stop the KEngine
 * in an @AfterClass annotated method and use the _repoObserver to await
 * the shutdown of the repository. The destoryLocalRepository function will
 * still run but it should do nothing since _repo is shutdown via the KEngine.
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractLocalRepositoryTest extends AbstractLoggingTest {

    private static final String TEST_REPOSITORY_CONFIG = "test-local-repository-in-memory-config.json";

    protected static final long TIME_TO_WAIT = 3; // in minutes

    protected static LocalRepository _repo = null;

    protected static LocalRepositoryObserver _repoObserver = null;

    @BeforeClass
    public static void initRepository() throws Exception {
        URL configUrl = AbstractLocalRepositoryTest.class.getResource(TEST_REPOSITORY_CONFIG);
        LocalRepositoryId id = new LocalRepositoryId(configUrl, DEFAULT_LOCAL_WORKSPACE_NAME);
        _repo = new LocalRepository(id);
        assertThat(_repo.getState(), is(State.NOT_REACHABLE));
        assertThat(_repo.ping(), is(false));

        _repoObserver = new LocalRepositoryObserver();
        assertNotNull(_repoObserver);
        _repo.addObserver(_repoObserver);

        // Start the repository
        final RepositoryClient client = mock(RepositoryClient.class);
        final RepositoryClientEvent event = RepositoryClientEvent.createStartedEvent(client);
        _repo.notify(event);

        // Wait for the starting of the repository or timeout of 1 minute
        if (!_repoObserver.getLatch().await(1, TimeUnit.MINUTES)) {
            fail("Test timed-out waiting for local repository to start");
        }

        Throwable startupError = _repoObserver.getError();
        if (startupError != null) {
            startupError.printStackTrace();
            fail("Repository error occurred on startup: " + startupError.getMessage());
        }

        { // verify initial content (see initialContent.xml)
            UnitOfWork transaction = null;

            try {
                transaction = _repo.createTransaction(TEST_USER, "verifyInitialRepositoryContent", true, null );
                final KomodoObject workspace = _repo.komodoWorkspace(transaction);
                workspace.getName( transaction );
                transaction.commit();
            } catch ( final Exception e ) {
                throw new Exception( "Failed verifying initial workspace content: " + e.getLocalizedMessage(), e );
            } finally {
                if ( transaction != null ) {
                    transaction.commit();
                }
            }
        }
    }

    /**
     * Shutdown and destroy repo
     *
     * @throws Exception
     */
    @AfterClass
    public static void destroyLocalRepository() throws Exception {
        assertNotNull(_repo);
        assertNotNull(_repoObserver);

        _repoObserver.resetLatch();

        RepositoryClient client = mock(RepositoryClient.class);
        RepositoryClientEvent event = RepositoryClientEvent.createShuttingDownEvent(client);
        _repo.notify(event);

        try {
            if (! _repoObserver.getLatch().await(TIME_TO_WAIT, TimeUnit.MINUTES))
                fail("Local repository was not stopped");
        } finally {
            _repo.removeObserver(_repoObserver);
            _repoObserver = null;
            _repo = null;
        }
    }

    @Rule
    public TestName name = new TestName();
    private boolean rollbackOnly = false;
    private int txCount;
    private UnitOfWork uow;
    private UnitOfWork sysUow;
    protected SynchronousCallback callback;
    protected SynchronousCallback sysCallback;

    @Before
    public void createInitialTransactions() throws Exception {
        this.callback = new TestTransactionListener();
        this.uow = createTransaction(callback);

        this.sysCallback = new TestTransactionListener();
        this.sysUow = createTransaction(Repository.SYSTEM_USER, txId(Repository.SYSTEM_USER, "tx"), false, this.sysCallback);
        KLog.getLogger().debug( "\n\n ----- Test {0}: createInitialTransactions() finished", this.name.getMethodName() );
    }

    @After
    public void cleanup() throws Exception {
        { // process current transaction if necessary
            if ( this.uow != null ) {
                switch ( this.uow.getState() ) {
                    case NOT_STARTED:
                    case RUNNING:
                        rollback();
                        break;
                    case COMMITTED:
                    case ERROR:
                    case ROLLED_BACK:
                    default:
                        break;
                }

                this.uow = null;
                this.callback = null;
                this.txCount = 0;
            }
        }

        //
        // process sys transaction if necessary
        //
        if ( this.sysUow != null ) {
            switch ( this.sysUow.getState() ) {
                case NOT_STARTED:
                case RUNNING:
                    sysRollback();
                    break;
                case COMMITTED:
                case ERROR:
                case ROLLED_BACK:
                default:
                    break;
            }

            this.sysUow = null;
            this.sysCallback = null;
        }

        { // clean repository
            assertNotNull( _repo );

            if ( !State.REACHABLE.equals( _repo.getState() ) ) return;

            _repoObserver.resetLatch();

            RepositoryClient client = mock( RepositoryClient.class );
            RepositoryClientEvent event = RepositoryClientEvent.createClearEvent( client );
            _repo.notify( event );

            if ( !_repoObserver.getLatch().await( TIME_TO_WAIT, TimeUnit.MINUTES ) ) throw new RuntimeException( "Local repository was not cleared" );

            KLog.getLogger().debug( "Test {0}: clearLocalRepository() finished\n\n=====\n\n", this.name.getMethodName() );
        }
    }

    protected static void commit( UnitOfWork currTx,
                                                              SynchronousCallback currCallback,
                                                              UnitOfWork.State expectedState) throws Exception {
        currTx.commit();

        assertThat( currCallback.await( TIME_TO_WAIT, TimeUnit.MINUTES ), is( true ) );

        if (expectedState == UnitOfWork.State.ERROR) {
            assertThat( currTx.getState(), is( expectedState ) );
        } else {
            assertThat( currTx.getError(), is( nullValue() ) );
            assertThat( currTx.getState(), is( expectedState ) );
        }

        if ( currCallback instanceof TestTransactionListener ) {
            final boolean respond = ( UnitOfWork.State.COMMITTED == expectedState );

            if ( !respond ) {
                assertThat( expectedState, is( UnitOfWork.State.ERROR ) );
            }

            assertThat( ( ( TestTransactionListener )currCallback ).respondCallbackReceived(), is( respond ) );
            assertThat( ( ( TestTransactionListener )currCallback ).errorCallbackReceived(), is( !respond ) );
        }
    }

    protected static void commit(UnitOfWork currTx, UnitOfWork.State expectedState) throws Exception {
        assertTrue(currTx.getCallback() instanceof SynchronousCallback);
        commit(currTx, (SynchronousCallback) currTx.getCallback(), expectedState);
    }

    private void commit( final UnitOfWork.State expectedState, final SynchronousCallback nextCallback ) throws Exception {
        commit(this.uow, this.callback, expectedState);

        this.callback = nextCallback;
        this.uow = _repo.createTransaction(TEST_USER, this.name.getMethodName(), this.rollbackOnly, this.callback );
    }

    protected void commit( final UnitOfWork.State expectedState ) throws Exception {
        final SynchronousCallback nextCallback = new TestTransactionListener();
        commit( expectedState, nextCallback );
    }

    protected void commit() throws Exception {
        commit(UnitOfWork.State.COMMITTED);
    }

    protected void sysCommit( final UnitOfWork.State expectedState, final SynchronousCallback nextCallback ) throws Exception {
        commit(this.sysUow, this.sysCallback, expectedState);

        this.sysCallback = nextCallback;
        this.sysUow = createTransaction(Repository.SYSTEM_USER, txId(Repository.SYSTEM_USER, "sysTx"), false, sysCallback);
    }

    protected void sysCommit( final UnitOfWork.State expectedState ) throws Exception {
        final SynchronousCallback nextCallback = new TestTransactionListener();
        sysCommit( expectedState, nextCallback );
    }

    protected void sysCommit() throws Exception {
        sysCommit(UnitOfWork.State.COMMITTED);
    }

    protected void useCustomCallback( final SynchronousCallback callback,
                                      final boolean commitCurrentTransaction ) throws Exception {
        if ( commitCurrentTransaction ) {
            commit( UnitOfWork.State.COMMITTED, callback );
        } else {
            rollback( callback );
        }
    }

    protected static UnitOfWork createTransaction( String user, String txName, boolean rollback, final UnitOfWorkListener callback ) throws Exception {
        return _repo.createTransaction(user, txName , rollback, callback);
    }

    private UnitOfWork createTransaction( final UnitOfWorkListener callback ) throws Exception {
        return createTransaction(TEST_USER, (this.name.getMethodName() + '-' + this.txCount++), this.rollbackOnly, callback);
    }

    protected static String txId(String... components) {
        StringBuffer buf = new StringBuffer();

        assertNotNull(components);
        for (int i = 0; i < components.length; ++i) {
            buf.append(components[i]);
            if (i < (components.length - 1))
                buf.append(HYPHEN);
        }

        return buf.toString();
    }

    /**
     * System Transaction can search from workspace root rather than just inside home directory
     */
    protected UnitOfWork sysTx() throws Exception {
        return this.sysUow;
    }

    /**
     * The transaction object should <strong>NOT</strong> be cached.
     *
     * @return the current transaction (never <code>null</code>)
     */
    protected UnitOfWork getTransaction() {
        return this.uow;
    }

    protected static void rollback( UnitOfWork currTx, SynchronousCallback currCallback) throws Exception {
        currTx.rollback();

        assertThat( currCallback.await( TIME_TO_WAIT, TimeUnit.MINUTES ), is( true ) );
        assertThat( currTx.getError(), is( nullValue() ) );
        assertThat( currTx.getState(), is( UnitOfWork.State.ROLLED_BACK ) );

        if ( currCallback instanceof TestTransactionListener ) {
            assertThat( ( ( TestTransactionListener )currCallback ).respondCallbackReceived(), is( true ) );
            assertThat( ( ( TestTransactionListener )currCallback ).errorCallbackReceived(), is( false ) );
        }
    }

    private void rollback( final SynchronousCallback nextCallback ) throws Exception {
        rollback(this.uow, this.callback);

        // create new transaction
        this.callback = nextCallback;
        this.uow = createTransaction( this.callback );
    }

    private void sysRollback( final SynchronousCallback nextCallback ) throws Exception {
        rollback(this.sysUow, this.sysCallback);

        // create new transaction
        this.sysCallback = nextCallback;
        this.sysUow = createTransaction( this.sysCallback );
    }

    protected void rollback() throws Exception {
        rollback( new TestTransactionListener() );
    }

    protected void sysRollback() throws Exception {
        sysRollback( new TestTransactionListener() );
    }

    protected Session session(UnitOfWork uow) throws Exception {
        if (!(uow instanceof UnitOfWorkImpl))
            throw new Exception("Attempt to extract session from unit of work which is not a UnitOfWorkImpl");

        Session session = ((UnitOfWorkImpl)uow).getSession();
        return session;
    }

    protected void traverse(String tabs, Node node, StringBuffer buffer) throws Exception {
        buffer.append(tabs + node.getName() + NEW_LINE);

        PropertyIterator propertyIterator = node.getProperties();
        while (propertyIterator.hasNext()) {
            javax.jcr.Property property = propertyIterator.nextProperty();
            buffer.append(tabs + TAB + "@" + property.toString() + NEW_LINE);
        }

        javax.jcr.NodeIterator children = node.getNodes();
        while (children.hasNext()) {
            traverse(tabs + TAB, children.nextNode(), buffer);
        }
    }

    protected void traverse(Node node) throws Exception {
        StringBuffer buffer = new StringBuffer(NEW_LINE);
        traverse(TAB, node, buffer);
        KLog.getLogger().info(buffer.toString());
    }

    protected void traverse(UnitOfWork uow, String nodePath) throws Exception {
        Session session = session(uow);
        Node node = session.getNode(nodePath);
        traverse(node);
    }

    protected void traverse(UnitOfWork uow) throws Exception {
        Session session = session(uow);
        traverse(session.getRootNode());
    }

    /**
     * @param property
     * @return String representation of property and its values
     * @throws Exception
     */
    @SuppressWarnings( "unused" )
    private String toString(Property property) throws Exception {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append(property.getName(this.uow)).append('=');
            if (property.isMultiple(this.uow)) {
                sb.append('[');
                Object[] values = property.getValues(this.uow);
                for (int i = 0; i < values.length; ++i) {
                    Object value = values[i];
                    sb.append(value);
                    if ((i + 1) < values.length)
                        sb.append(',');
                }
                sb.append(']');
            } else {
                Object value = property.getValue(this.uow);
                sb.append(value);
            }
        } catch (Exception e) {
            sb.append(" on deleted node ").append(property.getAbsolutePath());
        }

        return sb.toString();
    }

    protected static class TestTransactionListener extends SynchronousCallback {

        private boolean errorCallback = false;
        private boolean successCallback = false;

        public TestTransactionListener() {
            // Nothing to do
        }

        protected boolean errorCallbackReceived() {
            return this.errorCallback;
        }

        protected boolean respondCallbackReceived() {
            return this.successCallback;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.repository.SynchronousCallback#errorOccurred(java.lang.Throwable)
         */
        @Override
        public void errorOccurred( final Throwable error ) {
            this.errorCallback = true;
            super.errorOccurred( error );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.repository.SynchronousCallback#respond(java.lang.Object)
         */
        @Override
        public void respond( final Object results ) {
            this.successCallback = true;
            super.respond( results );
        }

    }

}
