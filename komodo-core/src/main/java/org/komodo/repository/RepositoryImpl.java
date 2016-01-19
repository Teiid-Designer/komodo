/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository;

import static org.komodo.repository.Messages.Komodo.ERROR_REPO_HAS_CHANGES;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PropertyIterator;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.query.Query;
import javax.jcr.query.QueryManager;
import javax.jcr.query.QueryResult;
import org.komodo.core.KEngine;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoLexicon.Komodo;
import org.komodo.core.KomodoLexicon.LibraryComponent;
import org.komodo.core.KomodoLexicon.Search;
import org.komodo.core.KomodoLexicon.WorkspaceItem;
import org.komodo.repository.search.ObjectSearcher;
import org.komodo.repository.validation.ValidationManagerImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Artifact;
import org.komodo.spi.repository.ArtifactDescriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.spi.repository.ValidationManager;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.JcrTools;

/**
 * A {@link Repository} implementation.
 */
public abstract class RepositoryImpl implements Repository, StringConstants {

    private class ArtifactDescriptorImpl implements ArtifactDescriptor {

        private final String description;
        private final String path;
        private final boolean readOnly;
        private final Repository repository;
        private final String type;
        private final String version;

        ArtifactDescriptorImpl( final String artifactType,
                                final String artifactDescription,
                                final String artifactPath,
                                final Repository artifactRepository,
                                final String artifactVersion,
                                final boolean artifactReadOnly ) {
            this.type = artifactType;
            this.description = artifactDescription;
            this.path = artifactPath;
            this.repository = artifactRepository;
            this.version = artifactVersion;
            this.readOnly = true;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.ArtifactDescriptor#getArtifactType()
         */
        @Override
        public String getArtifactType() {
            return this.type;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.ArtifactDescriptor#getDescription()
         */
        @Override
        public String getDescription() {
            return this.description;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.ArtifactDescriptor#getPath()
         */
        @Override
        public String getPath() {
            return this.path;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.ArtifactDescriptor#getRepository()
         */
        @Override
        public Repository getRepository() {
            return this.repository;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.ArtifactDescriptor#getVersion()
         */
        @Override
        public String getVersion() {
            return this.version;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.ArtifactDescriptor#isReadOnly()
         */
        @Override
        public boolean isReadOnly() {
            return this.readOnly;
        }

    }

    private class ArtifactImpl implements Artifact {

        private final ArtifactDescriptor descriptor;
        private final KomodoObject komodoObject;

        ArtifactImpl( final ArtifactDescriptor descriptor,
                      final KomodoObject komodoObject ) {
            this.descriptor = descriptor;
            this.komodoObject = komodoObject;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Artifact#get()
         */
        @Override
        public KomodoObject get() {
            return this.komodoObject;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Artifact#getDescriptor()
         */
        @Override
        public ArtifactDescriptor getDescriptor() {
            return this.descriptor;
        }

    }

    /**
     * A unit of work analogous to a transaction.
     */
    public static class UnitOfWorkImpl implements UnitOfWork {

        protected final UnitOfWorkListener callback;
        protected KException error;
        protected final String name;
        protected final boolean rollbackOnly;
        protected Session session;
        protected State state = State.NOT_STARTED;

        /**
         * @param uowName
         *        the transaction name (cannot be empty)
         * @param uowSession
         *        the repository session this unit of work will be using (cannot be <code>null</code>)
         * @param uowRollbackOnly
         *        <code>true</code> if only a rollback can be done (i.e., commit not allowed)
         * @param listener
         *        the callback (can be <code>null</code>)
         */
        public UnitOfWorkImpl( final String uowName,
                               final Session uowSession,
                               final boolean uowRollbackOnly,
                               final UnitOfWorkListener listener) {
            ArgCheck.isNotEmpty(uowName, "uowName"); //$NON-NLS-1$
            ArgCheck.isNotNull(uowSession, "uowSession"); //$NON-NLS-1$

            this.name = uowName;
            this.session = uowSession;
            this.rollbackOnly = uowRollbackOnly;
            this.callback = listener;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#commit()
         */
        @Override
        public void commit() {
            if (this.state != State.NOT_STARTED) {
                this.error = new KException( Messages.getString( Messages.Komodo.ERROR_TRANSACTION_FINISHED,
                                                                 this.name,
                                                                 this.state ) );
                this.state = State.ERROR;
            } else {
                LOGGER.debug( "commit transaction {0}", getName() ); //$NON-NLS-1$

                if (this.rollbackOnly) {
                    rollback();
                } else {
                    this.state = State.RUNNING;

                    try {
                        if (this.session == null) {
                            this.state = State.ERROR;
                            this.error = new KException( Messages.getString( Messages.Komodo.ERROR_SESSION_IS_CLOSED, this.name ) );
                        } else {
                            this.session.save();

                            this.state = State.COMMITTED;
                            LOGGER.debug( "transaction {0} saved", getName() ); //$NON-NLS-1$

                            if (this.callback != null) {
                                this.callback.respond( this );
                            }
                        }
                    } catch (final Exception e) {
                        this.state = State.ERROR;
                        this.error = new KException( e );

                        if (this.callback == null) {
                            LOGGER.error( Messages.getString( Messages.Komodo.ERROR_TRYING_TO_COMMIT, e, getName() ) );
                            rollback();
                            this.state = State.ERROR;
                        } else {
                            this.callback.errorOccurred( e );
                        }
                    } finally {
                        if (session.isLive()) this.session.logout();
                        this.session = null;
                    }
                }
            }
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getCallback()
         */
        @Override
        public UnitOfWorkListener getCallback() {
            return this.callback;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getError()
         */
        @Override
        public KException getError() {
            return this.error;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getName()
         */
        @Override
        public String getName() {
            return this.name;
        }

        /**
         * @return the JCR session used during the transaction (never <code>null</code>)
         */
        public Session getSession() {
            return this.session;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getState()
         */
        @Override
        public State getState() {
            return this.state;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#hasChanges()
         */
        @Override
        public boolean hasChanges() throws KException {
            if ( this.state == State.NOT_STARTED ) {
                try {
                    return ( ( this.session != null ) && this.session.isLive() && this.session.hasPendingChanges() );
                } catch ( final RepositoryException e ) {
                    throw new KException( Messages.getString( ERROR_REPO_HAS_CHANGES, this.name ), e );
                }
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#isRollbackOnly()
         */
        @Override
        public boolean isRollbackOnly() {
            return this.rollbackOnly;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#rollback()
         */
        @Override
        public void rollback() {
            if (this.state != State.NOT_STARTED) {
                this.error = new KException( Messages.getString( Messages.Komodo.ERROR_TRANSACTION_FINISHED,
                                                                 this.name,
                                                                 this.state ) );
                this.state = State.ERROR;
            } else {
                this.state = State.RUNNING;
                LOGGER.debug( "rollback transaction {0}", getName() ); //$NON-NLS-1$

                try {
                    if (this.session == null) {
                        this.state = State.ERROR;
                        this.error = new KException( Messages.getString( Messages.Komodo.ERROR_SESSION_IS_CLOSED, this.name ) );
                    } else {
                        this.session.refresh( false );
                        this.state = State.ROLLED_BACK;
                        LOGGER.debug( "transaction {0} rolled back", getName() ); //$NON-NLS-1$

                        if (this.callback != null) {
                            this.callback.respond( null );
                        }
                    }
                } catch (final Exception e) {
                    this.state = State.ERROR;
                    this.error = new KException( e );

                    if (this.callback == null) {
                        LOGGER.error( Messages.getString( Messages.Komodo.ERROR_TRYING_TO_ROLLBACK, e, getName() ) );
                    } else {
                        this.callback.errorOccurred( e );
                    }
                } finally {
                    if (session.isLive()) this.session.logout();
                    this.session = null;
                }
            }
        }

        /**
         * Decode the given string if it has been encoded by the UnitOfWork implementation
         * @param encoded encoded string
         * @return a decoded string according to the encoding requirements of the implementation
         */
        @Override
        public String decode(String encoded) {
            if (! (this.session instanceof JcrSession))
                return encoded; // session not supported

            return ((JcrSession) session).decode(encoded);
        }
    }

    /**
     * The root path of the repository.
     */
    public static final String REPO_ROOT = FORWARD_SLASH;

    /**
     * The root path of the Komodo repository.
     */
    public static final String KOMODO_ROOT = (REPO_ROOT + Komodo.NODE_TYPE);

    /**
     * The root path of the Komodo repository environment area.
     */
    public static final String ENV_ROOT = ( KOMODO_ROOT + FORWARD_SLASH + Komodo.ENVIRONMENT );

    /**
     * The root path of the Komodo repository library area.
     */
    public static final String LIBRARY_ROOT = (KOMODO_ROOT + FORWARD_SLASH + Komodo.LIBRARY);

    /**
     * The root path of the Komodo repository workspace area.
     */
    public static final String WORKSPACE_ROOT = (KOMODO_ROOT + FORWARD_SLASH + Komodo.WORKSPACE);

    /**
     * The root path of the Komodo repository workspace searches area
     */
    public static final String SEARCHES_ROOT = WORKSPACE_ROOT + FORWARD_SLASH + Search.GROUP_NODE;

    protected static final KLog LOGGER = KLog.getLogger();

    private final Set< RepositoryClient > clients = new HashSet< RepositoryClient >();
    private final Id id;
    private final Set< RepositoryObserver > observers = new HashSet< RepositoryObserver >();
    private final Type type;
    private ValidationManager validationMgr;

    /**
     * @param type
     *        the repository type (cannot be <code>null</code>)
     * @param id
     *        the repository identifier (cannot be <code>null</code>)
     */
    public RepositoryImpl( final Type type,
                           final Id id ) {
        ArgCheck.isNotNull(type, "type"); //$NON-NLS-1$
        ArgCheck.isNotNull(id, "id"); //$NON-NLS-1$

        this.type = type;
        this.id = id;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#add(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject add( final UnitOfWork transaction,
                             final String parentPath,
                             final String name,
                             final String primaryType ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("add: transaction = {0}, parentPath = {1}, name = {2}", //$NON-NLS-1$
                         transaction.getName(),
                         parentPath,
                         name);
        }

        final String workspacePath = getAbsoluteWorkspacePath(parentPath);
        final Session session = getSession(transaction);

        try {
            if (WORKSPACE_ROOT.equals(workspacePath) && !session.nodeExists(WORKSPACE_ROOT)) {
                komodoWorkspace(transaction);
            }

            final Node parent = session.getNode(workspacePath);
            final Node newNode = parent.addNode(name, primaryType);
            final KomodoObject result = new ObjectImpl(this, newNode.getPath(), 0);

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug( "RepositoryImpl.add: transaction = {0}, node name = {1}, index = {2}", //$NON-NLS-1$
                              transaction.getName(),
                              newNode.getName(),
                              newNode.getIndex() );
            }

            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#addClient(org.komodo.spi.repository.RepositoryClient)
     */
    @Override
    public void addClient( final RepositoryClient client ) {
        ArgCheck.isNotNull(client, "client"); //$NON-NLS-1$
        this.clients.add(client);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#addObserver(org.komodo.spi.repository.RepositoryObserver)
     */
    @Override
    public void addObserver( final RepositoryObserver observer ) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.add(observer);
    }

    private void copy( final UnitOfWork uow,
                       final KomodoObject komodoObject,
                       final Node node,
                       final ValueFactory factory ) throws Exception {
        copyProperties(uow, komodoObject, node, factory);

        for (final KomodoObject child : komodoObject.getChildren(uow)) {
            final Node childNode = node.addNode(child.getName(uow), child.getPrimaryType(uow).getName());
            copy(uow, child, childNode, factory);
        }
    }

    private void copyProperties( final UnitOfWork uow,
                                 final KomodoObject komodoObject,
                                 final Node node,
                                 final ValueFactory factory ) throws Exception {
        for (final String name : komodoObject.getPropertyNames(uow)) {
            final Property prop = komodoObject.getProperty(uow, name);
            final int type = PropertyDescriptorImpl.convert(prop.getDescriptor(uow).getType());

            if (prop.getDescriptor(uow).isMultiple()) {
                final Value[] values = PropertyImpl.createValues(factory, prop.getValues(uow), type);
                node.setProperty(name, values);
            } else {
                final Value value = PropertyImpl.createValue(factory, prop.getValue(uow), type);
                node.setProperty(name, value);
            }
        }
    }

    private KomodoObject create( final UnitOfWork transaction,
                                 final String absolutePath, final String nodeType ) throws KException {
        final Session session = getSession(transaction);

        try {
            Node node;
            if (nodeType == null)
                node = new JcrTools().findOrCreateNode(session, absolutePath);
            else
                node = new JcrTools().findOrCreateNode(session, absolutePath, nodeType);

            final KomodoObject result = new ObjectImpl(this, node.getPath(), node.getIndex());
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#query(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public List< KomodoObject > query( final UnitOfWork transaction,
                                       final String queryStatement ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(queryStatement, "Query statement cannot be empty"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("find: transaction = {0}, query = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         queryStatement);
        }

        final Session session = getSession(transaction);
        List<KomodoObject> results = new ArrayList<KomodoObject>();

        try {
            QueryManager queryMgr = session.getWorkspace().getQueryManager();
            Query query = queryMgr.createQuery(queryStatement, Query.JCR_SQL2);
            QueryResult result = query.execute();

            NodeIterator itr = result.getNodes();
            while (itr.hasNext()) {
                Node node = itr.nextNode();
                results.add(new ObjectImpl(this, node.getPath(), node.getIndex()));
            }
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }

        return results;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#searchByKeyword(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, java.lang.String, org.komodo.spi.repository.Repository.KeywordCriteria, java.lang.String[])
     */
    @Override
    public List< KomodoObject > searchByKeyword( final UnitOfWork transaction,
                                                 final String type,
                                                 final String property,
                                                 KeywordCriteria keywordCriteria,
                                                 final String... keywords ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(keywords, "Search by keyword requires keywords"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("find: transaction = {0}, keywords = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         keywords);
        }

        if (keywordCriteria == null)
            keywordCriteria = KeywordCriteria.getDefault();

        ObjectSearcher searcher = new ObjectSearcher(this);
        String typeAlias = "k1"; // where clauses need an alias so assign one to the type //$NON-NLS-1$
        searcher.addFromType(type, typeAlias);
        searcher.addWhereContainsClause(null, typeAlias, property, keywordCriteria, keywords);
        List<KomodoObject> searchObjects = searcher.searchObjects(transaction);

        return searchObjects;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#searchByType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public List< KomodoObject > searchByType( final UnitOfWork transaction,
                                              final String... types ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(types, "types"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("find: transaction = {0}, types = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         types);
        }

        ObjectSearcher searcher = new ObjectSearcher(this);
        for (String type : types) {
            searcher.addFromType(type);
        }

        List<KomodoObject> searchObjects = searcher.searchObjects(transaction);
        return searchObjects;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#searchByPath(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public List< KomodoObject > searchByPath( final UnitOfWork transaction,
                                              final String path ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(path, "path"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("find: transaction = {0}, path = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         path);
        }

        ObjectSearcher searcher = new ObjectSearcher(this);
        String typeAlias = "k1"; // where clauses need an alias so assign one to the type //$NON-NLS-1$
        searcher.addFromType(JcrConstants.NT_UNSTRUCTURED, typeAlias);
        searcher.addWherePathClause(null, typeAlias, path);

        List<KomodoObject> searchObjects = searcher.searchObjects(transaction);
        return searchObjects;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#getFromWorkspace(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getFromWorkspace( final UnitOfWork transaction,
                                          final String path ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        //
        // Transaction has to be committable (hence the 'false') since is it possible that
        // komodoWorkspace() below actually creates the workspace if it doesn't already exist
        //

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("get: transaction = {0}, path = {1}", transaction.getName(), path); //$NON-NLS-1$
        }

        final Session session = getSession(transaction);
        KomodoObject result = null;
        final String workspacePath = getAbsoluteWorkspacePath(path);

        try {
            if (session.nodeExists(workspacePath)) {
                final Node node = session.getNode(workspacePath);
                result = new ObjectImpl(this, workspacePath, node.getIndex());
            } else if (WORKSPACE_ROOT.equals(workspacePath)) {
                result = komodoWorkspace(transaction);
            }

            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    private String getAbsoluteLibraryPath( final String path ) {
        // return root if empty
        if ((path == null) || path.trim().isEmpty()) {
            return LIBRARY_ROOT;
        }

        String nodePath = path.trim();

        if (!nodePath.startsWith(LIBRARY_ROOT)) {
            if (REPO_ROOT.equals(path)) {
                return LIBRARY_ROOT;
            }

            if (nodePath.charAt(0) == File.separatorChar) {
                nodePath = LIBRARY_ROOT + FORWARD_SLASH + nodePath.substring(1); // remove leading slash
            } else {
                nodePath = LIBRARY_ROOT + FORWARD_SLASH + nodePath;
            }
        }

        return nodePath;
    }

    private String getAbsoluteWorkspacePath( final String path ) {
        // return root if empty
        if ((path == null) || path.trim().isEmpty()) {
            return WORKSPACE_ROOT;
        }

        String nodePath = path.trim();

        // return workspace root if path is forward slash
        if (REPO_ROOT.equals(nodePath)) {
            return WORKSPACE_ROOT;
        }

        // if path does not start with workspace root assume a relative path so insert workspace root
        if (!nodePath.startsWith(WORKSPACE_ROOT)) {
            if (nodePath.charAt(0) == File.separatorChar) {
                nodePath = WORKSPACE_ROOT + FORWARD_SLASH + nodePath.substring(1); // remove leading slash
            } else {
                nodePath = WORKSPACE_ROOT + FORWARD_SLASH + nodePath;
            }
        }

        return nodePath;
    }

    private String getAbsoluteWorkspacePath( String path,
                                             final String name ) {
        path = getAbsoluteWorkspacePath(path);
        return (path.endsWith("/") ? (path + name) : (path + FORWARD_SLASH + name)); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#getId()
     */
    @Override
    public Id getId() {
        return this.id;
    }

    private Session getSession( final UnitOfWork transaction ) {
        return ((UnitOfWorkImpl)transaction).getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#getType()
     */
    @Override
    public Type getType() {
        return this.type;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#getUsingId(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getUsingId( final UnitOfWork transaction,
                                    final String jcrUuid ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(jcrUuid, "jcrUuid"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getUsingId: transaction = {0}, uuid = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         jcrUuid);
        }

        final String sql = "SELECT * FROM [nt:unstructured] WHERE [jcr:uuid] = '" + jcrUuid + "'"; //$NON-NLS-1$ //$NON-NLS-2$

        try {
            assert (transaction instanceof UnitOfWorkImpl);
            final Query query = ((UnitOfWorkImpl)transaction).getSession().getWorkspace().getQueryManager().createQuery(sql, Query.JCR_SQL2);
            final QueryResult result = query.execute();
            final NodeIterator itr = result.getNodes();

            if (itr.getSize() == 0) {
                return null;
            }

            if (itr.getSize() == 1) {
                final Node node = itr.nextNode();
                return new ObjectImpl(this, node.getPath(), node.getIndex());
            }

            throw new KException(Messages.getString(Messages.Komodo.DUPLICATE_OBJECT_ERROR, jcrUuid));
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#getValidationManager()
     */
    @Override
    public ValidationManager getValidationManager() {
        if (this.validationMgr == null) {
            this.validationMgr = new ValidationManagerImpl( this );
        }

        return this.validationMgr;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#importFile(org.komodo.spi.repository.Repository.UnitOfWork, java.io.File,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject importFile( final UnitOfWork transaction,
                                    final File file,
                                    final String name,
                                    final String parentPath ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(file, "file"); //$NON-NLS-1$

        try {
            return importResource(transaction, file.toURI().toURL(), name, parentPath);
        } catch (final MalformedURLException e) {
            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#importResource(org.komodo.spi.repository.Repository.UnitOfWork, java.net.URL,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject importResource( final UnitOfWork transaction,
                                        final URL url,
                                        final String name,
                                        final String parentPath ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(url, "url"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("importResource: URL = {0}, name = {1}, parent = {2}, transaction = {3}", //$NON-NLS-1$
                         url,
                         name,
                         parentPath,
                         transaction.getName());
        }

        try {
            final Node parent = new JcrTools().findOrCreateNode(getSession(transaction), getAbsoluteWorkspacePath(parentPath));
            final Node newNode = parent.addNode(name);
            new JcrTools().uploadFile(getSession(transaction),
                                      getAbsoluteWorkspacePath(newNode.getPath(), WorkspaceItem.ORIGINAL_FILE),
                                      url.openStream());
            newNode.addMixin(WorkspaceItem.MIXIN_TYPE);
            final KomodoObject result = new ObjectImpl(this, newNode.getPath(), 0);

            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#notify(org.komodo.spi.repository.RepositoryClientEvent)
     */
    @Override
    public void notify( final RepositoryClientEvent event ) {
        // nothing to do
    }

    protected void notifyObservers() {
        final Set<RepositoryObserver> copy = new HashSet<>(this.observers);

        for (final RepositoryObserver observer : copy) {
            try {
                // Ensure all observers are informed even if one throws an exception
                observer.eventOccurred();
            } catch (final Exception ex) {
                KEngine.getInstance().getErrorHandler().error(Messages.getString(Messages.LocalRepository.General_Exception), ex);
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#publish(org.komodo.spi.repository.Repository.UnitOfWork, boolean,
     *      org.komodo.spi.repository.ArtifactDescriptor, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public void publish( final UnitOfWork transaction,
                         final boolean overwrite,
                         final ArtifactDescriptor descriptor,
                         final KomodoObject komodoObject ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(descriptor, "descriptor"); //$NON-NLS-1$
        ArgCheck.isNotNull(komodoObject, "komodoObject"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("publish: overwrite = {0}, name = {1}, library path = {2}, transaction = {3}", //$NON-NLS-1$
                         overwrite,
                         komodoObject,
                         descriptor.getPath(),
                         transaction.getName());
        }

        final Session session = getSession(transaction);

        try {
            final boolean exists = session.itemExists(descriptor.getPath());
            Node node = null;

            if (exists) {
                if (!overwrite) {
                    throw new KException(Messages.getString(Messages.Komodo.ARTIFACT_EXISTS_ERROR, descriptor.getPath()));
                }

                node = session.getNode(descriptor.getPath());

                { // remove children
                    final NodeIterator itr = node.getNodes();

                    while (itr.hasNext()) {
                        itr.nextNode().remove();
                    }
                }

                { // remove properties
                    final PropertyIterator itr = node.getProperties();

                    while (itr.hasNext()) {
                        itr.nextProperty().remove();
                    }
                }
            } else {
                node = session.getRootNode().addNode(descriptor.getPath());
                node.addMixin(LibraryComponent.MIXIN_TYPE);
            }

            node.setProperty(LibraryComponent.DESCRIPTION, descriptor.getDescription());
            node.setPrimaryType(descriptor.getArtifactType());

            // TODO not sure how version works??

            // copy node
            copy(transaction, komodoObject, node, session.getValueFactory());
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(Messages.getString(Messages.Komodo.ERROR_ADDING_ARTIFACT, komodoObject, descriptor.getPath()), e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#remove(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public void remove( final UnitOfWork transaction,
                        final String... paths ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( paths, "paths" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("remove: paths = {0}, transaction = {1}", //$NON-NLS-1$
                         Arrays.asList(paths),
                         transaction.getName());
        }

        final Session session = getSession( transaction );

        // first make sure all exist
        for ( final String path : paths ) {
            String absPath = null;

            try {
                ArgCheck.isNotNull( path, "path" ); //$NON-NLS-1$
                absPath = getAbsoluteWorkspacePath( path );

                if ( !session.itemExists( absPath ) ) {
                    throw new KException( Messages.getString( Messages.Komodo.UNABLE_TO_REMOVE_NON_EXISTENT_WORKSPACE_ITEM,
                                                              absPath ) );
                }
            } catch ( final Exception e ) {
                if ( e instanceof KException ) {
                    throw ( KException )e;
                }

                throw new KException( Messages.getString( Messages.Komodo.REMOVE_WORKSPACE_OBJECT_ERROR, absPath ), e );
            }
        }

        // all exist so now do the deletes
        for ( final String path : paths ) {
            final String absPath = getAbsoluteWorkspacePath( path );

            try {
                session.removeItem( absPath );
                LOGGER.debug( "removed workspace node at path {0} in transaction {1}", absPath, transaction.getName() ); //$NON-NLS-1$
            } catch ( final Exception e ) {
                if ( e instanceof KException ) {
                    throw ( KException )e;
                }

                throw new KException( Messages.getString( Messages.Komodo.REMOVE_WORKSPACE_OBJECT_ERROR, absPath ), e );
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#removeClient(org.komodo.spi.repository.RepositoryClient)
     */
    @Override
    public void removeClient( final RepositoryClient client ) {
        ArgCheck.isNotNull(client, "client"); //$NON-NLS-1$
        this.clients.remove(client);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#removeObserver(org.komodo.spi.repository.RepositoryObserver)
     */
    @Override
    public void removeObserver( final RepositoryObserver observer ) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.remove(observer);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#retrieve(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Artifact[] retrieve( final UnitOfWork transaction,
                                final String... artifactPaths ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(artifactPaths, "artifactPaths"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("retrieve: paths = {0}, transaction = {1}", //$NON-NLS-1$
                         Arrays.asList(artifactPaths),
                         transaction.getName());
        }

        final Session session = getSession(transaction);
        final Artifact[] artifacts = new Artifact[artifactPaths.length];
        int i = 0;

        try {
            for (final String path : artifactPaths) {
                ArgCheck.isNotNull(path, "path"); //$NON-NLS-1$
                final String absPath = getAbsoluteLibraryPath(path);

                if (session.nodeExists(absPath)) {
                    final Node node = session.getNode(absPath);
                    final String description = node.getProperty(LibraryComponent.DESCRIPTION).getString();
                    final ArtifactDescriptor descriptor = new ArtifactDescriptorImpl(node.getPrimaryNodeType().getName(),
                                                                                     description, path, this, "1", // TODO figure out version //$NON-NLS-1$
                                                                                     true); // TODO figure out how to tell if readonly
                    final KomodoObject komodoObject = new ObjectImpl(this, path, node.getIndex());
                    artifacts[i++] = new ArtifactImpl(descriptor, komodoObject);
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.ARTIFACT_DOES_NOT_EXIST_ERROR, path));
                }
            }

            return artifacts;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#unpublish(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public void unpublish( final UnitOfWork transaction,
                           final String... artifactPaths ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(artifactPaths, "artifactPaths"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("unpublish: artifact paths = {0}, transaction = {1}", //$NON-NLS-1$
                         Arrays.asList(artifactPaths),
                         transaction.getName());
        }

        final Session session = getSession(transaction);

        for (final String path : artifactPaths) {
            ArgCheck.isNotNull(path, "path"); //$NON-NLS-1$
            final String absPath = getAbsoluteLibraryPath(path);

            try {
                if (session.itemExists(absPath)) {
                    session.removeItem(absPath);
                    LOGGER.debug("removed library node at path {0} in transaction {1}", absPath, transaction.getName()); //$NON-NLS-1$
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_UNPUBLISH_NON_EXISTENT_ARTIFACT, absPath));
                }
            } catch (final Exception e) {
                if (e instanceof KException) {
                    throw (KException)e;
                }

                throw new KException(Messages.getString(Messages.Komodo.UNPUBLISH_ARTIFACT_ERROR, absPath), e);
            }
        }
    }

    /**
     * The komodo root in the repository, eg. /tko:komodo
     *
     * @param uow
     *        the transaction (can be <code>null</code> if operation should be automatically committed)
     *
     * @return the root komodo object
     * @throws KException if an error occurs
     */
    protected KomodoObject komodoRoot(final UnitOfWork uow) throws KException {
        return create(uow, KOMODO_ROOT, KomodoLexicon.Komodo.NODE_TYPE);
    }

    @Override
    public KomodoObject komodoSearches(UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        komodoRoot( transaction );
        return create( transaction, SEARCHES_ROOT, KomodoLexicon.Search.GROUP_NODE );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#komodoEnvironment(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject komodoEnvironment( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
        "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        komodoRoot( transaction );
        return create( transaction, ENV_ROOT, KomodoLexicon.Environment.NODE_TYPE );
    }

    @Override
    public KomodoObject komodoLibrary(final UnitOfWork uow) throws KException {
        komodoRoot(uow);
        return create(uow, LIBRARY_ROOT, KomodoLexicon.Library.NODE_TYPE);
    }

    @Override
    public KomodoObject komodoWorkspace(final UnitOfWork uow) throws KException {
        komodoRoot(uow);
        return create(uow, WORKSPACE_ROOT, KomodoLexicon.Workspace.NODE_TYPE);
    }
}
