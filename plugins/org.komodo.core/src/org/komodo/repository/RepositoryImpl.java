/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PropertyIterator;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.query.Query;
import javax.jcr.query.QueryManager;
import javax.jcr.query.QueryResult;
import org.komodo.core.KomodoLexicon.DataSource;
import org.komodo.core.KomodoLexicon.Komodo;
import org.komodo.core.KomodoLexicon.LibraryComponent;
import org.komodo.core.KomodoLexicon.Schema;
import org.komodo.core.KomodoLexicon.Vdb;
import org.komodo.core.KomodoLexicon.VdbEntry;
import org.komodo.core.KomodoLexicon.VdbImport;
import org.komodo.core.KomodoLexicon.VdbModel;
import org.komodo.core.KomodoLexicon.VdbModelSource;
import org.komodo.core.KomodoLexicon.VdbTranslator;
import org.komodo.core.KomodoLexicon.WorkspaceItem;
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
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrLexicon;
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
    public static class UnitOfWorkImpl implements Repository.UnitOfWork {

        private final UnitOfWorkListener callback;
        private final String name;
        private final boolean rollbackOnly;
        private Session session;

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
                               final UnitOfWorkListener listener ) {
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
            LOGGER.debug("commit transaction '{0}'", getName()); //$NON-NLS-1$

            if (this.rollbackOnly) {
                rollback();
            } else {
                try {
                    if (this.session == null) {
                        throw new KException(Messages.getString(Messages.Komodo.ERROR_SESSION_IS_CLOSED, this.name));
                    }

                    this.session.save();
                    LOGGER.debug("transaction '{0}' saved", getName()); //$NON-NLS-1$

                    if (this.callback != null) {
                        this.callback.respond(this);
                    }
                } catch (final Exception e) {
                    if (this.callback == null) {
                        LOGGER.error(Messages.getString(Messages.Komodo.ERROR_TRYING_TO_COMMIT, e, getName()));
                        rollback();
                    } else {
                        this.callback.errorOccurred(e);
                    }
                } finally {
                    this.session.logout();
                    this.session = null;
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
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getName()
         */
        @Override
        public String getName() {
            return this.name;
        }

        /**
         * @return the JCR session used during the transaction (never <code>null</code>)
         */
        protected Session getSession() {
            return this.session;
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
            LOGGER.debug("rollback transaction '{0}'", getName()); //$NON-NLS-1$
            try {
                if (this.session == null) {
                    throw new KException(Messages.getString(Messages.Komodo.ERROR_SESSION_IS_CLOSED, this.name));
                }

                this.session.refresh(false);
                LOGGER.debug("transaction '{0}' rolled back", getName()); //$NON-NLS-1$

                if (this.callback != null) {
                    this.callback.respond(null);
                }
            } catch (final Exception e) {
                if (this.callback == null) {
                    LOGGER.error(Messages.getString(Messages.Komodo.ERROR_TRYING_TO_ROLLBACK, e, getName()));
                } else {
                    this.callback.errorOccurred(e);
                }
            } finally {
                this.session.logout();
                this.session = null;
            }
        }

    }

    /**
     * The root path of the Komodo repository.
     */
    static String KOMODO_ROOT = (FORWARD_SLASH + Komodo.NODE_TYPE + FORWARD_SLASH);

    /**
     * The root path of the Komodo repository library.
     */
    public static String LIBRARY_ROOT = (KOMODO_ROOT + Komodo.LIBRARY + FORWARD_SLASH);

    protected static final KLog LOGGER = KLog.getLogger();

    /**
     * The root path of the Komodo repository workspace.
     */
    public static String WORKSPACE_ROOT = (KOMODO_ROOT + Komodo.WORKSPACE + FORWARD_SLASH);

    private final Set< RepositoryClient > clients = new HashSet< RepositoryClient >();
    private final Id id;
    private final Set< RepositoryObserver > observers = new HashSet< RepositoryObserver >();
    private final Type type;

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
    public KomodoObject add( final UnitOfWork uow,
                             final String parentPath,
                             final String name,
                             final String primaryType ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repositoryimpl-add", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("add: transaction = '{0}', parentPath = '{1}', name = '{2}'", //$NON-NLS-1$
                         transaction.getName(),
                         parentPath,
                         name);
        }

        final String workspacePath = getAbsoluteWorkspacePath(parentPath);
        final Session session = getSession(transaction);

        try {
            if (WORKSPACE_ROOT.equals(workspacePath) && !session.nodeExists(WORKSPACE_ROOT)) {
                create(transaction, WORKSPACE_ROOT);
            }

            final Node parent = session.getNode(workspacePath);
            final Node newNode = parent.addNode(name, primaryType);
            final KomodoObject result = new ObjectImpl(this, newNode.getPath(), 0);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            if (uow == null) {
                transaction.rollback();
            }

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
            final int type = PropertyDescriptorImpl.convert(prop.getDescriptor().getType());

            if (prop.getDescriptor().isMultiple()) {
                final Value[] values = PropertyImpl.createValues(factory, prop.getValues(), type);
                node.setProperty(name, values);
            } else {
                final Value value = PropertyImpl.createValue(factory, prop.getValue(), type);
                node.setProperty(name, value);
            }
        }
    }

    private KomodoObject create( final UnitOfWork transaction,
                                 final String absolutePath ) throws KException {
        assert (transaction != null);
        final Session session = getSession(transaction);

        try {
            final Node node = new JcrTools().findOrCreateNode(session, WORKSPACE_ROOT);
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
     * @see org.komodo.spi.repository.Repository#find(org.komodo.spi.repository.Repository.UnitOfWork, java.util.List,
     *      org.komodo.spi.repository.Repository.KeywordCriteria, java.lang.String[])
     */
    @Override
    public ArtifactDescriptor[] find( final UnitOfWork uow,
                                      final List< String > keywords,
                                      final KeywordCriteria criteria,
                                      final String... artifactTypes ) throws KException {
        // TODO do we need to consider wildcards in the keywords??
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repositoryimpl-find", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("find: transaction = '{0}', keywords = '{1}', criteria = '{2}'", //$NON-NLS-1$
                         transaction.getName(),
                         keywords,
                         criteria,
                         (artifactTypes == null) ? "ALL" : Arrays.asList(artifactTypes)); //$NON-NLS-1$
        }

        try {
            KeywordCriteria searchCriteria = criteria;
            final Set< String > paths = new HashSet< String >(10);
            final String ALIAS = "ARTIFACT"; //$NON-NLS-1$

            boolean hasArtifactTypes = false;
            final StringBuilder whereClause = new StringBuilder("WHERE "); //$NON-NLS-1$

            // artifact type
            if ((artifactTypes != null) && (artifactTypes.length != 0)) {
                hasArtifactTypes = true;
                whereClause.append(ALIAS).append(".[").append(JcrLexicon.PRIMARY_TYPE.getString()).append(" IN ("); //$NON-NLS-1$ //$NON-NLS-2$

                boolean firstTime = true;

                for (final String artifactType : artifactTypes) {
                    if (!firstTime) {
                        whereClause.append(',');
                    }

                    if (Schema.NODE_TYPE.equals(artifactType)) {
                        paths.add(Schema.GROUP_NODE);
                        whereClause.append(Schema.NODE_TYPE);
                    } else if (Vdb.NODE_TYPE.equals(artifactType)) {
                        paths.add(Vdb.GROUP_NODE);
                        whereClause.append(Vdb.NODE_TYPE);
                    } else if (VdbImport.NODE_TYPE.equals(artifactType)) {
                        paths.add(VdbImport.GROUP_NODE);
                        whereClause.append(VdbImport.NODE_TYPE);
                    } else if (VdbModel.NODE_TYPE.equals(artifactType)) {
                        paths.add(VdbModel.GROUP_NODE);
                        whereClause.append(VdbModel.NODE_TYPE);
                    } else if (VdbModelSource.NODE_TYPE.equals(artifactType)) {
                        paths.add(VdbModelSource.GROUP_NODE);
                        whereClause.append(VdbModelSource.NODE_TYPE);
                    } else if (VdbTranslator.NODE_TYPE.equals(artifactType)) {
                        paths.add(VdbTranslator.GROUP_NODE);
                        whereClause.append(VdbTranslator.NODE_TYPE);
                    } else if (VdbEntry.NODE_TYPE.equals(artifactType)) {
                        paths.add(VdbEntry.GROUP_NODE);
                        whereClause.append(VdbEntry.NODE_TYPE);
                    } else if (DataSource.NODE_TYPE.equals(artifactType)) {
                        paths.add(DataSource.GROUP_NODE);
                        whereClause.append(DataSource.NODE_TYPE);
                    }

                    firstTime = false;
                }

                { // add paths to the where clause
                    whereClause.append(") AND PATH() IN ("); //$NON-NLS-1$
                    firstTime = true;

                    for (final String path : paths) {
                        if (!firstTime) {
                            whereClause.append(',');
                        }

                        whereClause.append(path);
                        firstTime = false;
                    }

                    whereClause.append(")"); //$NON-NLS-1$
                }
            }

            // keywords
            boolean hasKeywords = false;

            if ((keywords != null) && !keywords.isEmpty()) {
                hasKeywords = true;

                if (hasArtifactTypes) {
                    whereClause.append(" AND "); //$NON-NLS-1$
                }

                // "CONTAINS(ARTIFACT.[tko:description],'foo' 'bar')"
                whereClause.append("CONTAINS(").append(ALIAS).append(".[").append(LibraryComponent.DESCRIPTION).append("],"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

                if (searchCriteria == null) {
                    searchCriteria = KeywordCriteria.getDefault();
                }

                boolean firstTime = true;

                for (final String keyword : keywords) {
                    if (!firstTime && (KeywordCriteria.ANY == searchCriteria)) {
                        whereClause.append(" OR "); //$NON-NLS-1$
                    }

                    if (KeywordCriteria.NONE == searchCriteria) {
                        whereClause.append('-');
                    }

                    whereClause.append('\'').append(keyword).append('\'');
                    firstTime = false;
                }
            }

            // construct query
            final String COLUMNS = JcrLexicon.PRIMARY_TYPE.getString() + ", " + LibraryComponent.DESCRIPTION; // TODO need other columns //$NON-NLS-1$
            String selectStmt = "SELECT " + COLUMNS + " FROM [" + LibraryComponent.MIXIN_TYPE + "] " + ALIAS + ' '; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

            if (hasArtifactTypes || hasKeywords) {
                selectStmt += whereClause;
            }

            LOGGER.debug("Artifact query: ", selectStmt); //$NON-NLS-1$

            // run query
            final Session session = getSession(transaction);
            final QueryManager queryMgr = session.getWorkspace().getQueryManager();
            final Query query = queryMgr.createQuery(Query.JCR_SQL2, selectStmt);
            final QueryResult resultSet = query.execute();

            // create results
            ArtifactDescriptor[] result = null;
            final NodeIterator itr = resultSet.getNodes();

            if (itr.hasNext()) {
                result = new ArtifactDescriptor[(int)itr.getSize()];
                int i = 0;

                while (itr.hasNext()) {
                    final Node node = itr.nextNode();

                    // description
                    String description = null;

                    if (node.hasProperty(LibraryComponent.DESCRIPTION)) {
                        description = node.getProperty(LibraryComponent.DESCRIPTION).getString();
                    } else {
                        description = Messages.getString(Messages.Komodo.NO_ARTIFACT_DESCRIPTION, node.getName());
                    }

                    // version
                    String version = null;

                    if (node.hasProperty(JcrLexicon.VERSION_LABELS.getString())) {
                        final Value[] labels = node.getProperty(JcrLexicon.VERSION_LABELS.getString()).getValues();
                        version = labels[0].getString(); // TODO not sure how versions work??
                    } else {
                        // TODO not sure what to do here
                        version = "1.0"; //$NON-NLS-1$
                    }

                    result[i++] = new ArtifactDescriptorImpl(node.getPrimaryNodeType().getName(), description, node.getPath(),
                                                             this, version, true); // TODO figure out how to tell if readonly
                }
            } else {
                result = ArtifactDescriptor.EMPTY;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            if (uow == null) {
                transaction.rollback();
            }

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#find(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public ArtifactDescriptor[] find( final UnitOfWork transaction,
                                      final String... artifactTypes ) throws KException {
        return find(transaction, null, null, artifactTypes);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Repository#get(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject get( final UnitOfWork uow,
                             final String path ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repositoryimpl-get", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("get: transaction = '{0}', path = '{1}'", transaction.getName(), path); //$NON-NLS-1$
        }

        final Session session = getSession(transaction);
        KomodoObject result = null;
        final String workspacePath = getAbsoluteWorkspacePath(path);

        try {
            if (session.nodeExists(workspacePath)) {
                final Node node = session.getNode(workspacePath);
                result = new ObjectImpl(this, workspacePath, node.getIndex());
            } else if (WORKSPACE_ROOT.equals(workspacePath)) {
                result = create(transaction, WORKSPACE_ROOT);
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            if (uow == null) {
                transaction.rollback();
            }

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
            if ("/".equals(path)) { //$NON-NLS-1$
                return LIBRARY_ROOT;
            }

            if (nodePath.charAt(0) == '/') {
                nodePath = LIBRARY_ROOT + nodePath.substring(1); // remove leading slash
            } else {
                nodePath = LIBRARY_ROOT + nodePath;
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

        if (!nodePath.startsWith(WORKSPACE_ROOT)) {
            if ("/".equals(path)) { //$NON-NLS-1$
                return WORKSPACE_ROOT;
            }

            if (nodePath.charAt(0) == '/') {
                nodePath = WORKSPACE_ROOT + nodePath.substring(1); // remove leading slash
            } else {
                nodePath = WORKSPACE_ROOT + nodePath;
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
    public KomodoObject getUsingId( final UnitOfWork uow,
                                    final String jcrUuid ) throws KException {
        ArgCheck.isNotEmpty(jcrUuid, "jcrUuid"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repositoryimpl-getUsingId", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getUsingId: transaction = '{0}', uuid = '{1}'", //$NON-NLS-1$
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
            if (uow == null) {
                transaction.rollback();
            }

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
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
    public KomodoObject importResource( final UnitOfWork uow,
                                        final URL url,
                                        final String name,
                                        final String parentPath ) throws KException {
        ArgCheck.isNotNull(url, "url"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repositoryimpl-importResource", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("importResource: URL = '{0}', name = '{1}', parent = '{2}', transaction = '{3}'", //$NON-NLS-1$
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

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            if (uow == null) {
                transaction.rollback();
            }

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
        for (final RepositoryObserver observer : this.observers) {
            try {
                // Ensure all observers are informed even if one throws an exception
                observer.stateChanged();
            } catch (final Exception ex) {
                KLog.getLogger().error(Messages.getString(Messages.LocalRepository.General_Exception), ex);
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
    public void publish( final UnitOfWork uow,
                         final boolean overwrite,
                         final ArtifactDescriptor descriptor,
                         final KomodoObject komodoObject ) throws KException {
        ArgCheck.isNotNull(descriptor, "descriptor"); //$NON-NLS-1$
        ArgCheck.isNotNull(komodoObject, "komodoObject"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repositoryimpl-publish", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("publish: overwrite = '{0}', name = '{1}', library path = '{2}', transaction = '{3}'", //$NON-NLS-1$
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

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            if (uow == null) {
                transaction.rollback();
            }

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
    public void remove( final UnitOfWork uow,
                        final String... paths ) throws KException {
        ArgCheck.isNotEmpty(paths, "paths"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repositoryimpl-publish", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("remove: paths = '{0}', transaction = '{1}'", //$NON-NLS-1$
                         Arrays.asList(paths),
                         transaction.getName());
        }

        final Session session = getSession(transaction);

        for (final String path : paths) {
            ArgCheck.isNotNull(path, "path"); //$NON-NLS-1$
            final String absPath = getAbsoluteWorkspacePath(path);

            try {
                if (session.itemExists(absPath)) {
                    session.removeItem(absPath);
                    LOGGER.debug("removed workspace node at path '{0}' in transaction '{1}'", absPath, transaction.getName()); //$NON-NLS-1$
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_NON_EXISTENT_WORKSPACE_ITEM, absPath));
                }

                if (uow == null) {
                    transaction.commit();
                }
            } catch (final Exception e) {
                if (uow == null) {
                    transaction.rollback();
                }

                if (e instanceof KException) {
                    throw (KException)e;
                }

                throw new KException(Messages.getString(Messages.Komodo.REMOVE_WORKSPACE_OBJECT_ERROR, absPath), e);
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
    public Artifact[] retrieve( final UnitOfWork uow,
                                final String... artifactPaths ) throws KException {
        ArgCheck.isNotEmpty(artifactPaths, "artifactPaths"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repository-retrieve", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("retrieve: paths = '{0}', transaction = '{1}'", //$NON-NLS-1$
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

            if (uow == null) {
                transaction.commit();
            }

            return artifacts;
        } catch (final Exception e) {
            if (uow == null) {
                transaction.rollback();
            }

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
    public void unpublish( final UnitOfWork uow,
                           final String... artifactPaths ) throws KException {
        ArgCheck.isNotEmpty(artifactPaths, "artifactPaths"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = createTransaction("repository-unpublish", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("unpublish: artifact paths = '{0}', transaction = '{1}'", //$NON-NLS-1$
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
                    LOGGER.debug("removed library node at path '{0}' in transaction '{1}'", absPath, transaction.getName()); //$NON-NLS-1$
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_UNPUBLISH_NON_EXISTENT_ARTIFACT, absPath));
                }

                if (uow == null) {
                    transaction.commit();
                }
            } catch (final Exception e) {
                if (uow == null) {
                    transaction.rollback();
                }

                if (e instanceof KException) {
                    throw (KException)e;
                }

                throw new KException(Messages.getString(Messages.Komodo.UNPUBLISH_ARTIFACT_ERROR, absPath), e);
            }
        }

    }

}
