/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PathNotFoundException;
import javax.jcr.PropertyIterator;
import javax.jcr.PropertyType;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.nodetype.NodeType;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoObjectVisitor;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrNtLexicon;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.api.JcrTools;

/**
 * An implementation of a {@link KomodoObject Komodo object}.
 */
public class ObjectImpl implements KomodoObject, StringConstants {

    private static final KLog LOGGER = KLog.getLogger();

    final int index;
    final String path;
    final Repository repository;

    /**
     * @param komodoRepository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param path
     *        the workspace path (can be empty if object exists at the workspace root)
     * @param index
     *        the object index (value is zero for non-SNS)
     * @throws KException
     *         if an error occurs
     */
    public ObjectImpl( final Repository komodoRepository,
                       final String path,
                       final int index ) throws KException {
        ArgCheck.isNotNull(komodoRepository, "komodoRepository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(path, "path"); //$NON-NLS-1$

        this.repository = komodoRepository;
        this.path = path;
        this.index = index;
    }

    ObjectImpl accessOuter() {
        return this;
    }

    /**
     * Implementation specific and not to be included on the interface!
     *
     * @param uow transaction for this operation. Can be null.
     *
     * @return the underlying object of this {@link ObjectImpl}
     *
     * @throws KException if an error occurs
     */
    protected Node node(UnitOfWork uow) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-node", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("kobject-node: transaction = {0}", uow); //$NON-NLS-1$
        }

        Node node = null;
        Session session = getSession(transaction);
        String absPath = getAbsolutePath();
        PathNotFoundException throwEx = null;

        try {

            //
            // Try finding the node with the conventional path as given
            //
            try {
                node = session.getNode(absPath);
            } catch (PathNotFoundException ex) {
                // node cannot be found with convential path as given
                throwEx = ex;
            }

            if (node == null && session instanceof JcrSession) {
                JcrSession jcrSession = (JcrSession)session;

                //
                // Try finding the node with the path decoded
                //
                try {
                    String decPath = jcrSession.decode(absPath);
                    node = session.getNode(decPath);
                } catch (PathNotFoundException ex) {
                    // node cannot be found with decoded path
                }

                if (node == null) {
                    //
                    // Try finding the node with the path encoded
                    //
                    try {
                        String encPath = jcrSession.encode(absPath);
                        node = session.getNode(encPath);
                    } catch (Exception ex) {
                        // node cannot be found with encoded path
                    }
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            if (node == null) {
                // throw the original path not found exception
                throw throwEx;
            }

            // return the found node
            return node;
        } catch (final Exception e) {
            throw handleError(transaction, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#addChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public KomodoObject addChild( final UnitOfWork uow,
                                  final String name,
                                  final String primaryType ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-addChild", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("kobject-addChild: transaction = {0}, name = {1}, primaryType = '{2}'", //$NON-NLS-1$
                         transaction.getName(),
                         name,
                         primaryType);
        }

        final String type = (StringUtils.isBlank(primaryType) ? JcrNtLexicon.UNSTRUCTURED.getString() : primaryType);

        try {
            final Node node = node(transaction).addNode(name, type);

            if (uow == null) {
                transaction.commit();
            }

            return new ObjectImpl(getRepository(), node.getPath(), node.getIndex());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#addDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void addDescriptor( final UnitOfWork uow,
                               final String... descriptorNames ) throws KException {
        ArgCheck.isNotEmpty(descriptorNames, "descriptorNames"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-addDescriptor", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("kobject-addDescriptor: transaction = {0}, descriptorNames = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         Arrays.asList(descriptorNames));
        }

        try {
            final Node node = getSession(transaction).getNode(this.path);

            for (final String mixin : descriptorNames) {
                ArgCheck.isNotEmpty(mixin, "mixin"); //$NON-NLS-1$
                node.addMixin(mixin);
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object object ) {
        if (object instanceof KomodoObject) {
            return this.path.equals(((ObjectImpl)object).path);
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getAbsolutePath()
     */
    @Override
    public String getAbsolutePath() {
        return this.path;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork uow,
                                  final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final Node node = node(transaction).getNode(name);
            final KomodoObject result = new ObjectImpl(getRepository(), node.getPath(), 0);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final NodeIterator itr = node(transaction).getNodes();
            final KomodoObject[] result = getChildren(transaction, itr);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    private KomodoObject[] getChildren( final UnitOfWork transaction,
                                        final NodeIterator itr ) throws Exception {
        if (!itr.hasNext()) {
            return KomodoObject.EMPTY_ARRAY;
        }

        final KomodoObject[] children = new KomodoObject[(int)itr.getSize()];

        for (int i = 0; itr.hasNext(); ++i) {
            final Node child = itr.nextNode();
            children[i] = new ObjectImpl(getRepository(), child.getPath(), child.getIndex() - 1);
        }

        return children;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildren(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow,
                                       final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        KomodoObject[] result = null;

        try {
            result = getChildren(transaction, node(transaction).getNodes(name));

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) throws KException {
        ArgCheck.isNotEmpty(type, "primaryType"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getChildrenOfType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject[] kids = getChildren(transaction, node(transaction).getNodes());

            if (kids.length != 0) {
                final List< KomodoObject > matches = new ArrayList< KomodoObject >(kids.length);

                for (final KomodoObject kid : kids) {
                    if (type.equals(kid.getPrimaryType(transaction).getName()) || kid.hasDescriptor(transaction, type)) {
                        matches.add(kid);
                    }
                }

                kids = matches.toArray(new KomodoObject[matches.size()]);
            }

            if (uow == null) {
                transaction.commit();
            }

            return kids;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor[] getDescriptors( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getDescriptors", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final Node node = node(transaction);
            final NodeType[] nodeTypes = node.getMixinNodeTypes();
            final Descriptor[] result = new Descriptor[nodeTypes.length];
            int i = 0;

            for (final NodeType nodeType : nodeTypes) {
                result[i++] = new DescriptorImpl(this.repository, nodeType.getName());
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getIndex()
     */
    @Override
    public int getIndex() {
        return this.index;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getName( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getName", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final String result = node(transaction).getName();

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final Node parent = node(transaction).getParent();
            String parentPath = parent.getPath();

            if (!parentPath.endsWith(FORWARD_SLASH)) {
                parentPath += FORWARD_SLASH;
            }

            if (uow == null) {
                transaction.commit();
            }

            return new ObjectImpl(this.repository, parent.getPath(), 0);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor getPrimaryType( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getPrimaryType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final NodeType nodeType = node(transaction).getPrimaryNodeType();

            if (uow == null) {
                transaction.commit();
            }

            return new DescriptorImpl(this.repository, nodeType.getName());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * Convenience method for retrieving a property.
     *
     * @param uow the transaction. If null then a new transaction is generated.
     * @param returnValueType the type of the return value type
     * @param getterName name of the method name calling this method
     * @param propertyPath relative path of the actual property
     * @return the value of the property cast to the specified return value type
     *
     * @throws KException
     */
    protected <T> T getObjectProperty(UnitOfWork uow, Property.ValueType returnValueType,
                                                     String getterName, String propertyPath) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction(getClass().getSimpleName() + HYPHEN + getterName, true, null);
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug(getterName + ": transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Property result = getProperty(transaction, propertyPath);

            if (uow == null) {
                transaction.commit();
            }

            if (result == null) {
                return null;
            }

            switch (returnValueType) {
                case STRING:
                    return (T) result.getStringValue();
                case LONG:
                    return (T) Long.valueOf(result.getLongValue());
                case INTEGER:
                    return (T) Integer.valueOf(Long.valueOf(result.getLongValue()).intValue());
                case BIG_DECIMAL:
                    return (T) result.getDecimalValue();
                case DOUBLE:
                    return (T) Double.valueOf(result.getDoubleValue());
                case BOOLEAN:
                    return (T) Boolean.valueOf(result.getBooleanValue());
                case CALENDAR:
                    return (T) result.getDateValue();
                default:
                    throw new UnsupportedOperationException("Further property types should be added for support in this method"); //$NON-NLS-1$
            }

        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Property getProperty( final UnitOfWork uow,
                                 final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getProperty", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final Node node = node(transaction);
            Property result = null;

            if (node.hasProperty(name)) {
                final javax.jcr.Property jcrProperty = node.getProperty(name);
                result = new PropertyImpl(this.repository, jcrProperty);
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getPropertyNames( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-getPropertyNames", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final List< String > names = new ArrayList< String >();

            for (final PropertyIterator iter = node(transaction).getProperties(); iter.hasNext();) {
                final String name = iter.nextProperty().getName();
                names.add(name);
            }

            if (uow == null) {
                transaction.commit();
            }

            return names.toArray(new String[names.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getRepository()
     */
    @Override
    public Repository getRepository() {
        return this.repository;
    }

    protected Session getSession( final UnitOfWork transaction ) {
        return ((UnitOfWorkImpl)transaction).getSession();
    }

    /**
     * Only one of the {@link UnitOfWork transactions} passed in should be non-<code>null</code>. Ensures that a transaction
     * rollback occurs if the transaction was constructed within the method.
     *
     * @param transactionParameter
     *        the transaction passed into the method (can be <code>null</code>)
     * @param transactionVariable
     *        the transaction constructed within the method (can be <code>null</code>)
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error passed in if already a {@link KException} or the error passed in wrapped in a {@link KException}
     */
    protected KException handleError( final UnitOfWork transactionParameter,
                                      final UnitOfWork transactionVariable,
                                      final Exception e ) {
        assert (e != null);
        assert ((transactionParameter == null) && (transactionVariable != null))
               || ((transactionParameter != null) && (transactionVariable == null));

        if (transactionParameter == null) {
            transactionVariable.rollback();
        }

        if (e instanceof KException) {
            return (KException)e;
        }

        return new KException(e);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork uow,
                             final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-hasChild", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final boolean result = node(transaction).hasNode(name);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-hasChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final boolean result = node(transaction).hasNodes();

            if (uow == null) {
                transaction.commit();
            }
            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasDescriptor( final UnitOfWork uow,
                                  final String descriptorName ) throws KException {
        ArgCheck.isNotEmpty(descriptorName);
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-hasDescriptor", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        boolean result = false;

        try {
            for (final Descriptor descriptor : getDescriptors(transaction)) {
                if (descriptorName.equals(descriptor.getName())) {
                    result = true;
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return this.path.hashCode();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasProperties(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasProperties( final UnitOfWork transaction ) throws KException {
        return (getPropertyNames(transaction).length > 0);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasProperty( final UnitOfWork uow,
                                final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-hasProperty", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final boolean result = node(transaction).hasProperty(name);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#print(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void print( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-print", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final JcrTools tools = new JcrTools(true);
            tools.printSubgraph(node(transaction));

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#removeChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void removeChild( final UnitOfWork uow,
                             final String... names ) throws KException {
        ArgCheck.isNotEmpty(names, "names"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-removeChild", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("kobject-removeChild: transaction = {0}, names = {1}", //$NON-NLS-1$
                                         transaction.getName(),
                                         Arrays.asList(names));
        }

        try {
            final Node node = node(transaction);

            for (final String name : names) {
                if (node.hasNode(name)) {
                    node.getNode(name).remove();
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_CHILD, names, getAbsolutePath()));
                }
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#removeDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void removeDescriptor( final UnitOfWork uow,
                                  final String... descriptorNames ) throws KException {
        ArgCheck.isNotEmpty(descriptorNames, "descriptorNames"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-removeDescriptor", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("kobject-removeDescriptor: transaction = {0}, mixins = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         Arrays.asList(descriptorNames));
        }

        try {
            final Node node = getSession(transaction).getNode(this.path);

            for (final String mixin : descriptorNames) {
                ArgCheck.isNotEmpty(mixin, "mixin"); //$NON-NLS-1$
                node.removeMixin(mixin);
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    void setMultiValuedProperty( final Session session,
                                 final Node node,
                                 final ValueFactory factory,
                                 final String name,
                                 final Object[] propValues,
                                 final int propertyType ) throws Exception {
        final Value[] values = new Value[propValues.length];
        int ndx = 0;

        for (final Object val : propValues) {
            values[ndx++] = PropertyImpl.createValue(factory, val, propertyType);
        }

        node.setProperty(name, values);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#setPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setPrimaryType( final UnitOfWork uow,
                                final String typeName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-setPrimaryType", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setPrimaryType: transaction = '{0}', typeName = '{1}'", transaction.getName(), typeName); //$NON-NLS-1$
        }

        try {
            final String type = (StringUtils.isBlank(typeName) ? JcrNtLexicon.UNSTRUCTURED.getString() : typeName);
            getSession(transaction).getNode(this.path).setPrimaryType(type);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    private void setProperty( final Session session,
                              final Node node,
                              final String name,
                              final Object... values ) throws Exception {
        final ValueFactory factory = session.getValueFactory();
        final boolean exists = node.hasProperty(name);

        // remove property
        if (values == null) {
            if (exists) {
                node.getProperty(name).remove();
            } else {
                throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_PROPERTY_THAT_DOES_NOT_EXIST,
                                                        name,
                                                        getAbsolutePath()));
            }
        } else {
            // must be an array at this point
            final int count = values.length;

            if (exists) {
                final javax.jcr.Property property = node.getProperty(name);
                final int type = property.getType();
                final boolean multiple = property.isMultiple();

                if (count == 0) {
                    if (multiple) {
                        property.remove();
                    } else {
                        throw new KException(
                                             Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_SINGLE_VALUE_PROPERTY_WITH_EMPTY_ARRAY,
                                                                name,
                                                                getAbsolutePath()));
                    }
                } else if (count > 1) {
                    if (multiple) {
                        setMultiValuedProperty(session, node, factory, name, values, type);
                    } else {
                        throw new KException(
                                             Messages.getString(Messages.Komodo.UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
                                                                name,
                                                                getAbsolutePath()));
                    }
                } else {
                    // only one value so set property
                    if (multiple) {
                        setMultiValuedProperty(session, node, factory, name, values, type);
                    } else {
                        // remove if value is null or empty string
                        if ((values[0] == null) || ((values[0] instanceof String) && StringUtils.isBlank((String)values[0]))) {
                            node.getProperty(name).remove();
                        } else {
                            node.setProperty(name, PropertyImpl.createValue(factory, values[0]));
                        }
                    }
                }
            } else {
                // property does not exist and no values being set
                if (count == 0) {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_PROPERTY_THAT_DOES_NOT_EXIST,
                                                            name,
                                                            getAbsolutePath()));
                }

                if (count > 1) {
                    setMultiValuedProperty(session, node, factory, name, values, PropertyType.UNDEFINED);
                } else {
                    if ((values[0] != null) && ((!(values[0] instanceof String)) || !StringUtils.isBlank((String)values[0]))) {
                        node.setProperty(name, PropertyImpl.createValue(factory, values[0]));
                    }
                }
            }
        }
    }

    protected void setObjectProperty(UnitOfWork uow, String setterName, String propertyName,
                                            Object value) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction(getClass().getSimpleName() + HYPHEN + setterName, false, null);
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug(setterName + ": transaction = '{0}', value = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         value);
        }

        try {
            setProperty(transaction, propertyName, value);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#setProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.Object[])
     */
    @Override
    public void setProperty( final UnitOfWork uow,
                             final String propertyName,
                             final Object... values ) throws KException {
        ArgCheck.isNotEmpty(propertyName, "propertyName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("kobject-setProperty", false, null); //$NON-NLS-1$
        }

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setProperty: transaction = '{0}', propertyName = '{1}', value(s) = '{2}'", //$NON-NLS-1$
                         transaction.getName(),
                         propertyName,
                         values);
        }

        try {
            final Session session = getSession(transaction);
            final Node node = session.getNode(getAbsolutePath());
            setProperty(session, node, propertyName, values);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.path;
    }

    @Override
    public void visit(KomodoObjectVisitor visitor) throws Exception {
        visitor.visit(this);
    }
}
