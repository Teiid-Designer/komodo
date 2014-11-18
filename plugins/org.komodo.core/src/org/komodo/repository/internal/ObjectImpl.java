/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.repository.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PropertyIterator;
import javax.jcr.PropertyType;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.nodetype.NodeType;
import org.komodo.core.Messages;
import org.komodo.repository.internal.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrNtLexicon;
import org.modeshape.jcr.api.JcrTools;

/**
 * An implementation of a {@link KomodoObject Komodo object}.
 */
class ObjectImpl implements KomodoObject {

    private static final KLog LOGGER = KLog.getLogger();

    final int index;
    final String path;
    final Repository repository;

    ObjectImpl( final Repository komodoRepository,
                final String path,
                final int index ) {
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
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#addChild(java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject addChild( final String name,
                                  final String primaryType ) throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-addChild", false, null); //$NON-NLS-1$

        try {
            final KomodoObject result = addChild(transaction, name, primaryType);
            transaction.commit();
            return result;
        } catch (final KException e) {
            transaction.rollback();
            throw e;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#addChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public KomodoObject addChild( final UnitOfWork transaction,
                                  final String name,
                                  final String primaryType ) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        LOGGER.debug("kobject-addChild: transaction = {0}, name = {1}, primaryType = {2}", transaction.getName(), name, primaryType); //$NON-NLS-1$

        final String type = (StringUtils.isBlank(primaryType) ? JcrNtLexicon.UNSTRUCTURED.getString() : primaryType);

        try {
            final Node node = getSession(transaction).getNode(getAbsolutePath()).addNode(name, type);
            return new ObjectImpl(getRepository(), node.getPath(), node.getIndex());
        } catch (Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#addMixin(java.lang.String[])
     */
    @Override
    public void addMixin( String... mixins ) throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-addMixin", false, null); //$NON-NLS-1$

        try {
            addMixin(transaction, mixins);
            transaction.commit();
        } catch (final KException e) {
            transaction.rollback();
            throw e;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#addMixin(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public void addMixin( final UnitOfWork transaction,
                          final String... mixins ) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(mixins, "mixins"); //$NON-NLS-1$
        LOGGER.debug("kobject-addMixin: transaction = {0}, mixins = {1}", transaction.getName(), Arrays.asList(mixins)); //$NON-NLS-1$

        try {
            final Node node = getSession(transaction).getNode(this.path);

            for (final String mixin : mixins) {
                ArgCheck.isNotEmpty(mixin, "mixin"); //$NON-NLS-1$
                node.addMixin(mixin);
            }
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
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object object ) {
        if (getClass() != object.getClass()) return false;
        return this.path.equals(((ObjectImpl)object).path);
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
     * @see org.komodo.spi.repository.KomodoObject#getChild(java.lang.String)
     */
    @Override
    public KomodoObject getChild( final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        final UnitOfWork transaction = this.repository.createTransaction("kobject-getChild", true, null); //$NON-NLS-1$
        KomodoObject result = null;

        try {
            result = new ObjectImpl(getRepository(), getSession(transaction).getNode(getAbsolutePath()).getNode(name).getPath(),
                                    0);
            transaction.commit();
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildren()
     */
    @Override
    public KomodoObject[] getChildren() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-getChildren", true, null); //$NON-NLS-1$

        try {
            final NodeIterator itr = getSession(transaction).getNode(getAbsolutePath()).getNodes();
            KomodoObject[] result = getChildren(itr);
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    private KomodoObject[] getChildren( final NodeIterator itr ) throws Exception {
        if (!itr.hasNext()) return KomodoObject.EMPTY_ARRAY;

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
     * @see org.komodo.spi.repository.KomodoObject#getChildren(java.lang.String)
     */
    @Override
    public KomodoObject[] getChildren( final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        final UnitOfWork transaction = this.repository.createTransaction("kobject-getChildren", true, null); //$NON-NLS-1$
        KomodoObject[] result = null;

        try {
            result = getChildren(getSession(transaction).getNode(getAbsolutePath()).getNodes(name));
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildrenOfType(java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final String primaryType ) throws KException {
        ArgCheck.isNotEmpty(primaryType, "primaryType"); //$NON-NLS-1$

        final UnitOfWork transaction = this.repository.createTransaction("kobject-getChildrenOfType", true, null); //$NON-NLS-1$

        try {
            KomodoObject[] kids = getChildren(getSession(transaction).getNode(getAbsolutePath()).getNodes());

            if (kids.length != 0) {
                final List< KomodoObject > matches = new ArrayList< KomodoObject >(kids.length);

                for (final KomodoObject kid : kids) {
                    if (primaryType.equals(kid.getPrimaryType())) {
                        matches.add(kid);
                    }
                }

                kids = matches.toArray(new KomodoObject[matches.size()]);
            }

            transaction.commit();
            return kids;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
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
     * @see org.komodo.spi.repository.KomodoObject#getDescriptors()
     */
    @Override
    public Descriptor[] getDescriptors() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-getMixins", true, null); //$NON-NLS-1$

        try {
            final Node node = getSession(transaction).getNode(getAbsolutePath());
            final NodeType[] nodeTypes = node.getMixinNodeTypes();
            final Descriptor[] result = new Descriptor[nodeTypes.length];
            int i = 0;

            for (final NodeType nodeType : nodeTypes) {
                result[i++] = new DescriptorImpl(this.repository, nodeType.getName());
            }

            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getName()
     */
    @Override
    public String getName() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-getName", true, null); //$NON-NLS-1$

        try {
            final String result = getSession(transaction).getNode(getAbsolutePath()).getName();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent()
     */
    @Override
    public KomodoObject getParent() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-getParent", true, null); //$NON-NLS-1$

        try {
            final Node parent = getSession(transaction).getNode(getAbsolutePath()).getParent();
            transaction.commit();

            String parentPath =  parent.getPath();

            if (!parentPath.endsWith("/")) { //$NON-NLS-1$
                parentPath += "/"; //$NON-NLS-1$
            }

            if (RepositoryImpl.WORKSPACE_ROOT.equals(parentPath)) {
                return null;
            }

            return new ObjectImpl(this.repository, parent.getPath(), 0);
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getPrimaryType()
     */
    @Override
    public Descriptor getPrimaryType() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-getPrimaryType", true, null); //$NON-NLS-1$

        try {
            final NodeType nodeType = getSession(transaction).getNode(getAbsolutePath()).getPrimaryNodeType();
            transaction.commit();
            return new DescriptorImpl(this.repository, nodeType.getName());
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getProperty(java.lang.String)
     */
    @Override
    public Property getProperty( final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        final UnitOfWork transaction = this.repository.createTransaction("kobject-getProperty", true, null); //$NON-NLS-1$

        try {
            final Node node = getSession(transaction).getNode(getAbsolutePath());
            Property result = null;

            if (node.hasProperty(name)) {
                final javax.jcr.Property jcrProperty = getSession(transaction).getNode(getAbsolutePath()).getProperty(name);
                result = new PropertyImpl(this, jcrProperty);
            }

            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getPropertyNames()
     */
    @Override
    public String[] getPropertyNames() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-getPropertyNames", true, null); //$NON-NLS-1$

        try {
            final List< String > names = new ArrayList< String >();

            for (final PropertyIterator iter = getSession(transaction).getNode(getAbsolutePath()).getProperties(); iter.hasNext();) {
                final String name = iter.nextProperty().getName();
                names.add(name);
            }

            transaction.commit();
            return names.toArray(new String[names.size()]);
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
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

    private Session getSession( final UnitOfWork transaction ) {
        return ((UnitOfWorkImpl)transaction).getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasChild(java.lang.String)
     */
    @Override
    public boolean hasChild( final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        final UnitOfWork transaction = this.repository.createTransaction("kobject-hasChild", true, null); //$NON-NLS-1$

        try {
            final boolean result = getSession(transaction).getNode(getAbsolutePath()).hasNode(name);
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasChildren()
     */
    @Override
    public boolean hasChildren() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-hasChildren", true, null); //$NON-NLS-1$

        try {
            final boolean result = getSession(transaction).getNode(getAbsolutePath()).hasNodes();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
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
     * @see org.komodo.spi.repository.KomodoObject#hasProperties()
     */
    @Override
    public boolean hasProperties() throws KException {
        return (getPropertyNames().length > 0);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasProperty(java.lang.String)
     */
    @Override
    public boolean hasProperty( final String name ) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        final UnitOfWork transaction = this.repository.createTransaction("kobject-hasProperty", true, null); //$NON-NLS-1$

        try {
            final boolean result = getSession(transaction).getNode(getAbsolutePath()).hasProperty(name);
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#print()
     */
    @Override
    public void print() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-print", true, null); //$NON-NLS-1$

        try {
            final JcrTools tools = new JcrTools();
            tools.printSubgraph(getSession(transaction).getNode(getAbsolutePath()));
            transaction.commit();
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#removeChild(java.lang.String[])
     */
    @Override
    public void removeChild( final String... names ) throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-removeChild", false, null); //$NON-NLS-1$

        try {
            removeChild(transaction, names);
            transaction.commit();
        } catch (final KException e) {
            transaction.rollback();
            throw e;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#removeChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void removeChild( final UnitOfWork transaction,
                             final String... names ) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(names, "names"); //$NON-NLS-1$
        LOGGER.debug("kobject-removeChild: transaction = {0}, names = {1}", Arrays.asList(names)); //$NON-NLS-1$

        try {
            final Node node = getSession(transaction).getNode(getAbsolutePath());

            for (final String name : names) {
                if (node.hasNode(name)) {
                    node.getNode(name).remove();
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_CHILD, names, getAbsolutePath()));
                }
            }
        } catch (Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#removeMixin(java.lang.String[])
     */
    @Override
    public void removeMixin( String... mixins ) throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-removeMixin", false, null); //$NON-NLS-1$

        try {
            removeMixin(transaction, mixins);
            transaction.commit();
        } catch (final KException e) {
            transaction.rollback();
            throw e;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#removeMixin(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void removeMixin( final UnitOfWork transaction,
                             final String... mixins ) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(mixins, "mixins"); //$NON-NLS-1$
        LOGGER.debug("kobject-removeMixin: transaction = {0}, mixins = {1}", transaction.getName(), Arrays.asList(mixins)); //$NON-NLS-1$

        try {
            final Node node = getSession(transaction).getNode(this.path);

            for (final String mixin : mixins) {
                ArgCheck.isNotEmpty(mixin, "mixin"); //$NON-NLS-1$
                node.removeMixin(mixin);
            }
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
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
     * @see org.komodo.spi.repository.KomodoObject#setPrimaryType(java.lang.String)
     */
    @Override
    public void setPrimaryType( String typeName ) throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-setPrimaryType", false, null); //$NON-NLS-1$

        try {
            setPrimaryType(transaction, typeName);
            transaction.commit();
        } catch (final KException e) {
            transaction.rollback();
            throw e;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#setPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setPrimaryType( final UnitOfWork transaction,
                                final String typeName ) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(typeName, "typeName"); //$NON-NLS-1$

        try {
            final String type = (StringUtils.isBlank(typeName) ? JcrNtLexicon.UNSTRUCTURED.getString() : typeName);
            getSession(transaction).getNode(this.path).setPrimaryType(type);
            transaction.commit();
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
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
                        node.setProperty(name, PropertyImpl.createValue(factory, values[0]));
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
                    node.setProperty(name, PropertyImpl.createValue(factory, values[0]));
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#setProperty(java.lang.String, java.lang.Object[])
     */
    @Override
    public void setProperty( final String propertyName,
                             final Object... values ) throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("kobject-setProperty", false, null); //$NON-NLS-1$

        try {
            setProperty(transaction, propertyName, values);
            transaction.commit();
        } catch (final KException e) {
            transaction.rollback();
            throw e;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#setProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.Object[])
     */
    @Override
    public void setProperty( final UnitOfWork transaction,
                             final String propertyName,
                             final Object... values ) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(propertyName, "propertyName"); //$NON-NLS-1$

        try {
            final Session session = getSession(transaction);
            final Node node = session.getNode(getAbsolutePath());
            setProperty(session, node, propertyName, values);
        } catch (Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
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

}
