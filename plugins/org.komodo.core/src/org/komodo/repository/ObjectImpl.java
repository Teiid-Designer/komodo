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
import java.util.Set;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PathNotFoundException;
import javax.jcr.PropertyIterator;
import javax.jcr.PropertyType;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.nodetype.NodeType;
import org.komodo.repository.KomodoTypeRegistry.TypeIdentifier;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoObjectVisitor;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrNtLexicon;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.api.JcrTools;
import org.modeshape.sequencer.ddl.DdlConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

/**
 * An implementation of a {@link KomodoObject Komodo object}.
 */
public class ObjectImpl implements KomodoObject, StringConstants {

    private static final KLog LOGGER = KLog.getLogger();

    protected static Descriptor[] getAllDescriptors( final UnitOfWork transaction,
                                                     final KomodoObject kobject ) throws KException {
        assert ( transaction != null );

        final Descriptor[] mixins = kobject.getDescriptors( transaction );
        final Descriptor[] allDescriptors = new Descriptor[ mixins.length + 1 ];
        System.arraycopy( mixins, 0, allDescriptors, 0, mixins.length );
        allDescriptors[mixins.length] = kobject.getPrimaryType( transaction );
        return allDescriptors;
    }

    /**
     * Wraps error in a {@link KException} if necessary.
     *
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error (never <code>null</code>)
     */
    public static KException handleError( final Exception e ) {
        assert ( e != null );

        if ( e instanceof KException ) {
            return ( KException )e;
        }

        return new KException( e );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param kobject
     *        the object whose property value is being validated (cannot be empty)
     * @param name
     *        the name of the property being validated (cannot be empty)
     * @param expectedValue
     *        the expected value or <code>null</code> if the property should not exist
     * @return <code>true</code> if the property value is the expected value
     * @throws KException
     *         if an error occurs
     */
    public static boolean validatePropertyValue( final UnitOfWork transaction,
                                                 final Repository repository,
                                                 final KomodoObject kobject,
                                                 final String name,
                                                 final Object expectedValue ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        boolean valid = true;
        final Property property = kobject.getProperty( transaction, name );

        if ( property == null ) {
            if ( expectedValue != null ) {
                valid = ( ( expectedValue instanceof String ) && StringUtils.isBlank( ( String )expectedValue ) );
            }
        } else {
            valid = property.getValue( transaction ).equals( expectedValue );
        }

        return valid;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param kobject
     *        the object whose type is being validated (cannot be empty)
     * @param types
     *        the primary type or descriptor names that the object must have (cannot be <code>null</code> or empty or have a
     *        <code>null</code> element)
     * @return <code>true</code> if object is resolvable to the specified type(s)
     * @throws KException
     *         if an error occurs or if object does not have all the specified types
     */
    public static boolean validateType( final UnitOfWork transaction,
                                        final Repository repository,
                                        final KomodoObject kobject,
                                        final String... types ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( types, "types" ); //$NON-NLS-1$

        boolean result = true;

        for ( final String type : types ) {
            ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

            if ( !kobject.hasDescriptor( transaction, type ) && !type.equals( kobject.getPrimaryType( transaction ).getName() ) ) {
                result = false;
                break;
            }
        }

        return result;
    }

    protected int index;
    protected String path;
    final private Repository repository;

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

    private void internalSetProperty( final UnitOfWork transaction,
                                      final String name,
                                      final Object... values ) throws Exception {
        final Session session = getSession( transaction );
        final Node node = session.getNode( getAbsolutePath() );
        final ValueFactory factory = session.getValueFactory();
        final boolean exists = node.hasProperty( name );

        // remove property
        if ( values == null ) {
            if ( exists ) {
                node.getProperty( name ).remove();
            } else {
                throw new KException( Messages.getString( Messages.Komodo.UNABLE_TO_REMOVE_PROPERTY_THAT_DOES_NOT_EXIST,
                                                          name,
                                                          getAbsolutePath() ) );
            }
        } else {
            // must be an array at this point
            final int count = values.length;

            if ( exists ) {
                final javax.jcr.Property property = node.getProperty( name );
                final int type = property.getType();
                final boolean multiple = property.isMultiple();

                if ( count == 0 ) {
                    if ( multiple ) {
                        property.remove();
                    } else {
                        throw new KException( Messages.getString( Messages.Komodo.UNABLE_TO_REMOVE_SINGLE_VALUE_PROPERTY_WITH_EMPTY_ARRAY,
                                                                  name,
                                                                  getAbsolutePath() ) );
                    }
                } else if ( count > 1 ) {
                    if ( multiple ) {
                        setMultiValuedProperty( session, node, factory, name, values, type );
                    } else {
                        throw new KException( Messages.getString( Messages.Komodo.UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
                                                                  name,
                                                                  getAbsolutePath() ) );
                    }
                } else {
                    // only one value so set property
                    if ( multiple ) {
                        setMultiValuedProperty( session, node, factory, name, values, type );
                    } else {
                        // remove if value is null or empty string
                        if ( ( values[0] == null )
                             || ( ( values[0] instanceof String ) && StringUtils.isBlank( ( String )values[0] ) ) ) {
                            node.getProperty( name ).remove();
                        } else {
                            node.setProperty( name, PropertyImpl.createValue( factory, values[0] ) );
                        }
                    }
                }
            } else {
                // property does not exist and no values being set
                if ( count == 0 ) {
                    throw new KException( Messages.getString( Messages.Komodo.UNABLE_TO_REMOVE_PROPERTY_THAT_DOES_NOT_EXIST,
                                                              name,
                                                              getAbsolutePath() ) );
                }

                // try and find property descriptor
                final PropertyDescriptor propDescriptor = getPropertyDescriptor( transaction, name );

                if ( propDescriptor == null ) {
                    // could not find a property descriptor so assume if more than one value it is multi-valued
                    if ( count > 1 ) {
                        setMultiValuedProperty( session, node, factory, name, values, PropertyType.UNDEFINED );
                    } else {
                        if ( ( values[0] != null )
                             && ( ( !( values[0] instanceof String ) ) || !StringUtils.isBlank( ( String )values[0] ) ) ) {
                            node.setProperty( name, PropertyImpl.createValue( factory, values[0] ) );
                        }
                    }
                } else if ( propDescriptor.isMultiple() ) {
                    final int propType = PropertyDescriptorImpl.convert( propDescriptor.getType() );
                    setMultiValuedProperty( session, node, factory, name, values, propType );
                } else {
                    if ( ( values[0] != null )
                         && ( ( !( values[0] instanceof String ) ) || !StringUtils.isBlank( ( String )values[0] ) ) ) {
                        node.setProperty( name, PropertyImpl.createValue( factory, values[0] ) );
                    }
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#isChildRestricted()
     */
    @Override
    public boolean isChildRestricted() {
        return false;
    }

    /**
     * Implementation specific and not to be included on the interface!
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the underlying object of this {@link ObjectImpl}
     * @throws KException
     *         if an error occurs
     */
    protected Node node( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-node: transaction = {0}, path = {1}", transaction.getName(), getAbsolutePath()); //$NON-NLS-1$
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

            if (node == null) {
                // throw the original path not found exception
                throw throwEx;
            }

            // return the found node
            return node;
        } catch (final Exception e) {
            throw handleError( e );
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
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-addChild: transaction = {0}, name = {1}, primaryType = {2}", //$NON-NLS-1$
                         transaction.getName(),
                         name,
                         primaryType);
        }

        final String type = (StringUtils.isBlank(primaryType) ? JcrNtLexicon.UNSTRUCTURED.getString() : primaryType);

        try {
            final Node node = node(transaction).addNode(name, type);
            final KomodoObject result = new ObjectImpl(getRepository(), node.getPath(), node.getIndex());

            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#addDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void addDescriptor( final UnitOfWork transaction,
                               final String... descriptorNames ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(descriptorNames, "descriptorNames"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-addDescriptor: transaction = {0}, descriptorNames = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         Arrays.asList(descriptorNames));
        }

        try {
            final Node node = getSession(transaction).getNode(this.path);

            for (final String mixin : descriptorNames) {
                ArgCheck.isNotEmpty(mixin, "mixin"); //$NON-NLS-1$
                node.addMixin(mixin);
            }
        } catch (final Exception e) {
            throw handleError( e );
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
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        try {
            final Node node = node(transaction).getNode(name);
            final KomodoObject result = new ObjectImpl(getRepository(), node.getPath(), 0);

            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        final KomodoObject[] kids = getChildren( transaction, name );

        if ( kids.length != 0 ) {
            for ( final KomodoObject kid : kids ) {
                if ( typeName.equals( kid.getPrimaryType( transaction ).getName() ) || kid.hasDescriptor( transaction, typeName ) ) {
                    return kid;
                }
            }
        }

        throw new KException( Messages.getString( Messages.Komodo.CHILD_NOT_FOUND, name, getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject[] result = getRawChildren( transaction );
        return result;
    }

    private KomodoObject[] getChildren( final UnitOfWork transaction,
                                        final NodeIterator itr ) throws Exception {
        assert ( transaction != null );
        assert ( transaction.getState() == State.NOT_STARTED );

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
    public KomodoObject[] getChildren( final UnitOfWork transaction,
                                       final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        KomodoObject[] result = null;

        try {
            result = getChildren(transaction, node(transaction).getNodes(name));
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

        try {
            KomodoObject[] kids = getChildren( transaction, node( transaction ).getNodes() );

            if ( kids.length != 0 ) {
                final List< KomodoObject > matches = new ArrayList< KomodoObject >( kids.length );

                for ( final KomodoObject kid : kids ) {
                    if ( type.equals( kid.getPrimaryType( transaction ).getName() ) || kid.hasDescriptor( transaction, type ) ) {
                        matches.add( kid );
                    }
                }

                kids = matches.toArray( new KomodoObject[ matches.size() ] );
            }

            return kids;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public Descriptor getDescriptor( final UnitOfWork transaction,
                                     final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        try {
            final Node node = node( transaction );
            final NodeType[] nodeTypes = node.getMixinNodeTypes();

            Descriptor result = null;

            for ( final NodeType nodeType : nodeTypes ) {
                if ( typeName.equals( nodeType.getName() ) ) {
                    result = new DescriptorImpl( this.repository, nodeType.getName() );
                    break;
                }
            }

            if ( result == null ) {
                throw new KException( Messages.getString( Messages.Komodo.DESCRIPTOR_NOT_FOUND, typeName, getAbsolutePath() ) );
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor[] getDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Descriptor[] result = getRawDescriptors( transaction );
        return result;
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
    public String getName( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final String result = node(transaction).getName();
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final Node parent = node(transaction).getParent();
            String parentPath = parent.getPath();

            if (!parentPath.endsWith(FORWARD_SLASH)) {
                parentPath += FORWARD_SLASH;
            }

            return new ObjectImpl(this.repository, parent.getPath(), 0);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor getPrimaryType( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final NodeType nodeType = node(transaction).getPrimaryNodeType();
            return new DescriptorImpl(this.repository, nodeType.getName());
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * Convenience method for retrieving a property.
     *
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param returnValueType
     *        the type of the return value type
     * @param getterName
     *        name of the method name calling this method
     * @param propertyPath
     *        relative path of the actual property
     * @return the value of the property cast to the specified return value type
     * @throws KException
     */
    @SuppressWarnings( "unchecked" )
    protected < T > T getObjectProperty( final UnitOfWork transaction,
                                         final PropertyValueType returnValueType,
                                         final String getterName,
                                         final String propertyPath ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if ( LOGGER.isDebugEnabled() ) {
            LOGGER.debug( getterName + ": transaction = {0}", transaction.getName() ); //$NON-NLS-1$
        }

        T result = null;
        Property property = getProperty( transaction, propertyPath );

        if ( property != null ) {
            switch ( returnValueType ) {
                case STRING:
                    result = ( T )property.getStringValue( transaction );
                    break;
                case LONG:
                    result = ( T )Long.valueOf( property.getLongValue( transaction ) );
                    break;
                case INTEGER:
                    result = ( T )Integer.valueOf( Long.valueOf( property.getLongValue( transaction ) ).intValue() );
                    break;
                case BIG_DECIMAL:
                    result = ( T )property.getDecimalValue( transaction );
                    break;
                case DOUBLE:
                    result = ( T )Double.valueOf( property.getDoubleValue( transaction ) );
                    break;
                case BOOLEAN:
                    result = ( T )Boolean.valueOf( property.getBooleanValue( transaction ) );
                    break;
                case CALENDAR:
                    result = ( T )property.getDateValue( transaction );
                    break;
                default:
                    throw new UnsupportedOperationException( "Further property types should be added for support in this method" ); //$NON-NLS-1$
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Property getProperty( final UnitOfWork transaction,
                                 final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        final Property result = getRawProperty( transaction, name );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getPropertyDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                                     final String propName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$

        for ( final Descriptor typeDescriptor : getAllDescriptors( transaction, this ) ) {
            for ( final PropertyDescriptor propDescriptor : typeDescriptor.getPropertyDescriptors( transaction ) ) {
                if ( propName.equals( propDescriptor.getName() ) ) {
                    return propDescriptor;
                }
            }
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getPropertyNames( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] result = getRawPropertyNames( transaction );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getRawChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final KomodoObject[] getRawChildren( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final NodeIterator itr = node( transaction ).getNodes();
            final KomodoObject[] result = getChildren( transaction, itr );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getRawDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final Descriptor[] getRawDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final Node node = node( transaction );
            final NodeType[] nodeTypes = node.getMixinNodeTypes();
            final Descriptor[] result = new Descriptor[ nodeTypes.length ];
            int i = 0;

            for ( final NodeType nodeType : nodeTypes ) {
                result[i++] = new DescriptorImpl( this.repository, nodeType.getName() );
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getRawProperty(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public final Property getRawProperty( final UnitOfWork transaction,
                                          final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        try {
            final Node node = node( transaction );
            Property result = null;

            if ( node.hasProperty( name ) ) {
                final javax.jcr.Property jcrProperty = node.getProperty( name );
                result = new PropertyImpl( this.repository, jcrProperty.getPath() );
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getRawPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final String[] getRawPropertyNames( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final List< String > names = new ArrayList< String >();

            for ( final PropertyIterator iter = node( transaction ).getProperties(); iter.hasNext(); ) {
                final String name = iter.nextProperty().getName();
                names.add( name );
            }

            return names.toArray( new String[ names.size() ] );
        } catch ( final Exception e ) {
            throw handleError( e );
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
        assert ( transaction != null );
        return ( ( UnitOfWorkImpl )transaction ).getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return getClass().hashCode();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        List<Descriptor> descriptors = new ArrayList<Descriptor>();
        descriptors.add(getPrimaryType(transaction));
        descriptors.addAll(Arrays.asList(getDescriptors(transaction)));

        KomodoTypeRegistry registry  = KomodoTypeRegistry.getInstance();
        Set<TypeIdentifier> identifiers = null;
        for (Descriptor descriptor : descriptors) {
            String name = descriptor.getName();
            identifiers = registry.getIdentifiers(name);
            if (! identifiers.isEmpty())
                break;
        }

        KomodoType result;
        if (identifiers == null) {
            // No identifiers but could be DDL Statements container
            String nodeName = getName(transaction);
            if (StandardDdlLexicon.STATEMENTS_CONTAINER.equals(nodeName))
                return KomodoType.DDL_SCHEMA;

            result = KomodoType.UNKNOWN;
        } else if( identifiers.size() == 1 ) {
            result = identifiers.iterator().next().getKomodoType();
        } else {
            //
            // Only lexicon id with multiple identifiers is TABLE_ELEMENT
            //

            // ACCESS_PATTERN, TeiidDdlLexicon.Constraint.TABLE_ELEMENT
            //
            // COLUMN, TeiidDdlLexicon.Constraint.TABLE_ELEMENT
            //
            // PRIMARY_KEY, TeiidDdlLexicon.Constraint.TABLE_ELEMENT
            //
            // UNIQUE_CONSTRAINT, TeiidDdlLexicon.Constraint.TABLE_ELEMENT

            String accessPatternConstraint = TeiidDdlConstants.TeiidNonReservedWord.ACCESSPATTERN.toDdl();
            String primaryKeyConstraint = DdlConstants.PRIMARY_KEY;
            String uniqueConstraint = TeiidDdlConstants.TeiidReservedWord.UNIQUE.toDdl();

            Property constProperty = getProperty(transaction, TeiidDdlLexicon.Constraint.TYPE);
            if (constProperty == null)
                result = KomodoType.UNKNOWN;
            else {
                String constType = constProperty.getStringValue(transaction);
                if (accessPatternConstraint.equals(constType))
                    result = KomodoType.ACCESS_PATTERN;
                else if (primaryKeyConstraint.equals(constType))
                    result = KomodoType.PRIMARY_KEY;
                else if (uniqueConstraint.equals(constType))
                    result = KomodoType.UNIQUE_CONSTRAINT;
                else
                    result = KomodoType.COLUMN;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        try {
            final boolean result = node( transaction ).hasNode( name );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        try {
            getChild( transaction, name, typeName ); // TODO should just query for node count > 0
            return true;
        } catch ( final KException e ) {
            // child not found
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final boolean result = node(transaction).hasNodes();
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#hasDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasDescriptor( final UnitOfWork transaction,
                                  final String descriptorName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptorName );

        boolean result = false;

        for ( final Descriptor descriptor : getDescriptors( transaction ) ) {
            if ( descriptorName.equals( descriptor.getName() ) ) {
                result = true;
            }
        }

        return result;
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
    public boolean hasProperty( final UnitOfWork transaction,
                                final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        try {
            final boolean result = node(transaction).hasProperty(name);
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#print(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void print( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final JcrTools tools = new JcrTools(true);
            tools.printSubgraph(node(transaction));
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#remove(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void remove( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "objectimpl-remove: transaction = {0}, path = {1}", transaction.getName(), getAbsolutePath() ); //$NON-NLS-1$
        }

        try {
            final Node node = node( transaction );
            node.remove();
        } catch (final Exception e) {
            throw handleError( e );
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
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(names, "names"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-removeChild: transaction = {0}, names = {1}", //$NON-NLS-1$
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
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#removeDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void removeDescriptor( final UnitOfWork transaction,
                                  final String... descriptorNames ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(descriptorNames, "descriptorNames"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-removeDescriptor: transaction = {0}, mixins = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         Arrays.asList(descriptorNames));
        }

        try {
            final Node node = getSession(transaction).getNode(this.path);

            for (final String mixin : descriptorNames) {
                ArgCheck.isNotEmpty(mixin, "mixin"); //$NON-NLS-1$
                node.removeMixin(mixin);
            }
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void rename( final UnitOfWork transaction,
                        final String newName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( newName, "newName" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "objectimpl-rename: transaction = {0}, old path = {1}, new name = {2}", //$NON-NLS-1$
                          transaction.getName(),
                          getAbsolutePath(),
                          newName );
        }

        try {
            String newPath = getParent( transaction ).getAbsolutePath();

            if (!newPath.endsWith( FORWARD_SLASH )) {
                newPath += FORWARD_SLASH;
            }

            newPath += newName;
            getSession( transaction ).move( getAbsolutePath(), newPath );
            this.path = newPath;
            // TODO seems like index could change also
        } catch (final Exception e) {
            throw handleError( e );
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
    public void setPrimaryType( final UnitOfWork transaction,
                                final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setPrimaryType: transaction = {0}, typeName = {1}", transaction.getName(), typeName); //$NON-NLS-1$
        }

        try {
            final String type = (StringUtils.isBlank(typeName) ? JcrNtLexicon.UNSTRUCTURED.getString() : typeName);
            getSession(transaction).getNode(this.path).setPrimaryType(type);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    protected void setObjectProperty( final UnitOfWork transaction,
                                      final String setterName,
                                      final String propertyName,
                                      final Object value ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug(setterName + ": transaction = {0}, value = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         value);
        }

        try {
            setProperty(transaction, propertyName, value);
        } catch (final Exception e) {
            throw handleError( e );
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
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(propertyName, "propertyName"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setProperty: transaction = {0}, propertyName = {1}, value(s) = {2}", //$NON-NLS-1$
                         transaction.getName(),
                         propertyName,
                         values);
        }

        try {
            internalSetProperty(transaction, propertyName, values);
        } catch (final Exception e) {
            throw handleError( e );
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
    public void visit( final UnitOfWork transaction,
                       final KomodoObjectVisitor visitor ) throws Exception {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        visitor.visit(transaction, this);
    }

}
