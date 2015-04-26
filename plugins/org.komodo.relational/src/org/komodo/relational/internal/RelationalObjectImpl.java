/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.RelationalObject;
import org.komodo.repository.Messages;
import org.komodo.repository.Messages.Komodo;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;

/**
 * A base implementation of a relational object.
 */
public abstract class RelationalObjectImpl extends ObjectImpl implements RelationalObject {

    private static TypeResolverRegistry _resolverRegistry;

    protected static final KLog LOGGER = KLog.getLogger();

    /**
     * Indicates if the initial state after construction should be validated.
     */
    public static final boolean VALIDATE_INITIAL_STATE = true;

    private static Descriptor[] getAllDescriptors( final UnitOfWork transaction,
                                                   final KomodoObject kobject ) throws KException {
        final Descriptor[] mixins = kobject.getDescriptors( transaction );
        final Descriptor[] allDescriptors = new Descriptor[ mixins.length + 1 ];
        System.arraycopy( mixins, 0, allDescriptors, 0, mixins.length );
        allDescriptors[mixins.length] = kobject.getPrimaryType( transaction );
        return allDescriptors;
    }

    private Filter[] filters = DEFAULT_FILTERS;

    protected RelationalObjectImpl( final UnitOfWork uow,
                                    final Repository repository,
                                    final String path ) throws KException {
        this( uow, repository, path, 0 );
    }

    protected RelationalObjectImpl( final UnitOfWork uow,
                                    final Repository repository,
                                    final String path,
                                    final int index ) throws KException {
        super( repository, path, index );
        internalValidateInitialState( uow, this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork uow,
                                  final String name ) throws KException {
        UnitOfWork transaction = uow;

        if ( uow == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getChild", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            final KomodoObject kobject = super.getChild( transaction, name );
            final KomodoObject result = resolveType( transaction, kobject );

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork uow,
                                  final String name,
                                  final String typeName ) throws KException {
        UnitOfWork transaction = uow;

        if ( uow == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getChild2", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            if ( !StringUtils.isBlank( typeName ) && isDescriptorFiltered( typeName ) ) {
                throw new KException( Messages.getString( Messages.Komodo.CHILD_NOT_FOUND, name, getAbsolutePath() ) );
            }

            final KomodoObject kobject = super.getChild( transaction, name, typeName );
            final KomodoObject result = resolveType( transaction, kobject );

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if ( uow == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getChildren", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            KomodoObject[] result = null;
            final KomodoObject[] kids = super.getChildren( transaction );

            if ( kids.length == 0 ) {
                result = kids;
            } else {
                final List< KomodoObject > temp = new ArrayList<>( kids.length );

                for ( final KomodoObject kobject : kids ) {
                    // ensure child has at least one non-filtered descriptor
                    for ( final Descriptor descriptor : getAllDescriptors( transaction, kobject ) ) {
                        if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                            temp.add( resolveType( transaction, kobject ) );
                            break;
                        }
                    }
                }

                result = temp.toArray( new KomodoObject[ temp.size() ] );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow,
                                       final String name ) throws KException {
        UnitOfWork transaction = uow;

        if ( uow == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getChildren", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            KomodoObject[] result = null;
            final KomodoObject[] kids = super.getChildren( transaction, name );

            if ( kids.length == 0 ) {
                result = kids;
            } else {
                final List< KomodoObject > temp = new ArrayList<>( kids.length );

                for ( final KomodoObject kobject : kids ) {
                    // ensure child has at least one non-filtered descriptor
                    for ( final Descriptor descriptor : getAllDescriptors( transaction, kobject ) ) {
                        if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                            temp.add( resolveType( transaction, kobject ) );
                            break;
                        }
                    }
                }

                result = temp.toArray( new KomodoObject[ temp.size() ] );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) throws KException {
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if ( uow == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getChildrenOfType", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            KomodoObject[] result = null;

            if ( isDescriptorFiltered( type ) ) {
                result = KomodoObject.EMPTY_ARRAY;
            } else {
                final KomodoObject[] kids = super.getChildrenOfType( transaction, type );

                if ( kids.length == 0 ) {
                    result = kids;
                } else {
                    final List< KomodoObject > temp = new ArrayList<>( kids.length );

                    for ( final KomodoObject kobject : kids ) {
                        // ensure child has at least one non-filtered descriptor
                        for ( final Descriptor descriptor : getAllDescriptors( transaction, kobject ) ) {
                            if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                                temp.add( resolveType( transaction, kobject ) );
                                break;
                            }
                        }
                    }

                    result = temp.toArray( new KomodoObject[ temp.size() ] );
                }
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getDescriptor(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Descriptor getDescriptor( final UnitOfWork uow,
                                     final String typeName ) throws KException {
        UnitOfWork transaction = uow;

        if ( uow == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getDescriptor", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            Descriptor result = null;

            if (!isDescriptorFiltered( typeName )) {
                result = new FilteredDescriptor( super.getDescriptor( transaction, typeName ) );
            }

            if ( result == null ) {
                throw new KException( Messages.getString( Komodo.DESCRIPTOR_NOT_FOUND, typeName, getAbsolutePath() ) );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor[] getDescriptors( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if ( uow == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getDescriptors", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            final Descriptor[] temp = super.getDescriptors( transaction );
            final List< Descriptor > result = new ArrayList<>(temp.length);

            if ( ( temp.length != 0 ) && ( getFilters().length != 0 ) ) {
                for ( final Descriptor descriptor : temp ) {
                    if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                        result.add( new FilteredDescriptor( descriptor ) );
                    }
                }
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result.toArray( new Descriptor[ result.size() ] );
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.RelationalObject#getFilters()
     */
    @Override
    public Filter[] getFilters() {
        assert (this.filters != null);
        return this.filters;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getParent", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            KomodoObject result = super.getParent( transaction );

            if ( result != null ) {
                result = resolveType( transaction, result );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Property getProperty( final UnitOfWork uow,
                                 final String name ) throws KException {
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getProperty", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            Property result = null;

            if (!isPropertyFiltered( name )) {
                result = super.getProperty( transaction, name );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getPropertyNames( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-getPropertyNames", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            final String[] props = super.getPropertyNames( transaction );
            final List< String > result = new ArrayList<>( props.length );

            for ( final String propName : props ) {
                if ( !isPropertyFiltered( propName ) ) {
                    result.add( propName );
                }
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result.toArray( new String[ result.size() ] );
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    private TypeResolverRegistry getResolverRegistry() {
        if (_resolverRegistry == null)
            _resolverRegistry = TypeResolverRegistry.getInstance();

        return _resolverRegistry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork uow,
                             final String name ) throws KException {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-hasChild", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            boolean result = super.hasChild( transaction, name );

            if ( result ) {
                result = false;

                // if one of the children with that name has a type that is not filtered return true
                for ( final KomodoObject kobject : getChildren( transaction, name ) ) {
                    for ( final Descriptor descriptor : getAllDescriptors( transaction, kobject ) ) {
                        if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                            result = true;
                            break;
                        }
                    }

                    if (result) {
                        break;
                    }
                }
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork uow,
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-hasChild2", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            boolean result = false;

            if ( !isDescriptorFiltered( typeName ) ) {
                result = super.hasChild( transaction, name, typeName );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-hasChildren", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            boolean result = super.hasChildren( transaction );

            if ( result ) {
                result = ( getChildren( transaction ).length != 0 ); // filtered children > 0
            }

            if ( uow == null ) {
                transaction.commit();
            }
            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasDescriptor(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasDescriptor( final UnitOfWork uow,
                                  final String descriptorName ) throws KException {
        ArgCheck.isNotEmpty( descriptorName );
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-hasDescriptor", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            boolean result = false;

            if ( !isDescriptorFiltered( descriptorName ) ) {
                result = super.hasDescriptor( transaction, descriptorName );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasProperty( final UnitOfWork uow,
                                final String name ) throws KException {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-hasProperty", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            boolean result = false;

            if ( !isPropertyFiltered( name ) ) {
                result = super.hasProperty( transaction, name );
            }

            if ( uow == null ) {
                transaction.commit();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    private final void internalValidateInitialState( final UnitOfWork uow,
                                                     final KomodoObject kobject ) throws KException {
        if ( VALIDATE_INITIAL_STATE ) {
            UnitOfWork transaction = uow;

            if ( transaction == null ) {
                transaction = getRepository().createTransaction( getClass().getSimpleName() + "-internalValidateInitialState", //$NON-NLS-1$
                                                                 true,
                                                                 null );
            }

            assert ( transaction != null );

            validateInitialState( transaction, kobject );

            if ( uow == null ) {
                transaction.commit();
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.RelationalObject#isChildRestricted()
     */
    @Override
    public boolean isChildRestricted() {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#removeDescriptor(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public void removeDescriptor( final UnitOfWork uow,
                                  final String... descriptorNames ) throws KException {
        ArgCheck.isNotEmpty( descriptorNames, "descriptorNames" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if ( transaction == null ) {
            transaction = getRepository().createTransaction( "relationalobjectimpl-removeDescriptor", false, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        if ( LOGGER.isDebugEnabled() ) {
            LOGGER.debug( "relationalobjectimpl-removeDescriptor: transaction = {0}, mixins = {1}", //$NON-NLS-1$
                          transaction.getName(),
                          Arrays.asList( descriptorNames ) );
        }

        try {
            for ( final String typeName : descriptorNames ) {
                ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

                if ( isDescriptorFiltered( typeName ) ) {
                    throw new KException( Messages.getString( Komodo.DESCRIPTOR_NOT_FOUND, typeName, getAbsolutePath() ) );
                }

                super.removeDescriptor( transaction, typeName );
            }

            if ( uow == null ) {
                transaction.commit();
            }
        } catch ( final Exception e ) {
            throw handleError( uow, transaction, e );
        }
    }

    protected KomodoObject resolveType( final UnitOfWork transaction,
                                        final KomodoObject kobject ) throws KException {
        TypeResolver< ? > resolver = getResolverRegistry().getResolver(kobject.getTypeIdentifier(transaction));
        if (resolver != null && resolver.resolvable(transaction, kobject))
            return resolver.resolve( transaction, kobject );

        // Failed with the type identifier so try to be safe than sorry
        // and iterate through all resolvers to check this object is really
        // not resolvable.
        for ( final TypeResolver< ? > aResolver : getResolverRegistry().getResolvers() ) {
            if ( aResolver.resolvable( transaction, kobject ) ) {
                return aResolver.resolve( transaction, kobject );
            }
        }

        return kobject;
    }

    private boolean isDescriptorFiltered( final String descriptorName ) {
        assert !StringUtils.isBlank( descriptorName );

        if ( getFilters().length != 0 ) {
            for ( final Filter filter : getFilters() ) {
                if ( filter.rejectDescriptor( descriptorName ) ) {
                    return true;
                }
            }
        }

        return false;
    }

    private boolean isPropertyFiltered( final String propName ) {
        assert !StringUtils.isBlank( propName );

        if ( getFilters().length != 0 ) {
            for ( final Filter filter : getFilters() ) {
                if ( filter.rejectProperty( propName ) ) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.RelationalObject#setFilters(org.komodo.relational.model.RelationalObject.Filter[])
     */
    @Override
    public void setFilters( final Filter[] newFilters ) {
        this.filters = ( ( newFilters == null ) ? RelationalObject.NO_FILTERS : newFilters );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#setPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public final void setPrimaryType( final UnitOfWork uow,
                                      final String typeName ) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#toString()
     */
    @Override
    public String toString() {
        return getAbsolutePath();
    }

    /**
     * @param uow
     *        the rollback only transaction (never <code>null</code>)
     * @param kobject
     *        the object being checked (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    protected void validateInitialState( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
        final TypeResolver< ? > resolver = getResolverRegistry().getResolver( kobject.getClass() );

        if ( ( resolver != null ) && !resolver.resolvable( transaction, kobject ) ) {
            throw new KException( Messages.getString( Komodo.INCORRECT_TYPE,
                                                      kobject.getAbsolutePath(),
                                                      kobject.getClass().getSimpleName() ) );
        }
    }

    class FilteredDescriptor implements Descriptor {

        private final Descriptor delegate;

        FilteredDescriptor( final Descriptor delegate ) {
            this.delegate = delegate;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Descriptor#getChildDescriptors()
         */
        @Override
        public Descriptor[] getChildDescriptors() throws KException {
            final Descriptor[] descriptors = this.delegate.getChildDescriptors();

            if ( descriptors.length == 0 ) {
                return descriptors;
            }

            final List< Descriptor > result = new ArrayList<>( descriptors.length );

            for ( final Descriptor descriptor : descriptors ) {
                if ( !RelationalObjectImpl.this.isDescriptorFiltered( descriptor.getName() ) ) {
                    result.add( new FilteredDescriptor( descriptor ) );
                }
            }

            return result.toArray( new Descriptor[ result.size() ] );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Descriptor#getName()
         */
        @Override
        public String getName() throws KException {
            return delegate.getName();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Descriptor#getPropertyDescriptors()
         */
        @Override
        public PropertyDescriptor[] getPropertyDescriptors() throws KException {
            final PropertyDescriptor[] descriptors = this.delegate.getPropertyDescriptors();

            if ( descriptors.length == 0 ) {
                return descriptors;
            }

            final List< PropertyDescriptor > result = new ArrayList<>( descriptors.length );

            for ( final PropertyDescriptor descriptor : descriptors ) {
                if ( !RelationalObjectImpl.this.isPropertyFiltered( descriptor.getName() ) ) {
                    result.add( descriptor );
                }
            }

            return result.toArray( new PropertyDescriptor[ result.size() ] );
        }

    }

}
