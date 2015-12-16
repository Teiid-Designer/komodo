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
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
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
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;

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

    private Filter[] filters = DEFAULT_FILTERS;

    protected RelationalObjectImpl( final UnitOfWork uow,
                                    final Repository repository,
                                    final String path ) throws KException {
        this( uow, repository, path, 0 );
    }

    protected RelationalObjectImpl( final UnitOfWork transaction,
                                    final Repository repository,
                                    final String path,
                                    final int index ) throws KException {
        super( repository, path, index );

        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        internalValidateInitialState( transaction, this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject kobject = super.getChild( transaction, name );
        final KomodoObject result = resolveType( transaction, kobject );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if ( !StringUtils.isBlank( typeName ) && isDescriptorFiltered( typeName ) ) {
            throw new KException( Messages.getString( Messages.Komodo.CHILD_NOT_FOUND, name, getAbsolutePath() ) );
        }

        final KomodoObject kobject = super.getChild( transaction, name, typeName );
        final KomodoObject result = resolveType( transaction, kobject );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction,
                                       final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;
        final KomodoObject[] kids = super.getChildren( transaction, namePatterns );

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

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( isDescriptorFiltered( type ) ) {
            result = KomodoObject.EMPTY_ARRAY;
        } else {
            final KomodoObject[] kids = super.getChildrenOfType( transaction, type, namePatterns );

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

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getDescriptor(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Descriptor getDescriptor( final UnitOfWork transaction,
                                     final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Descriptor result = null;

        if ( !isDescriptorFiltered( typeName ) ) {
            result = new FilteredDescriptor( super.getDescriptor( transaction, typeName ) );
        }

        if ( result == null ) {
            throw new KException( Messages.getString( Komodo.DESCRIPTOR_NOT_FOUND, typeName, getAbsolutePath() ) );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor[] getDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Descriptor[] temp = super.getDescriptors( transaction );
        final List< Descriptor > result = new ArrayList<>( temp.length );

        if ( ( temp.length != 0 ) && ( getFilters().length != 0 ) ) {
            for ( final Descriptor descriptor : temp ) {
                if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                    result.add( new FilteredDescriptor( descriptor ) );
                }
            }
        }

        return result.toArray( new Descriptor[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#getFilters()
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
    public KomodoObject getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject result = super.getParent( transaction );

        if ( result != null ) {
            result = resolveType( transaction, result );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Property getProperty( final UnitOfWork transaction,
                                 final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Property result = null;

        if ( !isPropertyFiltered( name ) ) {
            result = super.getProperty( transaction, name );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getPropertyDescriptor(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                                     final String propName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$

        if (isPropertyFiltered( propName )) {
            return null;
        }

        return super.getPropertyDescriptor( transaction, propName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getPropertyDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final PropertyDescriptor[] descriptors = super.getPropertyDescriptors( transaction );
        final List< PropertyDescriptor > result = new ArrayList<>( descriptors.length );

        for ( final PropertyDescriptor descriptor : descriptors ) {
            if ( !isPropertyFiltered( descriptor.getName() ) ) {
                result.add( descriptor );
            }

        }

        return result.toArray( new PropertyDescriptor[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getPropertyNames( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] props = super.getPropertyNames( transaction );
        final List< String > result = new ArrayList<>( props.length );

        for ( final String propName : props ) {
            if ( !isPropertyFiltered( propName ) ) {
                result.add( propName );
            }
        }

        return result.toArray( new String[ result.size() ] );
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
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

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

                if ( result ) {
                    break;
                }
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        boolean result = false;

        if ( !isDescriptorFiltered( typeName ) ) {
            result = super.hasChild( transaction, name, typeName );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        boolean result = super.hasChildren( transaction );

        if ( result ) {
            result = ( getChildren( transaction ).length != 0 ); // filtered children > 0
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasDescriptor(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasDescriptor( final UnitOfWork transaction,
                                  final String descriptorName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptorName );

        boolean result = false;

        if ( !isDescriptorFiltered( descriptorName ) ) {
            result = super.hasDescriptor( transaction, descriptorName );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasProperty( final UnitOfWork transaction,
                                final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        boolean result = false;

        if ( !isPropertyFiltered( name ) ) {
            result = super.hasProperty( transaction, name );
        }

        return result;
    }

    private final void internalValidateInitialState( final UnitOfWork transaction,
                                                     final KomodoObject kobject ) throws KException {
        assert ( transaction != null );

        if ( VALIDATE_INITIAL_STATE ) {
            validateInitialState( transaction, kobject );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#isChildRestricted()
     */
    @Override
    public boolean isChildRestricted() {
        return false;
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
     * @see org.komodo.repository.ObjectImpl#removeDescriptor(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public void removeDescriptor( final UnitOfWork transaction,
                                  final String... descriptorNames ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptorNames, "descriptorNames" ); //$NON-NLS-1$

        if ( LOGGER.isDebugEnabled() ) {
            LOGGER.debug( "relationalobjectimpl-removeDescriptor: transaction = {0}, mixins = {1}", //$NON-NLS-1$
                          transaction.getName(),
                          Arrays.asList( descriptorNames ) );
        }

        for ( final String typeName : descriptorNames ) {
            ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

            if ( isDescriptorFiltered( typeName ) ) {
                throw new KException( Messages.getString( Komodo.DESCRIPTOR_NOT_FOUND, typeName, getAbsolutePath() ) );
            }

            super.removeDescriptor( transaction, typeName );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#setFilters(org.komodo.relational.RelationalObject.Filter[])
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
        throw new UnsupportedOperationException( Messages.getString( Relational.PROPERTY_NOT_MODIFIABLE,
                                                                     JcrLexicon.PRIMARY_TYPE.getString() ) );
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
         * @see org.komodo.spi.repository.Descriptor#getChildDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
         */
        @Override
        public Descriptor[] getChildDescriptors( final UnitOfWork transaction ) throws KException {
            final Descriptor[] descriptors = this.delegate.getChildDescriptors(transaction);

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
        public String getName() {
            return delegate.getName();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Descriptor#getPropertyDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
         */
        @Override
        public PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException {
            final PropertyDescriptor[] descriptors = this.delegate.getPropertyDescriptors(transaction);

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
