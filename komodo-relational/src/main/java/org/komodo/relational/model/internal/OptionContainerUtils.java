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
package org.komodo.relational.model.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.StatementOption;
import org.komodo.repository.DescriptorImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * Utilities for retrieving and updating standard and custom statement options.
 */
public final class OptionContainerUtils {

    /**
     * A {@link PropertyDescriptor property descriptor} for an {@link StatementOption option}.
     */
    static class OptionDescriptor implements PropertyDescriptor {

        private final String[] defaultValues;
        private final String name;

        /**
         * @param optionName
         *        the name of the option (cannot be empty)
         * @param defaultValue
         *        the default value for the option (can be empty)
         */
        public OptionDescriptor( final String optionName,
                                 final String defaultValue ) {
            ArgCheck.isNotEmpty( optionName, "optionName" ); //$NON-NLS-1$
            this.name = optionName;
            this.defaultValues = ( StringUtils.isBlank( defaultValue ) ? StringConstants.EMPTY_ARRAY
                                                                       : new String[] { defaultValue } );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#getDefaultValues()
         */
        @Override
        public Object[] getDefaultValues() {
            return this.defaultValues;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#getName()
         */
        @Override
        public String getName() {
            return this.name;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#getType()
         */
        @Override
        public Type getType() {
            return Type.STRING;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#isMandatory()
         */
        @Override
        public boolean isMandatory() {
            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#isModifiable()
         */
        @Override
        public boolean isModifiable() {
            return true;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#isMultiple()
         */
        @Override
        public boolean isMultiple() {
            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getName();
        }

    }

    private static class PrimaryTypeDescriptor extends DescriptorImpl {

        private final OptionContainer container;
        private final Descriptor primaryType;

        /**
         * @param transaction
         *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
         * @param optionContainer
         *        the option container (cannot be <code>null</code>)
         * @param descriptor
         *        the primary type descriptor (cannot be <code>null</code>)
         * @throws KException
         *         if an error occurs
         */
        PrimaryTypeDescriptor( final UnitOfWork transaction,
                               final OptionContainer optionContainer,
                               final Descriptor descriptor ) throws KException {
            super( optionContainer.getRepository(), optionContainer.getName( transaction ) );
            ArgCheck.isNotNull( descriptor, "descriptor" ); //$NON-NLS-1$

            this.container = optionContainer;
            this.primaryType = descriptor;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.repository.DescriptorImpl#getPropertyDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
         */
        @Override
        public PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException {
            final PropertyDescriptor[] propDescriptors = this.primaryType.getPropertyDescriptors( transaction );
            PropertyDescriptor[] standardDescriptors = PropertyDescriptor.NO_DESCRIPTORS;
            PropertyDescriptor[] customDescriptors = PropertyDescriptor.NO_DESCRIPTORS;

            { // standard statement options
                final Map< String, String > standardOptions = this.container.getStandardOptions();

                if ( !standardOptions.isEmpty() ) {
                    standardDescriptors = new PropertyDescriptor[ standardOptions.size() ];
                    int i = 0;

                    for ( final Entry< String, String > entry : standardOptions.entrySet() ) {
                        standardDescriptors[ i++ ] = new OptionDescriptor( entry.getKey(), entry.getValue() );
                    }
                }
            }

            { // custom statement options
                final StatementOption[] customOptions = this.container.getCustomOptions( transaction );

                if ( customOptions.length > 0 ) {
                    customDescriptors = new PropertyDescriptor[ customOptions.length ];
                    int i = 0;

                    for ( final StatementOption option : customOptions ) {
                        customDescriptors[i++] = new OptionDescriptor( option.getName( transaction ), null );
                    }
                }
            }

            // combine all descriptors
            final PropertyDescriptor[] result = new PropertyDescriptor[ propDescriptors.length + standardDescriptors.length
                                                                        + customDescriptors.length ];
            System.arraycopy( propDescriptors, 0, result, 0, propDescriptors.length );
            System.arraycopy( standardDescriptors, 0, result, propDescriptors.length, standardDescriptors.length );
            System.arraycopy( customDescriptors,
                              0,
                              result,
                              ( propDescriptors.length + standardDescriptors.length ),
                              customDescriptors.length );

            return result;
        }

    }

    /**
     * Supplements the given descriptor with all the each standard and custom statement options.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container (cannot be <code>null</code>)
     * @param primaryType
     *        the primary type descriptor (cannot be <code>null</code>)
     * @return the primary type descriptor (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Descriptor createPrimaryType( final UnitOfWork transaction,
                                                final OptionContainer container,
                                                final Descriptor primaryType ) throws KException {
        return new PrimaryTypeDescriptor( transaction, container, primaryType );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose custom options are being requested (cannot be <code>null</code>)
     * @return the user-defined and any other non-standard statement options (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public static StatementOption[] getCustomOptions( final UnitOfWork transaction,
                                                      final OptionContainer container ) throws KException {
        final StatementOption[] options = getOptions( transaction, container );

        if ( options.length == 0 ) {
            return options;
        }

        final List< StatementOption > custom = new ArrayList<>( options.length );

        for ( final StatementOption option : options ) {
            if ( !container.isStandardOption( option.getName( transaction ) ) ) {
                custom.add( option );
            }
        }

        return custom.toArray( new StatementOption[ custom.size() ] );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose statement option value is being requested (cannot be <code>null</code>)
     * @param name
     *        the name of the statement option whose value is being requested (cannot be empty)
     * @return the option value or <code>null</code> if not found
     * @throws KException
     *         if an error occurs
     */
    public static String getOption( final UnitOfWork transaction,
                                    final OptionContainer container,
                                    final String name ) throws KException {
        final StatementOption option = getStatementOption( transaction, container, name );

        if ( option == null ) {
            return null;
        }

        return option.getOption( transaction );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param container
     *        the option container whose option descriptor is being requested (cannot be <code>null</code>)
     * @param propName
     *        the name of the statement option whose descriptor is being requested (cannot be empty)
     * @return the option's property descriptor (can be <code>null</code> if not found)
     * @throws KException
     *         if an error occurs
     */
    private static PropertyDescriptor getOptionDescriptor( final UnitOfWork transaction,
                                                           final OptionContainer container,
                                                           final String propName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( container, "container" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$

        PropertyDescriptor result = null;

        // see if standard statement option
        if ( container.isStandardOption( propName ) ) {
            result = new OptionDescriptor( propName, container.getStandardOptions().get( propName ) );
        } else {
            // see if there is a custom statement option persisted
            final StatementOption[] customOptions = getCustomOptions( transaction, container );

            if ( customOptions.length > 0 ) {
                for ( final StatementOption option : customOptions ) {
                    if ( propName.equals( option.getName( transaction ) ) ) {
                        result = option.getDescriptor( transaction );
                        break;
                    }
                }
            }
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose custom option names are being requested (cannot be <code>null</code>)
     * @return the names of the statement options that have been set (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public static String[] getOptionNames( final UnitOfWork transaction,
                                           final OptionContainer container ) throws KException {
        final StatementOption[] options = getOptions( transaction, container );
        final String[] names = new String[ options.length ];
        int i = 0;

        for ( final StatementOption option : options ) {
            names[i++] = option.getName( transaction );
        }

        return names;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose options are being requested (cannot be <code>null</code>)
     * @return the statement options (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public static StatementOption[] getOptions( final UnitOfWork transaction,
                                                final OptionContainer container ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( container, "container" ); //$NON-NLS-1$

        // must create a generic KomodoObject here so that we can get to the getChildrenOfType that does not filter result
        final KomodoObject same = new ObjectImpl( container.getRepository(), container.getAbsolutePath(), container.getIndex() );
        final List< StatementOption > result = new ArrayList< StatementOption >();

        for ( final KomodoObject kobject : same.getChildrenOfType( transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION ) ) {
            final StatementOption option = new StatementOptionImpl( transaction,
                                                                    container.getRepository(),
                                                                    kobject.getAbsolutePath() );
            result.add( option );
        }

        if ( result.isEmpty() ) {
            return StatementOption.NO_OPTIONS;
        }

        return result.toArray( new StatementOption[ result.size() ] );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose property or statement option is being requested (cannot be <code>null</code>)
     * @param name
     *        the name of the property or statement option being requested (cannot be empty)
     * @param property
     *        the property with the given name or <code>null</code> if it does not exist
     * @return the property, statement option or <code>null</code> if not found
     * @throws KException
     *         if an error occurs
     */
    public static Property getProperty( final UnitOfWork transaction,
                                        final OptionContainer container,
                                        final String name,
                                        final Property property ) throws KException {
        // if a property was not passed in see if a statement option has been set with that name
        if ( property == null ) {
            return getStatementOption( transaction, container, name );
        }

        return property;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose property or statement option descriptor is being requested (cannot be <code>null</code>)
     * @param name
     *        the name of the property or statement option whose descriptor is being requested (cannot be empty)
     * @param descriptor
     *        the property descriptor with the given name or <code>null</code> if it does not exist
     * @return the property or statement option descriptor or <code>null</code> if not found
     * @throws KException
     *         if an error occurs
     */
    public static PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                                            final OptionContainer container,
                                                            final String name,
                                                            final PropertyDescriptor descriptor ) throws KException {
        // if a descriptor was not passed in see if a statement option descriptor with that name is available
        if ( descriptor == null ) {
            return getOptionDescriptor( transaction, container, name );
        }

        return descriptor;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose property or statement option descriptor is being requested (cannot be <code>null</code>)
     * @param propNames
     *        the names of the properties that have values (cannot be <code>null</code>)
     * @return the names of the properties and statement options that have values (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static String[] getPropertyNames( final UnitOfWork transaction,
                                             final OptionContainer container,
                                             final String[] propNames ) throws KException {
        final String[] optionNames = getOptionNames( transaction, container );

        // combine
        final String[] result = new String[ propNames.length + optionNames.length ];
        System.arraycopy( propNames, 0, result, 0, propNames.length );
        System.arraycopy( optionNames, 0, result, propNames.length, optionNames.length );

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose statement option is being requested (cannot be <code>null</code>)
     * @param name
     *        the name of the statement option being requested (cannot be empty)
     * @return the statement option or <code>null</code> if not found
     * @throws KException
     *         if an error occurs
     */
    public static StatementOption getStatementOption( final UnitOfWork transaction,
                                                      final OptionContainer container,
                                                      final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        StatementOption result = null;
        final StatementOption[] options = getOptions( transaction, container );

        if ( options.length != 0 ) {
            for ( final StatementOption option : options ) {
                if ( name.equals( option.getName( transaction ) ) ) {
                    result = option;
                    break;
                }
            }
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param container
     *        the option container being checked (cannot be <code>null</code>)
     * @param name
     *        the name the custom statement option whose existence is being checked (cannot be empty)
     * @return <code>true</code> if the custom statement option exists
     * @throws KException
     *         if an error occurs
     */
    public static boolean hasCustomOption( final UnitOfWork transaction,
                                           final OptionContainer container,
                                           final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( container, "container" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        if ( container.isStandardOption( name ) ) {
            return false;
        }

        return ( getOption( transaction, container, name ) != null );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param container
     *        the option container being checked (cannot be <code>null</code>)
     * @param name
     *        the name the property or statement option whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a property or statement option with the supplied name exists
     * @throws KException
     *         if an error occurs
     */
    public static boolean hasOption( final UnitOfWork transaction,
                                     final OptionContainer container,
                                     final String name ) throws KException {
        return ( getStatementOption( transaction, container, name ) != null );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param container
     *        the option container being checked (cannot be <code>null</code>)
     * @return <code>true</code> if statement options exist
     * @throws KException
     *         if an error occurs
     */
    public static boolean hasOptions( final UnitOfWork transaction,
                                      final OptionContainer container ) throws KException {
        return ( getOptions( transaction, container ).length > 0 );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param container
     *        the option container being checked (cannot be <code>null</code>)
     * @param propertiesExist
     *        <code>true</code> if container has properties
     * @return <code>true</code> if properties or statement options exist
     * @throws KException
     *         if an error occurs
     */
    public static boolean hasProperties( final UnitOfWork transaction,
                                         final OptionContainer container,
                                         final boolean propertiesExist ) throws KException {
        return ( propertiesExist || hasOptions( transaction, container ) );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param container
     *        the option container being checked (cannot be <code>null</code>)
     * @param name
     *        the name of the property or statement option (cannot be empty)
     * @param propertyExists
     *        <code>true</code> if container has the specified property
     * @return <code>true</code> if a property or statement options with the specified name exists
     * @throws KException
     *         if an error occurs
     */
    public static boolean hasProperty( final UnitOfWork transaction,
                                       final OptionContainer container,
                                       final String name,
                                       final boolean propertyExists ) throws KException {
        return ( propertyExists || hasOption( transaction, container, name ) );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose option is being removed (cannot be <code>null</code>)
     * @param optionToRemove
     *        the name of the statement option being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public static void removeOption( final UnitOfWork transaction,
                                     final OptionContainer container,
                                     final String optionToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( container, "container" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionToRemove, "optionToRemove" ); //$NON-NLS-1$

        boolean found = false;
        final StatementOption[] options = getOptions( transaction, container );

        if ( options.length != 0 ) {
            for ( final StatementOption option : options ) {
                if ( optionToRemove.equals( option.getName( transaction ) ) ) {
                    option.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.STATEMENT_OPTION_NOT_FOUND_TO_REMOVE, optionToRemove ) );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose option is being set (cannot be <code>null</code>)
     * @param optionName
     *        the name of the statement option being added (cannot be empty)
     * @param optionValue
     *        the statement option value (can be empty if removing the option)
     * @return the statement option (<code>null</code> if removed)
     * @throws KException
     *         if an error occurs
     */
    public static StatementOption setOption( final UnitOfWork transaction,
                                             final OptionContainer container,
                                             final String optionName,
                                             final String optionValue ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( container, "container" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionName, "optionName" ); //$NON-NLS-1$

        if ( StringUtils.isBlank( optionValue ) ) {
            removeOption( transaction, container, optionName );
            return null;
        }

        StatementOption result = getStatementOption( transaction, container, optionName );

        if ( result == null ) {
            result = RelationalModelFactory.createStatementOption( transaction,
                                                                   container.getRepository(),
                                                                   container,
                                                                   optionName,
                                                                   optionValue );
        } else {
            result.setOption( transaction, optionValue );
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param container
     *        the option container whose option is being set (cannot be <code>null</code>)
     * @param propertyName
     *        the name of the option (cannot be empty)
     * @param values
     *        the new values (can be <code>null</code>)
     * @return <code>true</code> if an option was set
     * @throws KException
     *         if an error occurs
     */
    public static boolean setProperty( final UnitOfWork transaction,
                                       final OptionContainer container,
                                       final String propertyName,
                                       final Object... values ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$

        if ( container.isStandardOption( propertyName ) || hasCustomOption( transaction, container, propertyName ) ) {
            // must be a single-valued string, null, or empty
            String newValue = null;

            if ( values != null ) {
                if ( ( values.length == 1 ) && ( values[0] != null ) ) {
                    newValue = values[0].toString();
                } else if ( values.length > 1 ) {
                    Messages.getString( org.komodo.repository.Messages.Komodo.UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
                                        propertyName,
                                        container.getAbsolutePath() );
                }
            }

            setOption( transaction, container, propertyName, newValue );
            return true;
        }

        return false;
    }

    /**
     * Don't allow construction outside of this class.
     */
    private OptionContainerUtils() {
        // nothing to do
    }

}
