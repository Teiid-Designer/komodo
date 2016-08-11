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
package org.komodo.shell.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.RepositoryTools;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyDescriptor.Type;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.utils.TextFormat;
import org.komodo.ui.DefaultLabelProvider;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 *  Utilities for working with KomodoObject
 */
public class KomodoObjectUtils implements StringConstants {

    /**
     * A string representation of {@link Boolean#TRUE}.
     */
    public static final String TRUE_STRING = Boolean.TRUE.toString();

    /**
     * A string representation of {@link Boolean#FALSE}.
     */
    public static final String FALSE_STRING = Boolean.FALSE.toString();

    /**
     * Get property names for a KomodoObject
     * @param wsStatus the WorkspaceStatus
     * @param kObj the KomodoObject
     * @return the list of property names
     * @throws Exception the exception
     */
    public static List< String > getProperties(final WorkspaceStatus wsStatus, final KomodoObject kObj) throws Exception {
        KomodoObject resolvedObj = wsStatus.resolve(kObj);
        if(resolvedObj==null) resolvedObj = kObj;
        final List< String > props = new ArrayList<>( Arrays.asList( resolvedObj.getPropertyNames( wsStatus.getTransaction() ) ) ); // props with values
        final PropertyDescriptor[] descriptors = resolvedObj.getPropertyDescriptors( wsStatus.getTransaction() );

        if ( descriptors.length != 0 ) {
            for ( final PropertyDescriptor descriptor : descriptors ) {
                final String name = descriptor.getName();

                if ( !props.contains( name ) ) {
                    props.add( name );
                }
            }
        }

        return props;
    }

    /**
     * @param wsStatus the WorkspaceStatus
     * @param kObj the KomodoObject
     * @return the names of the unfiltered properties that have values (includes those that have not been set yet)
     * @throws Exception
     *         if an error occurs
     */
    public static List< String > getUnfilteredProperties(final WorkspaceStatus wsStatus, final KomodoObject kObj) throws Exception {
        KomodoObject resolvedObj = wsStatus.resolve(kObj);
        final List< String > props = new ArrayList<>( Arrays.asList( resolvedObj.getRawPropertyNames( wsStatus.getTransaction() ) ) ); // props with values
        final PropertyDescriptor[] descriptors = resolvedObj.getRawPropertyDescriptors( wsStatus.getTransaction() );

        if ( descriptors.length != 0 ) {
            for ( final PropertyDescriptor descriptor : descriptors ) {
                final String name = descriptor.getName();

                if ( !props.contains( name ) ) {
                    props.add( name );
                }
            }
        }

        return props;
    }

    /**
     * Get property value for the supplied property name
     * @param wsStatus the WorkspaceStatus
     * @param kObj the KomodoObject
     * @param propertyName the property name
     * @return the property value
     * @throws Exception the exception
     */
    public static String getPropertyValue( final WorkspaceStatus wsStatus, final KomodoObject kObj, final String propertyName ) throws Exception {
        KomodoObject resolvedObj = wsStatus.resolve(kObj);
        if(resolvedObj==null) resolvedObj = kObj;
        if ( resolvedObj.hasProperty( wsStatus.getTransaction(), propertyName ) ) {
            final Property property = resolvedObj.getProperty( wsStatus.getTransaction(), propertyName );
            final String displayValue = RepositoryTools.getDisplayValue( wsStatus.getTransaction(), property );

            // If value is a path, convert it to a display path
            final Type type = property.getDescriptor( wsStatus.getTransaction() ).getType();
            final boolean propIsReference = ( ( Type.REFERENCE == type ) || ( Type.WEAKREFERENCE == type ) );
            if(propIsReference) {
                // Multiple references need to be converted from repo paths to display paths
                if(property.isMultiple(wsStatus.getTransaction())) {
                    // Remove square brackets
                    String absPaths = displayValue.replaceAll("\\[|\\]", "");  //$NON-NLS-1$ //$NON-NLS-2$
                    // Get multiple paths are separated by commas
                    String[] splitPaths = absPaths.split(","); //$NON-NLS-1$

                    // Convert repo paths to display paths, returning the multiple display paths
                    StringBuilder sb = new StringBuilder("["); //$NON-NLS-1$
                    boolean first = true;
                    for(String absPath : splitPaths) {
                        if(!absPath.isEmpty()) {
                            if(!first) sb.append(","); //$NON-NLS-1$
                            sb.append( wsStatus.getObjectLabelProvider( resolvedObj ).getDisplayPath( wsStatus.getTransaction(),
                                                                                                      absPath,
                                                                                                      null ) );
                            first = false;
                        }
                    }
                    sb.append("]"); //$NON-NLS-1$
                    return sb.toString();
                } else {
                    return wsStatus.getObjectLabelProvider( resolvedObj ).getDisplayPath( wsStatus.getTransaction(),
                                                                                          displayValue,
                                                                                          null );
                }
            }

            return displayValue;
        }

        return null;
    }

    /**
     * @param wsStatus the WorkspaceStatus
     * @param kObj the KomodoObject
     * @param propertyName
     *        the name of the unfiltered being requested (cannot be empty)
     * @return the property value (never empty)
     * @throws Exception
     *         if an error occurs
     */
    public static String getUnfilteredPropertyValue( final WorkspaceStatus wsStatus, final KomodoObject kObj, final String propertyName ) throws Exception {
        KomodoObject resolvedObj = wsStatus.resolve(kObj);
        if ( resolvedObj.hasRawProperty( wsStatus.getTransaction(), propertyName ) ) {
            final Property property = resolvedObj.getRawProperty( wsStatus.getTransaction(), propertyName );
            return RepositoryTools.getDisplayValue( wsStatus.getTransaction(), property );
        }

        return I18n.bind( ShellI18n.noPropertyValue );
    }

    /**
     * @param wsStatus the WorkspaceStatus
     * @param context the KomodoObject
     *        the associated context (cannot be null)
     * @param propertyName
     *        the name whose namespace prefix is being attached (cannot be empty)
     * @return the property name with the namespace prefix attached (never empty)
     * @throws Exception
     *         if an error occurs
     */
    public static String attachPrefix( final WorkspaceStatus wsStatus, final KomodoObject context,
                                          final String propertyName ) throws Exception {
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$

        for ( final String name : getProperties(wsStatus,context) ) {
            if ( propertyName.equals( removePrefix( name ) ) ) {
                return name;
            }
        }

        return propertyName;
    }

    /**
     * @param propertyName
     *        the property name whose namespace prefix is being removed (cannot be empty)
     * @return the name without the namespace prefix (never empty)
     */
    public static String removePrefix( final String propertyName ) {
        ArgCheck.isNotEmpty( propertyName, "qname" ); //$NON-NLS-1$
        final int index = propertyName.indexOf( ':' );

        if ( index == -1 ) {
            return propertyName;
        }

        if ( index < propertyName.length() ) {
            return propertyName.substring( index + 1 );
        }

        return propertyName;
    }

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     * @param kobject
     *        the object whose short name is being requested (cannot be <code>null</code>)
     * @param format
     *        the text format or <code>null</code> if no formatting is desired
     * @return the object's full, qualified path (never empty)
     */
    public static String getShortName( final WorkspaceStatus wsStatus,
                                       final KomodoObject kobject,
                                       final TextFormat format ) {
        final String name = wsStatus.getObjectLabelProvider(kobject).getDisplayName( wsStatus.getTransaction(), kobject, format );

        if ( StringUtils.isBlank( name ) ) {
            final String[] segments = kobject.getAbsolutePath().split( FORWARD_SLASH );
            return segments[ segments.length - 1 ];
        }

        return name;
    }

    /**
     * Obtain the name or path of the specified object depending on the value of the
     * {@link WorkspaceStatus#SHOW_FULL_PATH_IN_PROMPT_KEY} global property.
     *
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     * @param kobject
     *        the object whose display name is being requested (cannot be <code>null</code>)
     * @param format
     *        the text format or <code>null</code> if no formatting is desired
     * @return the object's full, qualified path (never empty)
     */
    public static String getDisplayName( final WorkspaceStatus wsStatus,
                                         final KomodoObject kobject,
                                         final TextFormat format ) {
        ArgCheck.isNotNull( wsStatus, "wsStatus" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        if ( wsStatus.isShowingFullPathInPrompt() ) {
            return wsStatus.getDisplayPath( kobject, format );
        }

        return getShortName( wsStatus, kobject, format );
    }

    /**
     * Determine if the supplied KomodoObject is the root (tko:komodo)
     *
     * @param kObject
     *        the KomodoObject (cannot be <code>null</code>)
     * @return 'true' if root, 'false' if not
     */
    public static boolean isRoot( final KomodoObject kObject ) {
        ArgCheck.isNotNull( kObject, "kobject" ); //$NON-NLS-1$

        final String contextPath = kObject.getAbsolutePath();

        // /tko:komodo
        if ( DefaultLabelProvider.ROOT_PATH.equals( contextPath ) || DefaultLabelProvider.ROOT_SLASH_PATH.equals( contextPath ) ) {
            return true;
        }
        return false;
    }

    /**
     * Determine if the supplied KomodoObject is one of the root children (tko:komodo/tko:workspace, tko:komodo/tko:library, tko:komodo/tko:environment)
     * @param transaction 
     *
     * @param kObject
     *        the KomodoObject (cannot be <code>null</code>)
     * @return 'true' if root child, 'false' if not
     */
    public static boolean isRootChild( UnitOfWork transaction, final KomodoObject kObject ) {
        ArgCheck.isNotNull( kObject, "kobject" ); //$NON-NLS-1$

        final String contextPath = kObject.getAbsolutePath();
        return RepositoryImpl.isReservedPath(contextPath);
    }

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     * @param context
     *        the object whose multi-valued property value is being concatenated (cannot be <code>null</code>)
     * @param name
     *        the name of the multi-value property (cannot be empty)
     * @return the concatenated value or <code>null</code> if no value exists
     * @throws Exception
     *         if property is not a multi-valued property or if an error occurs
     */
    public static String concatMultiValues( final WorkspaceStatus status,
                                            final KomodoObject context,
                                            final String name ) throws Exception {
        ArgCheck.isNotNull( status, "status" ); //$NON-NLS-1$
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$
        ArgCheck.isNotNull( name, "name" ); //$NON-NLS-1$

        final String propertyName = status.isShowingPropertyNamePrefixes() ? name : attachPrefix( status, context, name );

        if ( !context.hasProperty( status.getTransaction(), name ) ) {
            return null;
        }

        if ( !isMultiValuedProperty( status, context, name ) ) {
            throw new KException( I18n.bind( ShellI18n.notMultiValuedProperty, name, context.getAbsolutePath() ) );
        }

        final Property property = context.getProperty( status.getTransaction(), propertyName );
        final StringBuilder result = new StringBuilder();
        boolean quoted = false;
        boolean firstTime = true;

        for ( final Object value : property.getValues( status.getTransaction() ) ) {
            if ( !firstTime ) {
                result.append( ',' );
            } else {
                firstTime = false;
            }

            // TODO need to account for escaped values

            final Type type = property.getDescriptor( status.getTransaction() ).getType();
            final boolean propIsReference = ( ( Type.REFERENCE == type ) || ( Type.WEAKREFERENCE == type ) );
            String valueAsText = null;

            if ( propIsReference ) {
                final String path = RepositoryTools.findPathOfReference( status.getTransaction(),
                                                                         property.getRepository(),
                                                                         value.toString() );
                valueAsText = ( StringUtils.isBlank( path ) ? value.toString()
                                                            : status.getObjectLabelProvider( context )
                                                                    .getDisplayPath( status.getTransaction(), path, null ) );
            } else {
                valueAsText = value.toString();

                if ( ( valueAsText.indexOf( ' ' ) != -1 ) && !quoted ) {
                    quoted = true;
                    result.insert( 0, '"' );
                }
            }

            result.append( valueAsText );
        }

        if ( quoted ) {
            result.append( '"' );
        }

        return result.toString();
    }

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     * @param context
     *        the object whose property is being checked (cannot be <code>null</code>)
     * @param name
     *        the name of the property (cannot be empty)
     * @return <code>true</code> if the property is multi-valued
     * @throws Exception
     *         if an error occurs
     */
    public static boolean isMultiValuedProperty( final WorkspaceStatus status,
                                                 final KomodoObject context,
                                                 final String name ) throws Exception {
        ArgCheck.isNotNull( status, "status" ); //$NON-NLS-1$
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$
        ArgCheck.isNotNull( name, "name" ); //$NON-NLS-1$

        final String propertyName = status.isShowingPropertyNamePrefixes() ? name : attachPrefix( status, context, name );
        final PropertyDescriptor descriptor = context.getPropertyDescriptor( status.getTransaction(), propertyName );
        return ( ( descriptor == null ) ? false : descriptor.isMultiple() );
    }

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     * @param propName
     *        the property name (cannot be empty)
     * @param context
     *        the object whose property is being validated (cannot be <code>null</code>)
     * @return <code>true</code> if the property exists or a property descriptor exists
     * @exception Exception
     *            if there is a problem obtaining property information
     */
    public static boolean isValidProperty( final WorkspaceStatus status,
                                           final String propName,
                                           final KomodoObject context ) throws Exception {
        ArgCheck.isNotNull( status, "status" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$

        if ( !StringUtils.isEmpty( propName ) ) {
            final List< String > propNames = getProperties( status, context );

            if ( propNames.contains( propName )
                 || ( !status.isShowingPropertyNamePrefixes()
                      && propNames.contains( attachPrefix( status, context, propName ) ) ) ) {
                return true;
            }
        }

        return false;
    }

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     * @param propName
     *        the property name (cannot be empty)
     * @param propValue
     *        the property value (can be empty)
     * @param context
     *        the workspace context (cannot be <code>null</code>)
     * @return <code>true</code> if the value is valid for the specified property
     * @throws KException
     *         if an error occurs
     */
    public static boolean isValidPropertyValue( final WorkspaceStatus status,
                                                final String propName,
                                                final String propValue,
                                                final KomodoObject context ) throws KException {
        ArgCheck.isNotNull( status, "status" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$

        final PropertyDescriptor descriptor = context.getPropertyDescriptor( status.getTransaction(), propName );

        // empty value is not valid if mandatory
        if ( StringUtils.isBlank( propValue ) ) {
            return ( ( descriptor == null ) || !descriptor.isMandatory() );
        }

        try {
            // multi-valued property
            if ( isMultiValuedProperty( status, context, propName ) ) {
                final String[] multiValues = parseMultiValues( propValue );

                if ( multiValues.length == 0 ) {
                    return false;
                }

                // make sure each value is valid
                for ( final String value : multiValues ) {
                    if ( !isValidValue( value, descriptor ) ) {
                        return false;
                    }
                }

                return true;
            }

            // single valued property
            return isValidValue( propValue, descriptor );
        } catch ( final Exception e ) {
            return false;
        }
    }

    private static boolean isValidValue( final String value,
                                         final PropertyDescriptor descriptor ) {
        if ( StringUtils.isBlank( value ) ) {
            return ( ( descriptor == null ) || !descriptor.isMandatory() );
        }

        if ( descriptor != null ) {
            if ( descriptor.getType() == Type.BOOLEAN ) {
                return ( FALSE_STRING.equals( value ) || TRUE_STRING.equals( value ) );
            }

            // TODO: maybe add logic to test other property types
        }

        return true;
    }

    /**
     * @param valuesString
     *        the string being parsed into values (can be empty)
     * @return the values (never <code>null</code> but can be empty)
     */
    public static String[] parseMultiValues( final String valuesString ) {
        if ( StringUtils.isBlank( valuesString ) ) {
            return StringConstants.EMPTY_ARRAY;
        }

        // TODO need to account for escaped values

        String multiValues = null;

        // strip off leading and trailing quotes if necessary
        if ( valuesString.startsWith( "\"" ) && valuesString.endsWith( "\"" ) ) { //$NON-NLS-1$ //$NON-NLS-2$
            if ( valuesString.length() == 2 ) {
                return StringConstants.EMPTY_ARRAY;
            }

            multiValues = valuesString.substring( 1, valuesString.length() - 1 );
        } else {
            multiValues = valuesString;
        }

        return multiValues.split( "," ); //$NON-NLS-1$
    }

}
