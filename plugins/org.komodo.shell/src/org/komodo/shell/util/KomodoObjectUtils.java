/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.repository.RepositoryTools;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.KomodoObjectLabelProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 *  Utilities for working with KomodoObject
 */
public class KomodoObjectUtils implements StringConstants {

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
            //final Type type = property.getDescriptor( wsStatus.getTransaction() ).getType();
            //final boolean propIsReference = ( ( Type.REFERENCE == type ) || ( Type.WEAKREFERENCE == type ) );
            final String displayValue = RepositoryTools.getDisplayValue( wsStatus.getTransaction(), property );

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

        return Messages.getString( SHELL.NO_PROPERTY_VALUE );
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

        for ( final String name : KomodoObjectUtils.getProperties(wsStatus,context) ) {
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
     *        the object whose full path is being requested (cannot be <code>null</code>)
     * @return the object's full, qualified path (never empty)
     */
    public static String getFullName( final WorkspaceStatus wsStatus,
                                      final KomodoObject kobject ) {
        final String path = wsStatus.getLabelProvider().getDisplayPath( kobject );

        if ( StringUtils.isBlank( path ) ) {
            return kobject.getAbsolutePath();
        }

        return path;
    }

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     * @param kobject
     *        the object whose short name is being requested (cannot be <code>null</code>)
     * @return the object's full, qualified path (never empty)
     */
    public static String getShortName( final WorkspaceStatus wsStatus,
                                      final KomodoObject kobject ) {
        final String name = wsStatus.getLabelProvider().getDisplayName( kobject );

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
     * @return the object's full, qualified path (never empty)
     */
    public static String getDisplayName( final WorkspaceStatus wsStatus,
                                         final KomodoObject kobject ) {
        ArgCheck.isNotNull( wsStatus, "wsStatus" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        if ( wsStatus.isShowingFullPathInPrompt() ) {
            return getFullName( wsStatus, kobject );
        }

        return getShortName( wsStatus, kobject );
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
        if ( KomodoObjectLabelProvider.ROOT_PATH.equals( contextPath ) || KomodoObjectLabelProvider.ROOT_SLASH_PATH.equals( contextPath ) ) {
            return true;
        }
        return false;
    }
    
    /**
     * Determine if the supplied KomodoObject is one of the root children (tko:komodo/tko:workspace, tko:komodo/tko:library, tko:komodo/tko:environment)
     *
     * @param kObject
     *        the KomodoObject (cannot be <code>null</code>)
     * @return 'true' if root child, 'false' if not
     */
    public static boolean isRootChild( final KomodoObject kObject ) {
        ArgCheck.isNotNull( kObject, "kobject" ); //$NON-NLS-1$

        final String contextPath = kObject.getAbsolutePath();

        // /tko:komodo/workspace
        if ( KomodoObjectLabelProvider.WORKSPACE_PATH.equals( contextPath ) || KomodoObjectLabelProvider.WORKSPACE_SLASH_PATH.equals( contextPath ) ) {
            return true;
        }

        // /tko:komodo/library
        if ( KomodoObjectLabelProvider.LIB_PATH.equals( contextPath ) || KomodoObjectLabelProvider.LIB_SLASH_PATH.equals( contextPath ) ) {
            return true;
        }

        // /tko:komodo/environment
        if ( KomodoObjectLabelProvider.ENV_PATH.equals( contextPath ) || KomodoObjectLabelProvider.ENV_SLASH_PATH.equals( contextPath ) ) {
            return true;
        }
        return false;
    }

}
