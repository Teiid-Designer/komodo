/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.core.KomodoLexicon.Komodo;
import org.komodo.repository.RepositoryTools;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.ArgCheck;

/**
 *  Utilities for working with KomodoObject
 */
public class KomodoObjectUtils implements StringConstants {

    /**
     * Checks whether the supplied KomodoObject is the root context
     * @param wsStatus the WorkspaceStatus
     * @param kObject the KomodoObject
     * @return 'true' if the supplied kObj is the root context, 'false' if not.
     */
    public static boolean isRootContext(final WorkspaceStatus wsStatus, final KomodoObject kObject) {
        if(kObject==wsStatus.getRootContext()) {
            return true;
        }
        return false;
    }
    
    /**
     * Get the full name path for this context.  e.g. home.parentContext.thisContext
     * @param wsStatus the WorkspaceStatus
     * @param kObj the KomodoObject
     * @return the full name
     * @throws Exception if error occurs
     */
    public static String getName(final WorkspaceStatus wsStatus, final KomodoObject kObj) throws Exception {
        KomodoObject resolvedObj = wsStatus.resolve(kObj);
        String name = resolvedObj.getName(wsStatus.getTransaction());
        if(name.equals(Komodo.NODE_TYPE)) {
            name = ContextUtils.KOMODO_DISPLAY_NAME;
        } else if(name.equals(Komodo.ENVIRONMENT)) {
            name = ContextUtils.ENV_DISPLAY_NAME;
        } else if(name.equals(Komodo.LIBRARY)) {
            name = ContextUtils.LIBRARY_DISPLAY_NAME;
        } else if(name.equals(Komodo.WORKSPACE)) {
            name = ContextUtils.WORKSPACE_DISPLAY_NAME;
        }

        return name;
    }

    /**
     * Get the full name path for this context.  e.g. home.parentContext.thisContext
     * @param wsStatus the WorkspaceStatus
     * @param kObj the KomodoObject
     * @return the full name
     * @throws Exception if error occurs
     */
    public static String getFullName(final WorkspaceStatus wsStatus, final KomodoObject kObj) throws Exception {
        KomodoObject resolvedObj = wsStatus.resolve(kObj);
        StringBuffer sb = new StringBuffer();
        if(!wsStatus.isRoot(resolvedObj)) {
            KomodoObject parentObj = resolvedObj.getParent(wsStatus.getTransaction());
            parentObj = wsStatus.resolve(parentObj);
            sb.append(getFullName(wsStatus,parentObj));
        }
        
        sb.append(File.separator);
        sb.append(getName(wsStatus,resolvedObj));

        return sb.toString();
    }
    
    /**
     * Get property names for a KomodoObject
     * @param wsStatus the WorkspaceStatus
     * @param kObj the KomodoObject
     * @return the list of property names
     * @throws Exception the exception
     */
    public static List< String > getProperties(final WorkspaceStatus wsStatus, final KomodoObject kObj) throws Exception {
        KomodoObject resolvedObj = wsStatus.resolve(kObj);
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


}
