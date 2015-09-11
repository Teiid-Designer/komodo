/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.util.Arrays;
import java.util.Objects;
import org.komodo.relational.vdb.DataRole;
import org.komodo.spi.constants.StringConstants;

/**
 * A data role that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyDataRole",
 *     "description" : "data role description goes here",
 *     "allowCreateTempTables" : true,
 *     "anyAuthenticated" : true,
 *     "grantAll" : true,
 *     "mappedRoles" : [
 *         "admin", "tester", "developer", "manager"
 *     ],
 *     "permissions" : [
 *         {
 *             "id" : "MyPermission",
 *             "allowAlter" : true,
 *             "allowCreate" : true,
 *             "allowDelete" : true,
 *             "allowExecute" : true,
 *             "allowLanguage" : true,
 *             "allowRead" : true,
 *             "allowUpdate" : true,
 *             "conditions" : {
 *                 "cant" : true,
 *                 "buy" : false,
 *                 "me" : true,
 *                 "love" : false
 *             },
 *             "masks" : {
 *                 "love" : "words",
 *                 "me" : "of",
 *                 "do" : "love"
 *             }
 *         },
 *         {
 *             "id" : "YourPermission",
 *             "allowAlter" : false,
 *             "allowCreate" : false,
 *             "allowDelete" : false,
 *             "allowExecute" : false,
 *             "allowLanguage" : false,
 *             "allowRead" : false,
 *             "allowUpdate" : false,
 *             "conditions" : {
 *                 "watching" : true,
 *                 "the" : true,
 *                 "detectives" : false
 *             },
 *             "masks" : {
 *                 "beatonthebrat" : "withabaseballbat"
 *             }
 *         }
 *     ]
 * }
 * </code>
 * </pre>
 */
public final class RestVdbDataRole extends KomodoRestEntity {

    /**
     * An empty array of data roles.
     */
    public static final RestVdbDataRole[] NO_DATA_ROLES = new RestVdbDataRole[ 0 ];

    private String name;
    private String description;
    private boolean allowCreateTempTables = DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES;
    private boolean anyAuthenticated = DataRole.DEFAULT_ANY_AUTHENTICATED;
    private boolean grantAll = DataRole.DEFAULT_GRANT_ALL;
    private String[] mappedRoles = StringConstants.EMPTY_ARRAY;
    private RestVdbPermission[] permissions = RestVdbPermission.NO_PERMISSIONS;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbDataRole() {
        // nothing to do
    }

    /**
     * Constructs a data role with the specified name.
     *
     * @param name
     *        the data role name (can be empty)
     */
    public RestVdbDataRole( final String name ) {
        this.name = name;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object other ) {
        if ( !super.equals( other ) ) {
            return false;
        }

        assert( other != null );
        assert( getClass().equals( other.getClass() ) );

        final RestVdbDataRole that = ( RestVdbDataRole )other;

        // check name
        if ( this.name == null ) {
            if ( that.name != null ) {
                return false;
            }
        } else if ( !this.name.equals( that.name ) ) {
            return false;
        }

        // check description
        if ( this.description == null ) {
            if ( that.description != null ) {
                return false;
            }
        } else if ( !this.description.equals( that.description ) ) {
            return false;
        }

        if ( ( this.allowCreateTempTables != that.allowCreateTempTables ) || ( this.anyAuthenticated != that.anyAuthenticated )
             || ( this.grantAll != that.grantAll ) ) {
            return false;
        }

        // mapped roles
        if ( !Arrays.deepEquals( this.mappedRoles, that.mappedRoles ) ) {
            return false;
        }

        // permissions
        if ( !Arrays.deepEquals( this.permissions, that.permissions ) ) {
            return false;
        }

        return true;
    }

    /**
     * @return the description (can be empty)
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @return the mapped roles (never <code>null</code> but can be empty)
     */
    public String[] getMappedRoles() {
        return this.mappedRoles;
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the permission (never <code>null</code> but can be empty)
     */
    public RestVdbPermission[] getPermissions() {
        return this.permissions;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.name,
                             this.description,
                             this.allowCreateTempTables,
                             this.anyAuthenticated,
                             this.grantAll,
                             Arrays.deepHashCode( this.mappedRoles ),
                             Arrays.deepHashCode( this.permissions ),
                             super.hashCode() );
    }

    /**
     * @return <code>true</code> if allows creating temp tables
     */
    public boolean isAllowCreateTempTables() {
        return this.allowCreateTempTables;
    }

    /**
     * @return <code>true</code> if any authenticated
     */
    public boolean isAnyAuthenticated() {
        return this.anyAuthenticated;
    }

    /**
     * @return <code>true</code> if allowed grant all permissions
     */
    public boolean isGrantAll() {
        return this.grantAll;
    }

    /**
     * @param newAllowCreateTempTables
     *        <code>true</code> if allows creating temp tables
     */
    public void setAllowCreateTempTables( final boolean newAllowCreateTempTables ) {
        this.allowCreateTempTables = newAllowCreateTempTables;
    }

    /**
     * @param newAnyAuthenticated
     *        <code>true</code> if any authenticated
     */
    public void setAnyAuthenticated( final boolean newAnyAuthenticated ) {
        this.anyAuthenticated = newAnyAuthenticated;
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        this.description = newDescription;
    }

    /**
     * @param newGrantAll
     *        <code>true</code> if allows grant all
     */
    public void setGrantAll( final boolean newGrantAll ) {
        this.grantAll = newGrantAll;
    }

    /**
     * @param newMappedRoles
     *        the new mapped roles (can be <code>null</code>)
     */
    public void setMappedRoles( final String[] newMappedRoles ) {
        this.mappedRoles = ( ( newMappedRoles == null ) ? StringConstants.EMPTY_ARRAY : newMappedRoles );
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @param newPermissions
     *        the new permissions (can be <code>null</code>)
     */
    public void setPermissions( final RestVdbPermission[] newPermissions ) {
        this.permissions = ( ( newPermissions == null ) ? RestVdbPermission.NO_PERMISSIONS : newPermissions );
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append( "data role name = " ).append( this.name ); //$NON-NLS-1$
        return builder.toString();
    }

}
