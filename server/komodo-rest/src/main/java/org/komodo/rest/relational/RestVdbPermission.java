/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import org.komodo.relational.vdb.Permission;
import org.komodo.rest.KomodoRestEntity;

/**
 * A data role that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyPermission",
 *     "allowAlter" : true,
 *     "allowCreate" : true,
 *     "allowDelete" : true,
 *     "allowExecute" : true,
 *     "allowLanguage" : true,
 *     "allowRead" : true,
 *     "allowUpdate" : true,
 *     "conditions" : {
 *         "cant" : true,
 *         "buy" : false,
 *         "me" : true,
 *         "love" : false
 *     },
 *     "masks" : {
 *         "love" : "words",
 *         "me" : "of",
 *         "do" : "love"
 *     }
 * }
 * </code>
 * </pre>
 */
public final class RestVdbPermission extends KomodoRestEntity {

    /**
     * An empty array of permissions.
     */
    public static final RestVdbPermission[] NO_PERMISSIONS = new RestVdbPermission[ 0 ];

    private String name;
    private boolean allowAlter = Permission.DEFAULT_ALLOW_ALTER;
    private boolean allowCreate = Permission.DEFAULT_ALLOW_CREATE;
    private boolean allowDelete = Permission.DEFAULT_ALLOW_DELETE;
    private boolean allowExecute = Permission.DEFAULT_ALLOW_EXECUTE;
    private boolean allowLanguage = Permission.DEFAULT_ALLOW_LANGUAGE;
    private boolean allowRead = Permission.DEFAULT_ALLOW_READ;
    private boolean allowUpdate = Permission.DEFAULT_ALLOW_UPDATE;
    private final Map< String, Boolean > conditions = new HashMap< >();
    private final Map< String, String > masks = new HashMap< >();

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbPermission() {
        // nothing to do
    }

    /**
     * Constructs a permission with the specified name.
     *
     * @param name
     *        the permission name (can be empty)
     */
    public RestVdbPermission( final String name ) {
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

        final RestVdbPermission that = ( RestVdbPermission )other;

        // check name
        if ( this.name == null ) {
            if ( that.name != null ) {
                return false;
            }
        } else if ( !this.name.equals( that.name ) ) {
            return false;
        }

        // allows
        if ( ( this.allowAlter != that.allowAlter ) || ( this.allowCreate != that.allowCreate )
             || ( this.allowDelete != that.allowDelete ) || ( this.allowExecute != that.allowExecute )
             || ( this.allowLanguage != that.allowLanguage ) || ( this.allowRead != that.allowRead )
             || ( this.allowUpdate != that.allowUpdate ) ) {
            return false;
        }

        // conditions
        if ( !this.conditions.equals( that.conditions ) ) {
            return false;
        }

        // masks
        if ( !this.masks.equals( that.masks ) ) {
            return false;
        }

        return true;
    }

    /**
     * @return the conditions (never <code>null</code> but can be empty)
     */
    public Map< String, Boolean > getConditions() {
        return this.conditions;
    }

    /**
     * @return the masks (never <code>null</code> but can be empty)
     */
    public Map< String, String > getMasks() {
        return this.masks;
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.name,
                             this.allowAlter,
                             this.allowCreate,
                             this.allowDelete,
                             this.allowExecute,
                             this.allowLanguage,
                             this.allowRead,
                             this.allowUpdate,
                             this.conditions,
                             this.masks,
                             super.hashCode() );
    }

    /**
     * @return <code>true</code> if allows alter
     */
    public boolean isAllowAlter() {
        return this.allowAlter;
    }

    /**
     * @return <code>true</code> if allows create
     */
    public boolean isAllowCreate() {
        return this.allowCreate;
    }

    /**
     * @return <code>true</code> if allows delete
     */
    public boolean isAllowDelete() {
        return this.allowDelete;
    }

    /**
     * @return <code>true</code> if allows execute
     */
    public boolean isAllowExecute() {
        return this.allowExecute;
    }

    /**
     * @return <code>true</code> if allows language
     */
    public boolean isAllowLanguage() {
        return this.allowLanguage;
    }

    /**
     * @return <code>true</code> if allows read
     */
    public boolean isAllowRead() {
        return this.allowRead;
    }

    /**
     * @return <code>true</code> if allows update
     */
    public boolean isAllowUpdate() {
        return this.allowUpdate;
    }

    /**
     * @param newAllowAlter
     *        <code>true</code> if allows alter
     */
    public void setAllowAlter( final boolean newAllowAlter ) {
        this.allowAlter = newAllowAlter;
    }

    /**
     * @param newAllowCreate
     *        <code>true</code> if allows create
     */
    public void setAllowCreate( final boolean newAllowCreate ) {
        this.allowCreate = newAllowCreate;
    }

    /**
     * @param newAllowDelete
     *        <code>true</code> if allows delete
     */
    public void setAllowDelete( final boolean newAllowDelete ) {
        this.allowDelete = newAllowDelete;
    }

    /**
     * @param newAllowExecute
     *        <code>true</code> if allows execute
     */
    public void setAllowExecute( final boolean newAllowExecute ) {
        this.allowExecute = newAllowExecute;
    }

    /**
     * @param newAllowLanguage
     *        <code>true</code> if allows language
     */
    public void setAllowLanguage( final boolean newAllowLanguage ) {
        this.allowLanguage = newAllowLanguage;
    }

    /**
     * @param newAllowRead
     *        <code>true</code> if allows read
     */
    public void setAllowRead( final boolean newAllowRead ) {
        this.allowRead = newAllowRead;
    }

    /**
     * @param newAllowUpdate
     *        <code>true</code> if allows update
     */
    public void setAllowUpdate( final boolean newAllowUpdate ) {
        this.allowUpdate = newAllowUpdate;
    }

    /**
     * @param newConditions
     *        the new conditions (can be <code>null</code> or empty)
     */
    public void setConditions( final Map< String, Boolean > newConditions ) {
        this.conditions.clear();

        if ( newConditions != null ) {
            this.conditions.putAll( newConditions );
        }
    }

    /**
     * @param newMasks
     *        the new masks (can be <code>null</code> or empty)
     */
    public void setMasks( final Map< String, String > newMasks ) {
        this.masks.clear();

        if ( newMasks != null ) {
            this.masks.putAll( newMasks );
        }
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append( "permission name = " ).append( this.name ); //$NON-NLS-1$
        return builder.toString();
    }

}
