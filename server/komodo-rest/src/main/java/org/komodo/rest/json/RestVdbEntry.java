/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.util.Objects;
import org.komodo.utils.ArgCheck;
import com.google.gson.annotations.SerializedName;

/**
 * A VDB entry that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyEntry",
 *     "path" : "/my/entry/path",
 *     "description" : "translator description goes here"
 * }
 * </code>
 * </pre>
 */
public final class RestVdbEntry extends KomodoRestEntity {

    /**
     * And empty array of VDB entries.
     */
    public static final RestVdbEntry[] NO_ENTRIES = new RestVdbEntry[ 0 ];

    @SerializedName( "id" )
    private String name;
    private String description;
    private String path;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbEntry() {
        // nothing to do
    }

    /**
     * @param name
     *        the name of the entry (cannot be empty)
     * @param path
     *        the path of the entry (cannot be empty)
     */
    public RestVdbEntry( final String name,
                         final String path ) {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( path, "path" ); //$NON-NLS-1$
        this.name = name;
        this.path = path;
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

        final RestVdbEntry that = ( RestVdbEntry )other;

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

        // check type
        if ( this.path == null ) {
            if ( that.path != null ) {
                return false;
            }
        } else if ( !this.path.equals( that.path ) ) {
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
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the entry path (can be empty)
     */
    public String getPath() {
        return this.path;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.name, this.description, this.path, super.hashCode() );
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        this.description = newDescription;
    }

    /**
     * @param newName
     *        the new entry name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @param newPath
     *        the new entry path (can be empty)
     */
    public void setPath( final String newPath ) {
        this.path = newPath;
    }

}
