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
 * A translator that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * {
 *     "id" : "MyTranslator",
 *     "description" : "translator description goes here",
 *     "type" : "customType",
 *     "properties" : [
 *         "green" : "lantern",
 *         "captain" : "america",
 *         "black" : "widow"
 *     ]
 * }
 * </pre>
 */
public final class RestTranslator extends KomodoRestEntity {

    /**
     * And empty array of translators.
     */
    public static final RestTranslator[] NO_TRANSLATORS = new RestTranslator[ 0 ];

    @SerializedName( "id" )
    private String name;
    private String description;
    private String type;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestTranslator() {
        // nothing to do
    }

    /**
     * @param name
     *        the name of the translator (cannot be empty)
     * @param type
     *        the translator type (cannot be empty)
     */
    public RestTranslator( final String name,
                           final String type ) {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$
        this.name = name;
        this.type = type;
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

        final RestTranslator that = ( RestTranslator )other;

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
        if ( this.type == null ) {
            if ( that.type != null ) {
                return false;
            }
        } else if ( !this.type.equals( that.type ) ) {
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
     * @return the translator type (can be empty)
     */
    public String getType() {
        return this.type;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.name, this.description, this.type, super.hashCode() );
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
     *        the VDB name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @param newType
     *        the new translator type (can be empty)
     */
    public void setType( final String newType ) {
        this.type = newType;
    }

}
