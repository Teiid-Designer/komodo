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

/**
 * A translator that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
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
 * </code>
 * </pre>
 */
public final class RestVdbTranslator extends KomodoRestEntity {

    /**
     * An empty array of translators.
     */
    public static final RestVdbTranslator[] NO_TRANSLATORS = new RestVdbTranslator[ 0 ];

    private String name;
    private String description;
    private String type;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbTranslator() {
        // nothing to do
    }

    /**
     * @param name
     *        the name of the translator (cannot be empty)
     * @param type
     *        the translator type (cannot be empty)
     */
    public RestVdbTranslator( final String name,
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

        final RestVdbTranslator that = ( RestVdbTranslator )other;

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
     *        the new translator name (can be empty)
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

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append( "translator name = " ).append( this.name ); //$NON-NLS-1$
        builder.append( ", " ); //$NON-NLS-1$
        builder.append( "type = " ).append( this.type ); //$NON-NLS-1$

        return builder.toString();
    }

}
