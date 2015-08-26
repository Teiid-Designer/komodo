/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.komodo.rest.KomodoRestUriBuilder;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.utils.ArgCheck;
import com.google.gson.annotations.SerializedName;

/**
 * A VDB descriptor that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * {
 *     "id" : "MyVdb",
 *     "description" : "vdb description goes here",
 *     "links" : [
 *         { "rel" : "self", "href" : "http://<baseUri>/komodo/workspace/vdbs/MyVdb" },
 *         { "rel" : "parent", "href" : "http://<baseUri>/komodo/workspace/vdbs" },
 *         { "rel" : "delete", "href" : "http://<baseUri>/komodo/workspace/vdbs/MyVdb" },
 *         { "rel" : "manifest", "href" : "http://<baseUri>/komodo/workspace/vdbs/MyVdb/manifest" }
 *     ]
 * }
 * </pre>
 */
public final class RestVdbDescriptor implements Jsonable {

    /**
     * An empty array of descriptors.
     */
    public static final RestVdbDescriptor[] NO_DESCRIPTORS = new RestVdbDescriptor[ 0 ];

    @SerializedName( "id" )
    private final String name;
    private String description;
    private final RestLink[] links;

    /**
     * @param vdbName
     *        the VDB name (cannot be empty)
     * @param baseUri
     *        the base URI (cannot be <code>null</code>)
     * @param linkTypes
     *        the types of links to create (cannot be <code>null</code> or empty)
     */
    public RestVdbDescriptor( final String vdbName,
                              final URI baseUri,
                              final LinkType... linkTypes ) {
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        ArgCheck.isNotNull( baseUri, "baseUri" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( linkTypes, "linkTypes" ); //$NON-NLS-1$

        this.name = vdbName;

        // create links (make sure no duplicates)
        final KomodoRestUriBuilder uriBuilder = new KomodoRestUriBuilder( baseUri );
        final List< RestLink > temp = new ArrayList< >();
        final Set< LinkType > types = new HashSet< >( linkTypes.length );

        for ( final LinkType linkType : linkTypes ) {
            if ( types.add( linkType ) ) {
                temp.add( new RestLink( linkType, uriBuilder.buildVdbUri( linkType, vdbName ) ) );
            }
        }

        this.links = temp.toArray( new RestLink[ temp.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object other ) {
        if ( ( other == null ) || !getClass().equals( other.getClass() ) ) {
            return false;
        }

        final RestVdbDescriptor that = ( RestVdbDescriptor )other;

        if ( this.name.equals( that.name ) ) {
            if ( this.description == null ) {
                if ( that.description != null ) {
                    return false;
                }
            } else if ( !this.description.equals( that.description ) ) {
                return false;
            }

            return Arrays.deepEquals( this.links, that.links );
        }

        // names not equal
        return false;
    }

    /**
     * @return the description (can be empty)
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @return the links (never <code>null</code> or empty)
     */
    public RestLink[] getLinks() {
        return this.links;
    }

    /**
     * @return the name (never empty)
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
        return Objects.hash( this.name, this.description, Arrays.deepHashCode( this.links ) );
    }

    /**
     * @param description
     *        the new description (can be empty)
     */
    public void setDescription( final String description ) {
        this.description = description;
    }

}
