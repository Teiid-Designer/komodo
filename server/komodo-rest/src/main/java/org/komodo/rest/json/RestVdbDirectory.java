/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.util.Arrays;
import com.google.common.base.Objects;

/**
 * Represents a VDB directory and can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     vdbs: {
 *         {
 *             "id" : "MyVdb",
 *             "description" : "my vdb description goes here",
 *             "links" : [
 *                 { "rel" : "self", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/MyVdb" },
 *                 { "rel" : "parent", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/" },
 *                 { "rel" : "delete", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/MyVdb" },
 *                 { "rel" : "content", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/MyVdb/manifest" }
 *             ]
 *         },
 *         {
 *             "id" : "YourVdb",
 *             "description" : "your vdb description goes here",
 *             "links" : [
 *                 { "rel" : "self", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/YourVdb" },
 *                 { "rel" : "parent", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/" },
 *                 { "rel" : "delete", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/YourVdb" },
 *                 { "rel" : "xml", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/YourVdb/manifest" }
 *              ]
 *         }
 *     }
 * }
 * </code>
 * </pre>
 */
public final class RestVdbDirectory extends KomodoRestEntity {

    private RestVdbDescriptor[] vdbs = RestVdbDescriptor.NO_DESCRIPTORS;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbDirectory() {
        // nothing to do
    }

    /**
     * @param descriptors
     *        the VDB descriptors used to create the directory (can be <code>null</code> or empty)
     */
    public RestVdbDirectory( final RestVdbDescriptor... descriptors ) {
        this.vdbs = ( ( descriptors == null ) ? RestVdbDescriptor.NO_DESCRIPTORS : descriptors );
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

        final RestVdbDirectory that = ( RestVdbDirectory )other;
        return Arrays.deepEquals( this.vdbs, that.vdbs );
    }

    /**
     * @return the descriptors (never <code>null</code> but can be empty)
     */
    public RestVdbDescriptor[] getDescriptors() {
        return this.vdbs;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hashCode( Arrays.deepHashCode( this.vdbs ), super.hashCode() );
    }

    /**
     * @param newVdbs
     *        the new VDB descriptors (can be <code>null</code>)
     */
    public void setVdbs( final RestVdbDescriptor[] newVdbs ) {
        this.vdbs = ( ( newVdbs == null ) ? RestVdbDescriptor.NO_DESCRIPTORS : newVdbs );
    }

}
