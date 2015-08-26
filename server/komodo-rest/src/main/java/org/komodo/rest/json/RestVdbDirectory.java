/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.util.Arrays;

/**
 * Represents a VDB directory and can be used by GSON to build a JSON document representation.
 *
 * <pre>
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
 * </pre>
 */
public final class RestVdbDirectory implements Jsonable {

    private final RestVdbDescriptor[] vdbs;

    /**
     * @param descriptors
     *        the VDB descriptors used to create the directory (can be <code>null</code> or empty)
     */
    public RestVdbDirectory( final RestVdbDescriptor... descriptors ) {
        this.vdbs = ( ( descriptors == null ) ? RestVdbDescriptor.NO_DESCRIPTORS : descriptors );
    }

    /**
     * @return the descriptors (can be <code>null</code> or empty)
     */
    public RestVdbDescriptor[] getDescriptors() {
        return this.vdbs;
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

        final RestVdbDirectory that = ( RestVdbDirectory )other;
        return Arrays.deepEquals( this.vdbs, that.vdbs );
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Arrays.deepHashCode( this.vdbs );
    }

}
