/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.net.URI;
import org.komodo.rest.KomodoRestUtils;
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
 *         { "rel" : "content", "href" : "http://<baseUri>/komodo/workspace/vdbs/MyVdb/vdb.xml" }
 *     ]
 * }
 * </pre>
 */
public final class RestVdbDescriptor implements Jsonable {

    /**
     * An empty array of descriptors.
     */
    public static final RestVdbDescriptor[] EMTPY = new RestVdbDescriptor[ 0 ];

    @SerializedName( "id" )
    private final String name;
    private String description;
    private final RestLink[] links;

    /**
     * @param vdbName
     *        the VDB name (cannot be empty)
     * @param baseUri
     *        the base URI (can be <code>null</code>)
     */
    public RestVdbDescriptor( final String vdbName,
                              final URI baseUri ) {
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        this.name = vdbName;

        // create links
        this.links = new RestLink[] { new RestLink( LinkType.SELF, KomodoRestUtils.getVdbUri( baseUri, vdbName ) ),
                                      new RestLink( LinkType.PARENT, KomodoRestUtils.getVdbsUri( baseUri ) ),
                                      new RestLink( LinkType.CONTENT, KomodoRestUtils.getVdbContentUri( baseUri, vdbName ) ) };
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
     * @param description
     *        the new description (can be empty)
     */
    public void setDescription( final String description ) {
        this.description = description;
    }

}
