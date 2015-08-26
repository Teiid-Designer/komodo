/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import org.komodo.utils.ArgCheck;
import com.google.gson.annotations.SerializedName;

/**
 * A VDB that can be used by GSON to build a JSON document representation.
 */
public final class RestVdb implements Jsonable {

    @SerializedName( "id" )
    private String name;
    private String description;
    private String originalFilePath;
    private RestLink[] links;

    /**
     * @param vdbName
     *        the name of the VDB (cannot be empty)
     */
    public RestVdb( final String vdbName ) {
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        this.name = vdbName;
    }

    /**
     * @return the VDB description (can be empty)
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @return the links (never <code>null</code> but can be empty)
     */
    public RestLink[] getLinks() {
        if ( this.links == null ) {
            return RestLink.NO_LINKS;
        }

        return this.links;
    }

    /**
     * @return the VDB name (never empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the external file path of the VDB (can be empty)
     */
    public String getOriginalFilePath() {
        return this.originalFilePath;
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        this.description = newDescription;
    }

    /**
     * @param newLinks
     *        the new links (can be <code>null</code> or empty)
     */
    public void setLinks( final RestLink[] newLinks ) {
        this.links = newLinks;
    }

    /**
     * @param newOriginalFilePath
     *        the new VDB external file path (can be empty)
     */
    public void setOriginalFilePath( final String newOriginalFilePath ) {
        this.originalFilePath = newOriginalFilePath;
    }

}
