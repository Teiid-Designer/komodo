/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import com.google.gson.annotations.SerializedName;

/**
 *
 */
public class RestVdb implements Jsonable {

    private String description;

    private RestLink[] links;
    @SerializedName( "id" )
    private String name;
    private String originalFilePath;

    public RestVdb() {

    }

    /**
     *
     */
    public RestVdb( final String vdbName ) {
        this.name = vdbName;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @return the links
     */
    public RestLink[] getLinks() {
        return this.links;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the originalFilePath
     */
    public String getOriginalFilePath() {
        return this.originalFilePath;
    }

    /**
     * @param description
     *        the description to set
     */
    public void setDescription( final String description ) {
        this.description = description;
    }

    /**
     * @param links
     *        the links to set
     */
    public void setLinks( final RestLink[] links ) {
        this.links = links;
    }

    /**
     * @param originalFilePath
     *        the originalFilePath to set
     */
    public void setOriginalFilePath( final String originalFilePath ) {
        this.originalFilePath = originalFilePath;
    }

}
