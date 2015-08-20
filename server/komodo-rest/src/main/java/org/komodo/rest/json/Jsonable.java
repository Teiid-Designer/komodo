/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * Indicates the objects has a JSON representation.
 */
public interface Jsonable {

    /**
     * @return a JSON string representation (never empty)
     */
    default public String toJson() {
        final Gson gson = new GsonBuilder().create();
        return gson.toJson( this );
    }

}
