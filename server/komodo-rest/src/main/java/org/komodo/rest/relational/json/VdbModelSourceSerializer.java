/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbModelSource;

/**
 * A GSON serializer/deserializer for {@link RestVdb}s.
 */
public final class VdbModelSourceSerializer extends BasicEntitySerializer<RestVdbModelSource> {

    @Override
    protected RestVdbModelSource createEntity() {
        return new RestVdbModelSource();
    }
}
