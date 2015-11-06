/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdb;
import org.komodo.utils.StringUtils;

/**
 * A GSON serializer/deserializer for {@link RestVdb}s.
 */
public final class VdbSerializer extends BasicEntitySerializer<RestVdb> {

    @Override
    protected boolean isComplete(final RestVdb vdb) {
        return super.isComplete(vdb) && !StringUtils.isBlank(vdb.getName());
    }

    @Override
    protected RestVdb createEntity() {
        return new RestVdb();
    }
}
