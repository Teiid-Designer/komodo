/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdbMask;
import org.komodo.utils.StringUtils;

/**
 * A GSON serializer/deserializer for {@link RestVdbMask}s.
 */
public final class VdbMaskSerializer extends BasicEntitySerializer< RestVdbMask > {

    @Override
    protected boolean isComplete( final RestVdbMask condition ) {
        return super.isComplete(condition) && !StringUtils.isBlank( condition.getName() );
    }

    @Override
    protected RestVdbMask createEntity() {
        return new RestVdbMask();
    }
}
