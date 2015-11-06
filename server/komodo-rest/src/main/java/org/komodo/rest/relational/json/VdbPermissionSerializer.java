/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdbPermission;
import org.komodo.utils.StringUtils;

/**
 * A GSON serializer/deserializer for {@link RestVdbPermission}s.
 */
public final class VdbPermissionSerializer extends BasicEntitySerializer< RestVdbPermission > {

    @Override
    protected boolean isComplete( final RestVdbPermission permission ) {
        return super.isComplete(permission) && !StringUtils.isBlank( permission.getName() );
    }

    @Override
    protected RestVdbPermission createEntity() {
        return new RestVdbPermission();
    }
}
