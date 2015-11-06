/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdbDataRole;
import org.komodo.utils.StringUtils;

/**
 * A GSON serializer/deserializer for {@link RestVdbDataRole}s.
 */
public final class VdbDataRoleSerializer extends BasicEntitySerializer<RestVdbDataRole> {

    @Override
    protected boolean isComplete( final RestVdbDataRole dataRole ) {
        return super.isComplete(dataRole) && !StringUtils.isBlank( dataRole.getName() );
    }

    @Override
    protected RestVdbDataRole createEntity() {
        return new RestVdbDataRole();
    }
}
