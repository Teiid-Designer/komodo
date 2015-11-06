/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdbCondition;
import org.komodo.utils.StringUtils;

/**
 * A GSON serializer/deserializer for {@link RestVdbCondition}s.
 */
public final class VdbConditionSerializer extends BasicEntitySerializer< RestVdbCondition > {

    @Override
    protected boolean isComplete( final RestVdbCondition condition ) {
        return super.isComplete(condition) && !StringUtils.isBlank( condition.getName() );
    }

    @Override
    protected RestVdbCondition createEntity() {
        return new RestVdbCondition();
    }
}
