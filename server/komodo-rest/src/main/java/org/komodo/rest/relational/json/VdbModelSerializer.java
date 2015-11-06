/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbModel;

/**
 * A GSON serializer/deserializer for {@link RestVdb}s.
 */
public final class VdbModelSerializer extends BasicEntitySerializer<RestVdbModel> {

    @Override
    protected boolean isComplete(final RestVdbModel model) {
        return super.isComplete(model) && model.getModelType() != null;
    }

    @Override
    protected RestVdbModel createEntity() {
        return new RestVdbModel();
    }
}
