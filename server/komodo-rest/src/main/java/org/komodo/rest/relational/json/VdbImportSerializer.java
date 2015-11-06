/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import org.komodo.rest.relational.RestVdbImport;
import org.komodo.utils.StringUtils;

/**
 * A GSON serializer/deserializer for {@link RestVdbImport}s.
 */
public final class VdbImportSerializer extends BasicEntitySerializer< RestVdbImport > {

    @Override
    protected boolean isComplete( final RestVdbImport vdbImport ) {
        return super.isComplete(vdbImport) && !StringUtils.isBlank( vdbImport.getName() );
    }

    @Override
    protected RestVdbImport createEntity() {
        return new RestVdbImport();
    }
}
