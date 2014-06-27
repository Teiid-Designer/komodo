/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.teiid.query.sql.v87;

import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.sql.v85.Test85SQLStringVisitor;

/**
 *
 */
@SuppressWarnings( {"javadoc"} )
public class Test87SQLStringVisitor extends Test85SQLStringVisitor {

    protected Test87SQLStringVisitor(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test87SQLStringVisitor() {
        this(Version.TEIID_8_7.get());
    }

}
