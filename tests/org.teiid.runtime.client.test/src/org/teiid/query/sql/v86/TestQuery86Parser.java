/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.teiid.query.sql.v86;

import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.sql.v85.TestQuery85Parser;

/**
 *
 */
@SuppressWarnings( {"javadoc"} )
public class TestQuery86Parser extends TestQuery85Parser {

    protected TestQuery86Parser(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public TestQuery86Parser() {
        this(Version.TEIID_8_6.get());
    }

}