/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.teiid.query.validator.v86;

import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.validator.v85.Test85UpdateValidator;

/**
 *
 */
@SuppressWarnings( "javadoc" )
public class Test86UpdateValidator extends Test85UpdateValidator {

    protected Test86UpdateValidator(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test86UpdateValidator() {
        this(Version.TEIID_8_6.get());
    }

}
