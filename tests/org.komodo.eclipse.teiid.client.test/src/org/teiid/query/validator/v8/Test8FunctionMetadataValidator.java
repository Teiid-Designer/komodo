/*
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
*/
package org.teiid.query.validator.v8;

import org.junit.Test;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.sql.AbstractTestFactory;
import org.teiid.query.sql.v8.Test8Factory;
import org.teiid.query.validator.AbstractTestFunctionMetadataValidator;

/**
 *
 */
@SuppressWarnings( "javadoc" )
public class Test8FunctionMetadataValidator extends AbstractTestFunctionMetadataValidator {

    private Test8Factory factory;

    protected Test8FunctionMetadataValidator(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test8FunctionMetadataValidator() {
        this(Version.TEIID_8_0.get());
    }

    @Override
    protected AbstractTestFactory getFactory() {
        if (factory == null)
            factory = new Test8Factory(getQueryParser());

        return factory;
    }

    @Test
    public void testValidateNameFail3() {
        helpTestValidateName("a.b"); //$NON-NLS-1$
    }
}
