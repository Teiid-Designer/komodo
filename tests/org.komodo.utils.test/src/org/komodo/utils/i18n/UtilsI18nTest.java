/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.utils.i18n;

import org.junit.Test;

/**
 * Test class for {@link UtilsI18n}.
 */
@SuppressWarnings("javadoc")
public final class UtilsI18nTest {

    @Test
    public void shouldNotHaveErrors() throws Exception {
        UtilsI18n.missingI18Field.length();
    }

}
