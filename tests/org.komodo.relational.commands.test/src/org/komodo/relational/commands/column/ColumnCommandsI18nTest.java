/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.column;

import org.junit.Test;

/**
 * Test class for {@link ColumnCommandsI18n}.
 */
@SuppressWarnings("javadoc")
public final class ColumnCommandsI18nTest {

    @Test
    public void shouldNotHaveErrors() throws Exception {
        ColumnCommandsI18n.invalidSearchablePropertyValue.length();
    }

}
