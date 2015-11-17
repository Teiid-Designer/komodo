/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.server;

import org.junit.Test;

/**
 * Test class for {@link ServerCommandsI18n}.
 */
@SuppressWarnings("javadoc")
public final class ServerCommandsI18nTest {

    @Test
    public void shouldNotHaveErrors() throws Exception {
        ServerCommandsI18n.attemptingToConnect.length();
    }

}
