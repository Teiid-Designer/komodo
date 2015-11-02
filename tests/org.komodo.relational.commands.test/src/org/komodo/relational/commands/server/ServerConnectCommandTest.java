/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.relational.commands.server;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test ServerConnectCommand
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class ServerConnectCommandTest extends AbstractCommandTest {

    @Test
    public void shouldFailNoLocalhostFound() throws Exception {
        final String[] commands = { 
            "set-auto-commit false",
            "workspace",
            "create-teiid myTeiid",
            "commit",
            "set-server myTeiid",
            "server-connect" };

        setup( commands );

        CommandResult result = execute();
        String msg = result.getMessage();

        assertTrue(msg.contains("localhost is not available"));
    }

}
