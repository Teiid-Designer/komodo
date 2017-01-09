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
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link ServerShowPropertiesCommand}.
 */
@SuppressWarnings({"javadoc", "nls"})
public class ServerShowPropertiesCommandTest extends AbstractServerCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "show-server-properties extraArg" };
        execute( commands );
    }

    @Test
    public void shouldBeAvailableAtLibrary() throws Exception {
        final String[] commands = { "library" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsAvailable( ServerShowPropertiesCommand.NAME );
    }

    @Test
    public void shouldBeAvailableAtRoot() throws Exception {
        assertCommandsAvailable( ServerShowPropertiesCommand.NAME );
    }

    @Test
    public void shouldBeAvailableAtWorkspace() throws Exception {
        final String[] commands = { "workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertCommandsAvailable( ServerShowPropertiesCommand.NAME );
    }

    @Test
    public void shouldShowProperties() throws Exception {
        final String[] commands = {
            "server-show-properties" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains("adminUser"));
        assertTrue(writerOutput.contains("jdbcPort"));
    }

}
