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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * Test Class to test {@link ServerDisconnectCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerDisconnectCommandTest extends AbstractServerCommandTest {

    @Test
    @Ignore
    public void shouldNotBeAvailableForServerNotConnected() throws Exception {
        // Initialize a disconnected server
        initServer("myTeiid", false, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        this.assertCommandsNotAvailable(ServerDisconnectCommand.NAME);
    }
    
    @Test
    @Ignore
    public void shouldFailNoServerConnected() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        assertCommandResultOk(result);

        // Initialize a mock server (not connected) with no artifacts
        initServer("myTeiid", false, null, null, null, null);
        
        // Results in 'command not found' - not enabled if server is not connected
        ShellCommand command = wsStatus.getCommand("server-disconnect");
        result = command.execute();
        String output = result.getMessage();
        assertThat( output, output.contains( ServerDisconnectCommand.NAME ), is( true ) );
        assertThat( output, output.contains( "not found" ), is( true ) );
    }
    
    @Test
    @Ignore
    public void shouldDisconnectServer() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        assertCommandResultOk(result);

        // Initialize a mock server (connected) with no artifacts
        initServer("myTeiid", true, null, null, null, null);
        
        // Disconnect the connected server
        ShellCommand command = wsStatus.getCommand("server-disconnect");
        result = command.execute();
        assertCommandResultOk(result);
        String output = result.getMessage();
        assertThat( output, output.contains( "myTeiid" ), is( true ) );
        assertThat( output, output.contains( "disconnected" ), is( true ) );
    }
}
