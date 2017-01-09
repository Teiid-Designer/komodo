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
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * Test Class to test {@link ServerDeployVdbCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerDeployVdbCommandTest extends AbstractServerCommandTest {

    @Test
    @Ignore
    public void shouldNotBeAvailableForServerNotConnected() throws Exception {
        // Initialize a disconnected server
        initServer("myTeiid", false, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        this.assertCommandsNotAvailable(ServerDeployVdbCommand.NAME);
    }

    @Test
    @Ignore
    public void shouldNotDeployMissingWorkspaceVDB() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-vdb VDB1",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Attempt to deploy the VDB - fails because workspace VDB does not exist (only VDB1 exists)
        ShellCommand command = wsStatus.getCommand("server-deploy-vdb");
        command.setArguments(new Arguments("myVdb"));
        result = command.execute();
        assertThat(result.isOk(), is( false ) );
        String output = result.getMessage();
        assertThat( output, output.contains( "myVdb" ), is( true ) );
        assertThat( output, output.contains( "not found" ), is( true ) );
    }

    @Test
    @Ignore
    public void shouldNotDeployVDBExistsOnServer() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-vdb VDB1",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Attempt to deploy workspace VDB1 - this fails because same VDB is on the server (and no overwrite arg).
        ShellCommand command = wsStatus.getCommand("server-deploy-vdb");
        command.setArguments(new Arguments("VDB1"));
        result = command.execute();
        assertThat(result.isOk(), is( false ) );
        String output = result.getMessage();
        assertThat( output, output.contains( "VDB1" ), is( true ) );
        assertThat( output, output.contains( "already exists" ), is( true ) );
    }
    
    @Test
    @Ignore
    public void shouldDeployVdb() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-vdb myVdb",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Deploys myVdb from workspace - there is no conflicting vdb on the server.
        result = execute(new String[]{"server-deploy-vdb myVdb"});
        
        assertCommandResultOk( result );
        final String output = getCommandOutput();
        assertThat( output, output.contains( "deployed successfully" ), is( true ) );
    }
    
    @Test
    @Ignore
    public void shouldDeployExistingVDBWithOverwrite() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-vdb VDB1",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Deploy VDB1 from workspace - there is a conflicting server VDB, but overwrite arg is supplied.
        ShellCommand command = wsStatus.getCommand("server-deploy-vdb");
        command.setArguments(new Arguments("VDB1 -o"));
        result = command.execute();
        
        assertCommandResultOk( result );
    }

    @Test
    @Ignore
    public void shouldNotDeployVDBServerMissingSources() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-vdb VDB1",
            "cd VDB1",
            "add-model myModel",
            "cd myModel",
            "add-source modelSource",
            "cd modelSource",
            "set-property sourceJndiName java:/modelSource",
            "commit",
            "set-server myTeiid",
            "workspace" };
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Try to deploy the VDB - fails because workspace VDB does not exist.
        ShellCommand command = wsStatus.getCommand("server-deploy-vdb");
        command.setArguments(new Arguments("VDB1 -o"));
        result = command.execute();
        
        assertThat(result.isOk(), is( false ) );
        String output = result.getMessage();
        assertThat( output, output.contains( "server does not have source" ), is( true ) );
        assertThat( output, output.contains( "java:/modelSource" ), is( true ) );
    }

}
