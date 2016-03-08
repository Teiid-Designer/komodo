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
 * Test Class to test {@link ServerDeployDatasourceCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerDeployDatasourceCommandTest extends AbstractServerCommandTest {

    @Test
    @Ignore
    public void shouldNotBeAvailableForServerNotConnected() throws Exception {
        // Initialize a disconnected server
        initServer("myTeiid", false, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        this.assertCommandsNotAvailable(ServerDeployDatasourceCommand.NAME);
    }

    @Test
    @Ignore
    public void shouldNotDeployMissingWorkspaceDatasource() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-datasource DS1",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Attempt to deploy the Datasource - fails because workspace Datasource does not exist (only DS1 exists)
        ShellCommand command = wsStatus.getCommand("server-deploy-datasource");
        command.setArguments(new Arguments("myDs"));
        result = command.execute();
        assertThat(result.isOk(), is( false ) );
        String output = result.getMessage();
        assertThat( output, output.contains( "myDs" ), is( true ) );
        assertThat( output, output.contains( "not found" ), is( true ) );
    }

    @Test
    @Ignore
    public void shouldNotDeployDatasourceExistsOnServer() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-datasource DS1",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Attempt to deploy workspace DS1 - this fails because same Datasource is on the server (and no overwrite arg).
        ShellCommand command = wsStatus.getCommand("server-deploy-datasource");
        command.setArguments(new Arguments("DS1"));
        result = command.execute();
        assertThat(result.isOk(), is( false ) );
        String output = result.getMessage();
        assertThat( output, output.contains( "DS1" ), is( true ) );
        assertThat( output, output.contains( "already exists" ), is( true ) );
    }
    
    @Test
    @Ignore
    public void shouldDeployDatasource() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-datasource myDs",
            "cd myDs",
            "set-property driverName DS_TYPE1",
            "workspace",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Deploys myDs from workspace - there is no conflicting DataSource on the server.
        result = execute(new String[]{"server-deploy-datasource myDs"});
        
        assertCommandResultOk( result );
        final String output = getCommandOutput();
        assertThat( output, output.contains( "deployed successfully" ), is( true ) );
    }
    
    @Test
    @Ignore
    public void shouldDeployExistingDatasourceWithOverwrite() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "create-datasource DS1",
            "cd DS1",
            "set-property driverName DS_TYPE1",
            "workspace",
            "commit",
            "set-server myTeiid"};
        CommandResult result = execute( commands );
        
        assertCommandResultOk(result);
        
        // Initialize mock server with artifacts
        initServer("myTeiid", true, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        
        // Deploy DS1 from workspace - there is a conflicting server DataSource, but overwrite arg is supplied.
        ShellCommand command = wsStatus.getCommand("server-deploy-datasource");
        command.setArguments(new Arguments("DS1 -o"));
        result = command.execute();
        
        assertCommandResultOk( result );
    }

}
