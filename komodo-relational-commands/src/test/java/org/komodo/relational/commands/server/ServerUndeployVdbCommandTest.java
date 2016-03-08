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
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * Test Class to test {@link ServerUndeployVdbCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerUndeployVdbCommandTest extends AbstractServerCommandTest {

    @Test
    @Ignore
    public void shouldNotBeAvailableForServerNotConnected() throws Exception {
        // Initialize a disconnected server
        initServer("myTeiid", false, 
                   new TeiidVdb[]{VDB1}, new TeiidDataSource[]{DS1}, 
                   new TeiidTranslator[]{TRANSLATOR1}, new String[]{DS_TYPE1});
        
        this.assertCommandsNotAvailable(ServerUndeployVdbCommand.NAME);
    }

    @Test
    @Ignore
    public void shouldUndeployVdb() throws Exception {
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
        
        
        result = execute(new String[]{"server-undeploy-vdb VDB1"});
        
        final String output = getCommandOutput();
        assertThat( output, output.contains( "undeployed successfully" ), is( true ) );
    }

}
