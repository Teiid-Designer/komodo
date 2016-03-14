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

import java.util.Arrays;
import java.util.Collection;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;

/**
 * Test Class to test {@link ServerConnectCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerConnectCommandTest extends AbstractServerCommandTest {

    @Test
    public void shouldBeAvailable() throws Exception {
        final Collection< String > available = Arrays.asList( this.wsStatus.getAvailableCommandNames() );
        if ( !available.contains( ServerConnectCommand.NAME ) ) {
            Assert.fail( "Command " + name + " should be available" );
        }
    }
    
    @Test
    @Ignore
    public void shouldConnect() throws Exception {
        ShellCommand command = wsStatus.getCommand("server-connect");
        CommandResult result = command.execute();
        assertCommandResultOk(result);
    }

}
