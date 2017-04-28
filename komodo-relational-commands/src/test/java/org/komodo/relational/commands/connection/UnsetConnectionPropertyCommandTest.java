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
package org.komodo.relational.commands.connection;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.commands.connection.UnsetConnectionPropertyCommand;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link UnsetConnectionPropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class UnsetConnectionPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testUnsetProperty1() throws Exception {
        final String[] commands = {
            "create-connection testSource",
            "cd testSource",
            "set-property jndiName java:/testSource",
            "unset-property jndiName" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Connection[] connections = wkspMgr.findConnections(getTransaction());

        assertEquals(1, connections.length);
        assertEquals("testSource", connections[0].getName(getTransaction())); //$NON-NLS-1$

        assertEquals(null, connections[0].getJndiName(getTransaction()));
    }

    @Test
    public void shouldUnsetDescription() throws Exception {
        final String[] commands = { "create-connection testSource",
                                    "cd testSource",
                                    "set-property description blah",
                                    "unset-property description" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Connection[] connections = wkspMgr.findConnections( getTransaction() );

        assertThat( connections.length, is( 1 ) );
        assertThat( connections[ 0 ].getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldUnsetExternalLocation() throws Exception {
        final String[] commands = { "create-connection testSource",
                                    "cd testSource",
                                    "set-property externalLocation /Users/sledge/blah",
                                    "unset-property externalLocation" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Connection[] connections = wkspMgr.findConnections( getTransaction() );

        assertThat( connections.length, is( 1 ) );
        assertThat( connections[ 0 ].getExternalLocation( getTransaction() ), is( nullValue() ) );
    }

}
