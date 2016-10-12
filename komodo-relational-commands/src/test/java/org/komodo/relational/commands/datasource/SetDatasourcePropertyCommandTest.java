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
package org.komodo.relational.commands.datasource;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetDatasourcePropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class SetDatasourcePropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "create-datasource testSource",
            "cd testSource",
            "set-property jndiName java:/testSource" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Datasource[] sources = wkspMgr.findDatasources(getTransaction());

        assertEquals(1, sources.length);
        assertEquals("testSource", sources[0].getName(getTransaction())); //$NON-NLS-1$

        assertEquals("java:/testSource", sources[0].getJndiName(getTransaction()));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String description = "blah";
        final String[] commands = { "create-datasource testSource",
                                    "cd testSource",
                                    "set-property description " + description };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Datasource[] sources = wkspMgr.findDatasources( getTransaction() );

        assertThat( sources.length, is( 1 ) );
        assertThat( sources[ 0 ].getDescription( getTransaction() ), is( description ) );
    }

    @Test
    public void shouldSetExternalLocation() throws Exception {
        final String extLoc = "/Users/sledge/blah";
        final String[] commands = { "create-datasource testSource",
                                    "cd testSource",
                                    "set-property externalLocation " + extLoc };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Datasource[] sources = wkspMgr.findDatasources( getTransaction() );

        assertThat( sources.length, is( 1 ) );
        assertThat( sources[ 0 ].getExternalLocation( getTransaction() ), is( extLoc ) );
    }

}
