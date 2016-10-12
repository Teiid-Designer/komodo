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
package org.komodo.relational.commands.workspace;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link CreateVdbCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class CreateVdbCommandTest extends AbstractCommandTest {

    @Test
    public void shouldCreateVdbWithoutOptionalPath() throws Exception {
        final String[] commands = { "create-vdb testVdb" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);
        assertEquals("testVdb", vdbs[0].getName(getTransaction()));
    }

    @Test
    public void shouldCreateVdbWithOptionalPath() throws Exception {
        final String[] commands = { "create-vdb testVdb vdbPath" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);
        assertEquals("testVdb", vdbs[0].getName(getTransaction()));
    }

    @Test
    public void shouldCreatePartsVdbFromBatchFile() throws Exception {
        setup( "PartsVDBScript.txt" );
        final CommandResult result = execute();

        assertCommandResultOk( result );

        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = mgr.findVdbs( getTransaction() );
        assertThat( vdbs[ 0 ].getName( getTransaction() ), is( "PartsVDB" ) );

        final Vdb partsVdb = vdbs[ 0 ];
        assertThat( partsVdb.getTranslators( getTransaction() ).length, is( 1 ) );
        assertThat( partsVdb.getTranslators( getTransaction() )[ 0 ].getName( getTransaction() ), is( "custom_oracle" ) );
    }

}
