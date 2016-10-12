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
package org.komodo.relational.commands.permission;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetPermissionPropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class SetPermissionPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-data-role myDataRole",
            "cd myDataRole",
            "add-permission myPermission",
            "cd myPermission",
            "set-property allowAlter true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());
        assertEquals(1, vdbs.length);

        DataRole[] dataRoles = vdbs[0].getDataRoles(getTransaction());
        assertEquals(1, dataRoles.length);

        Permission[] permissions = dataRoles[0].getPermissions(getTransaction());
        assertEquals(1, permissions.length);
        assertEquals("myPermission", permissions[0].getName(getTransaction())); //$NON-NLS-1$

        assertEquals(true, permissions[0].isAllowAlter(getTransaction()));
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addPermissions.cmd");
    	final String[] commands = { "cd myPermission1" };
    	final CommandResult result = execute( commands );
        assertCommandResultOk(result);

    	candidates.add(PermissionShellCommand.ALLOW_CREATE);
    	assertTabCompletion("set-property allowCre", candidates);
    	assertTabCompletion("set-property allowcre", candidates);
    }

    @Test
    public void shouldSetAllowCreate() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "set-property allowCreate true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final DataRole[] dataRoles = vdbs[ 0 ].getDataRoles( getTransaction() );
        assertThat( dataRoles.length, is( 1 ) );

        final Permission[] permissions = dataRoles[ 0 ].getPermissions( getTransaction() );
        assertThat( permissions.length, is( 1 ) );
        assertThat( permissions[ 0 ].isAllowCreate( getTransaction() ), is( true ) );
    }

    @Test
    public void shouldSetAllowDelete() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "set-property allowDelete true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final DataRole[] dataRoles = vdbs[ 0 ].getDataRoles( getTransaction() );
        assertThat( dataRoles.length, is( 1 ) );

        final Permission[] permissions = dataRoles[ 0 ].getPermissions( getTransaction() );
        assertThat( permissions.length, is( 1 ) );
        assertThat( permissions[ 0 ].isAllowDelete( getTransaction() ), is( true ) );
    }

    @Test
    public void shouldSetAllowExecute() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "set-property allowExecute true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final DataRole[] dataRoles = vdbs[ 0 ].getDataRoles( getTransaction() );
        assertThat( dataRoles.length, is( 1 ) );

        final Permission[] permissions = dataRoles[ 0 ].getPermissions( getTransaction() );
        assertThat( permissions.length, is( 1 ) );
        assertThat( permissions[ 0 ].isAllowExecute( getTransaction() ), is( true ) );
    }

    @Test
    public void shouldSetAllowLanguage() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "set-property allowLanguage true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final DataRole[] dataRoles = vdbs[ 0 ].getDataRoles( getTransaction() );
        assertThat( dataRoles.length, is( 1 ) );

        final Permission[] permissions = dataRoles[ 0 ].getPermissions( getTransaction() );
        assertThat( permissions.length, is( 1 ) );
        assertThat( permissions[ 0 ].isAllowLanguage( getTransaction() ), is( true ) );
    }

    @Test
    public void shouldSetAllowRead() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "set-property allowRead true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final DataRole[] dataRoles = vdbs[ 0 ].getDataRoles( getTransaction() );
        assertThat( dataRoles.length, is( 1 ) );

        final Permission[] permissions = dataRoles[ 0 ].getPermissions( getTransaction() );
        assertThat( permissions.length, is( 1 ) );
        assertThat( permissions[ 0 ].isAllowRead( getTransaction() ), is( true ) );
    }

    @Test
    public void shouldSetAllowUpdate() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "set-property allowUpdate true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final DataRole[] dataRoles = vdbs[ 0 ].getDataRoles( getTransaction() );
        assertThat( dataRoles.length, is( 1 ) );

        final Permission[] permissions = dataRoles[ 0 ].getPermissions( getTransaction() );
        assertThat( permissions.length, is( 1 ) );
        assertThat( permissions[ 0 ].isAllowUpdate( getTransaction() ), is( true ) );
    }

}
