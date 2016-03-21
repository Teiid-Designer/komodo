/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.commands.datarole;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test class for the {@link ShowPermissionsCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowPermissionsCommandTest extends AbstractCommandTest {

    private static final String PERMISSION_1 = "my_permission";
    private static final String PERMISSION_2 = "your_permission";
    private static final String PERMISSION_3 = "messi";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission " + PERMISSION_1,
                                    "add-permission " + PERMISSION_2,
                                    "add-permission " + PERMISSION_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shoudAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowPermissionsCommand.NAME + SPACE + "mes*" + SPACE + PERMISSION_2 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PERMISSION_1 ), is( false ) );
        assertThat( output, output.contains( PERMISSION_2 ), is( true ) );
        assertThat( output, output.contains( PERMISSION_3 ), is( true ) );
    }

    @Test
    public void shoudDisplayPermissions() throws Exception {
        final String[] commands = { ShowPermissionsCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PERMISSION_1 ), is( true ) );
        assertThat( output, output.contains( PERMISSION_2 ), is( true ) );
        assertThat( output, output.contains( PERMISSION_3 ), is( true ) );
    }

    @Test
    public void shoudDisplayPermissionsThatMatchPattern() throws Exception {
        final String[] commands = { ShowPermissionsCommand.NAME + " my_*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PERMISSION_1 ), is( true ) );
        assertThat( output, output.contains( PERMISSION_2 ), is( false ) );
        assertThat( output, output.contains( PERMISSION_3 ), is( false ) );
    }

    @Test
    public void shoudNotMatchPattern() throws Exception {
        final String[] commands = { ShowPermissionsCommand.NAME + " blah" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PERMISSION_1 ), is( false ) );
        assertThat( output, output.contains( PERMISSION_2 ), is( false ) );
        assertThat( output, output.contains( PERMISSION_3 ), is( false ) );
    }

}
