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
 * Test class for the {@link ShowMappedRolesCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowMappedRolesCommandTest extends AbstractCommandTest {

    private static final String ROLE_1 = "my_role";
    private static final String ROLE_2 = "your_role";
    private static final String ROLE_3 = "role_3";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "add-mapped-role " + ROLE_1,
                                    "add-mapped-role " + ROLE_2,
                                    "add-mapped-role " + ROLE_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shoudAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowMappedRolesCommand.NAME + SPACE + " *r_*" + SPACE + ROLE_1 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ROLE_1 ), is( true ) );
        assertThat( output, output.contains( ROLE_2 ), is( true ) );
        assertThat( output, output.contains( ROLE_3 ), is( false ) );
    }

    @Test
    public void shoudDisplayMappedRoles() throws Exception {
        final String[] commands = { ShowMappedRolesCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ROLE_1 ), is( true ) );
        assertThat( output, output.contains( ROLE_2 ), is( true ) );
        assertThat( output, output.contains( ROLE_3 ), is( true ) );
    }

    @Test
    public void shoudDisplayMappedRolesThatMatchPattern() throws Exception {
        final String[] commands = { ShowMappedRolesCommand.NAME + " *3*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ROLE_1 ), is( false ) );
        assertThat( output, output.contains( ROLE_2 ), is( false ) );
        assertThat( output, output.contains( ROLE_3 ), is( true ) );
    }

    @Test
    public void shoudNotMatchPattern() throws Exception {
        final String[] commands = { ShowMappedRolesCommand.NAME + " *blah*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ROLE_1 ), is( false ) );
        assertThat( output, output.contains( ROLE_2 ), is( false ) );
        assertThat( output, output.contains( ROLE_3 ), is( false ) );
    }

}
