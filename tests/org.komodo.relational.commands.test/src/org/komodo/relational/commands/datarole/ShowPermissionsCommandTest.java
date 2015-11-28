/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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
