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
package org.komodo.relational.commands.table;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test class for {@link ShowColumnsCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowColumnsCommandTest extends AbstractCommandTest {

    private static final String COLUMN_1 = "my_column";
    private static final String COLUMN_2 = "your_column";
    private static final String COLUMN_3 = "column_three";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "add-table myTable",
                                    "cd myTable",
                                    "add-column " + COLUMN_1,
                                    "add-column " + COLUMN_2,
                                    "add-column " + COLUMN_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowColumnsCommand.NAME + SPACE + COLUMN_3 + SPACE + "*column*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( COLUMN_1 ), is( true ) );
        assertThat( output, output.contains( COLUMN_2 ), is( true ) );
        assertThat( output, output.contains( COLUMN_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayColumns() throws Exception {
        final String[] commands = { ShowColumnsCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( COLUMN_1 ), is( true ) );
        assertThat( output, output.contains( COLUMN_2 ), is( true ) );
        assertThat( output, output.contains( COLUMN_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayColumnsThatMatchPattern() throws Exception {
        final String[] commands = { ShowColumnsCommand.NAME + " *column" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( COLUMN_1 ), is( true ) );
        assertThat( output, output.contains( COLUMN_2 ), is( true ) );
        assertThat( output, output.contains( COLUMN_3 ), is( false ) );
    }

    @Test
    public void shouldNotMatchPattern() throws Exception {
        final String[] commands = { ShowColumnsCommand.NAME + " *blah" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( COLUMN_1 ), is( false ) );
        assertThat( output, output.contains( COLUMN_2 ), is( false ) );
        assertThat( output, output.contains( COLUMN_3 ), is( false ) );
    }

}
