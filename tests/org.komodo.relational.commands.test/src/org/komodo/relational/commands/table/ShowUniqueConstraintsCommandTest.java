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
 * Test class for {@link ShowUniqueConstraintsCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowUniqueConstraintsCommandTest extends AbstractCommandTest {

    private static final String CONSTRAINT_1 = "my_constraint";
    private static final String CONSTRAINT_2 = "your_constraint";
    private static final String CONSTRAINT_3 = "gabbie_girl";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "add-table myTable",
                                    "cd myTable",
                                    "add-unique-constraint " + CONSTRAINT_1,
                                    "add-unique-constraint " + CONSTRAINT_2,
                                    "add-unique-constraint " + CONSTRAINT_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowUniqueConstraintsCommand.NAME + SPACE + "*girl" + SPACE + "*our*" + SPACE + "blah" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( CONSTRAINT_1 ), is( false ) );
        assertThat( output, output.contains( CONSTRAINT_2 ), is( true ) );
        assertThat( output, output.contains( CONSTRAINT_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayUniqueConstraints() throws Exception {
        final String[] commands = { ShowUniqueConstraintsCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( CONSTRAINT_1 ), is( true ) );
        assertThat( output, output.contains( CONSTRAINT_2 ), is( true ) );
        assertThat( output, output.contains( CONSTRAINT_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayUniqueConstraintsThatMatchPattern() throws Exception {
        final String[] commands = { ShowUniqueConstraintsCommand.NAME + " *bb*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( CONSTRAINT_1 ), is( false ) );
        assertThat( output, output.contains( CONSTRAINT_2 ), is( false ) );
        assertThat( output, output.contains( CONSTRAINT_3 ), is( true ) );
    }

    @Test
    public void shouldNotMatchPattern() throws Exception {
        final String[] commands = { ShowUniqueConstraintsCommand.NAME + " *blah*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( CONSTRAINT_1 ), is( false ) );
        assertThat( output, output.contains( CONSTRAINT_2 ), is( false ) );
        assertThat( output, output.contains( CONSTRAINT_3 ), is( false ) );
    }

}
