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
package org.komodo.relational.commands.model;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test class for the {@link ShowVirtualProceduresCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowVirtualProceduresCommandTest extends AbstractCommandTest {

    private static final String PROC_1 = "my_virtual_procedure";
    private static final String PROC_2 = "your_virtual_procedure";
    private static final String PROC_3 = "elvis";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "add-virtual-procedure " + PROC_1,
                                    "add-virtual-procedure " + PROC_2,
                                    "add-virtual-procedure " + PROC_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowVirtualProceduresCommand.NAME + SPACE + PROC_1 + SPACE + "*our*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PROC_1 ), is( true ) );
        assertThat( output, output.contains( PROC_2 ), is( true ) );
        assertThat( output, output.contains( PROC_3 ), is( false ) );
    }

    @Test
    public void shouldDisplayStoredProcedures() throws Exception {
        final String[] commands = { ShowVirtualProceduresCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PROC_1 ), is( true ) );
        assertThat( output, output.contains( PROC_2 ), is( true ) );
        assertThat( output, output.contains( PROC_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayStoredProceduresThatMatchPattern() throws Exception {
        final String[] commands = { ShowVirtualProceduresCommand.NAME + " elvis" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PROC_1 ), is( false ) );
        assertThat( output, output.contains( PROC_2 ), is( false ) );
        assertThat( output, output.contains( PROC_3 ), is( true ) );
    }

    @Test
    public void shouldNotMatchPattern() throws Exception {
        final String[] commands = { ShowVirtualProceduresCommand.NAME + " presley"};
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( PROC_1 ), is( false ) );
        assertThat( output, output.contains( PROC_2 ), is( false ) );
        assertThat( output, output.contains( PROC_3 ), is( false ) );
    }

}
