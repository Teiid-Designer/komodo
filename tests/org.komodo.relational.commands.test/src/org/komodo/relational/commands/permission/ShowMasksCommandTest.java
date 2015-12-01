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
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test class for {@link ShowMasksCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowMasksCommandTest extends AbstractCommandTest {

    private static final String MASK_1 = "my_mask";
    private static final String MASK_2 = "your_mask";
    private static final String MASK_3 = "batman_mask";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "add-mask " + MASK_1,
                                    "add-mask " + MASK_2,
                                    "add-mask " + MASK_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowMasksCommand.NAME + SPACE + "blah" + SPACE + MASK_1 + SPACE + "*foo*" + SPACE + MASK_2 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( MASK_1 ), is( true ) );
        assertThat( output, output.contains( MASK_2 ), is( true ) );
        assertThat( output, output.contains( MASK_3 ), is( false ) );
    }

    @Test
    public void shouldDisplayMasks() throws Exception {
        final String[] commands = { ShowMasksCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( MASK_1 ), is( true ) );
        assertThat( output, output.contains( MASK_2 ), is( true ) );
        assertThat( output, output.contains( MASK_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayMasksThatMatchPattern() throws Exception {
        final String[] commands = { ShowMasksCommand.NAME + " *our*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( MASK_1 ), is( false ) );
        assertThat( output, output.contains( MASK_2 ), is( true ) );
        assertThat( output, output.contains( MASK_3 ), is( false ) );
    }

    @Test
    public void shouldNotMatchPattern() throws Exception {
        final String[] commands = { ShowMasksCommand.NAME + " *blah*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( MASK_1 ), is( false ) );
        assertThat( output, output.contains( MASK_2 ), is( false ) );
        assertThat( output, output.contains( MASK_3 ), is( false ) );
    }

}
