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
 * Test class for the {@link ShowViewsCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowViewsCommandTest extends AbstractCommandTest {

    private static final String VIEW_1 = "my_view";
    private static final String VIEW_2 = "your_view";
    private static final String VIEW_3 = "elvis";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "add-view " + VIEW_1,
                                    "add-view " + VIEW_2,
                                    "add-view " + VIEW_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowViewsCommand.NAME + SPACE + VIEW_1 + SPACE + "el*" + SPACE + "*blah*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( VIEW_1 ), is( true ) );
        assertThat( output, output.contains( VIEW_2 ), is( false ) );
        assertThat( output, output.contains( VIEW_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayViews() throws Exception {
        final String[] commands = { ShowViewsCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( VIEW_1 ), is( true ) );
        assertThat( output, output.contains( VIEW_2 ), is( true ) );
        assertThat( output, output.contains( VIEW_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayViewsThatMatchPattern() throws Exception {
        final String[] commands = { ShowViewsCommand.NAME + " *_*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( VIEW_1 ), is( true ) );
        assertThat( output, output.contains( VIEW_2 ), is( true ) );
        assertThat( output, output.contains( VIEW_3 ), is( false ) );
    }

    @Test
    public void shouldNotMatchPattern() throws Exception {
        final String[] commands = { ShowViewsCommand.NAME + " blah" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( VIEW_1 ), is( false ) );
        assertThat( output, output.contains( VIEW_2 ), is( false ) );
        assertThat( output, output.contains( VIEW_3 ), is( false ) );
    }

}
