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
 * Test class for the {@link ShowSourcesCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowSourcesCommandTest extends AbstractCommandTest {

    private static final String SOURCE_1 = "my_source";
    private static final String SOURCE_2 = "your_source";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "add-source " + SOURCE_1,
                                    "add-source " + SOURCE_2 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldDisplaySources() throws Exception {
        final String[] commands = { ShowSourcesCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( SOURCE_1 ), is( true ) );
        assertThat( output, output.contains( SOURCE_2 ), is( true ) );
    }

    @Test( expected = AssertionError.class )
    public void shouldNotAllowArguments() throws Exception {
        final String[] commands = { ShowSourcesCommand.NAME + " blah" };
        execute( commands );
    }

}
