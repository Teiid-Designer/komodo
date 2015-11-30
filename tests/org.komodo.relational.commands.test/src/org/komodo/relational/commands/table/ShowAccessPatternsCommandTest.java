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
 * Test class for {@link ShowAccessPatternsCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class ShowAccessPatternsCommandTest extends AbstractCommandTest {

    private static final String ACCESS_PATTERN_1 = "my_access_pattern";
    private static final String ACCESS_PATTERN_2 = "your_access_pattern";
    private static final String ACCESS_PATTERN_3 = "elvis_is_king";

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "add-table myTable",
                                    "cd myTable",
                                    "add-access-pattern " + ACCESS_PATTERN_1,
                                    "add-access-pattern " + ACCESS_PATTERN_2,
                                    "add-access-pattern " + ACCESS_PATTERN_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
    }

    @Test
    public void shouldAllowMultiplePatterns() throws Exception {
        final String[] commands = { ShowAccessPatternsCommand.NAME + SPACE + ACCESS_PATTERN_1 + SPACE + ACCESS_PATTERN_3 };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ACCESS_PATTERN_1 ), is( true ) );
        assertThat( output, output.contains( ACCESS_PATTERN_2 ), is( false ) );
        assertThat( output, output.contains( ACCESS_PATTERN_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayAccessPatterns() throws Exception {
        final String[] commands = { ShowAccessPatternsCommand.NAME };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ACCESS_PATTERN_1 ), is( true ) );
        assertThat( output, output.contains( ACCESS_PATTERN_2 ), is( true ) );
        assertThat( output, output.contains( ACCESS_PATTERN_3 ), is( true ) );
    }

    @Test
    public void shouldDisplayAccessPatternsThatMatchPattern() throws Exception {
        final String[] commands = { ShowAccessPatternsCommand.NAME + " your*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ACCESS_PATTERN_1 ), is( false ) );
        assertThat( output, output.contains( ACCESS_PATTERN_2 ), is( true ) );
        assertThat( output, output.contains( ACCESS_PATTERN_3 ), is( false ) );
    }

    @Test
    public void shouldNotMatchPattern() throws Exception {
        final String[] commands = { ShowAccessPatternsCommand.NAME + " blah*" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final String output = getCommandOutput();
        assertThat( output, output.contains( ACCESS_PATTERN_1 ), is( false ) );
        assertThat( output, output.contains( ACCESS_PATTERN_2 ), is( false ) );
        assertThat( output, output.contains( ACCESS_PATTERN_3 ), is( false ) );
    }

}
