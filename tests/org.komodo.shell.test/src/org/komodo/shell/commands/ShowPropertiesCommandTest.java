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
package org.komodo.shell.commands;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link ShowPropertiesCommand}.
 */
@SuppressWarnings({"javadoc", "nls"})
public class ShowPropertiesCommandTest extends AbstractCommandTest {

    @Test
    public void shouldNotBeAvailableAtLibrary() throws Exception {
        final String[] commands = { "library",
                                    "show-properties" };
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( false ) );
        assertThat( result.getMessage(), result.getMessage().contains( ShowPropertiesCommand.NAME ), is( true ) );
    }

    @Test
    public void shouldNotBeAvailableAtRoot() throws Exception {
        final String[] commands = { "show-properties" };
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( false ) );
        assertThat( result.getMessage(), result.getMessage().contains( ShowPropertiesCommand.NAME ), is( true ) );
    }

    @Test
    public void shouldNotBeAvailableAtWorkspace() throws Exception {
        final String[] commands = { "workspace",
                                    "show-properties" };
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( false ) );
        assertThat( result.getMessage(), result.getMessage().contains( ShowPropertiesCommand.NAME ), is( true ) );
    }

}
