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
import org.komodo.shell.api.KomodoObjectLabelProvider;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test Class to test {@link ShowSummaryCommand}.
 */
@SuppressWarnings({"javadoc", "nls"})
public class ShowSummaryCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "show-summary extraArg" };
        execute( commands );
    }

    @Test
    public void shouldBeAvailableAtLibrary() throws Exception {
        final String[] commands = { "library",
                                    "show-summary" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String writerOutput = getCommandOutput();
        assertThat( writerOutput, writerOutput.contains( KomodoObject.class.getSimpleName()
                                                         + " '"
                                                         + KomodoObjectLabelProvider.LIB_DISPLAY_PATH
                                                         + '\'' ),
                    is( true ) );
    }

    @Test
    public void shouldBeAvailableAtRoot() throws Exception {
        final String[] commands = { "show-summary" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String writerOutput = getCommandOutput();
        assertThat( writerOutput, writerOutput.contains( KomodoObject.class.getSimpleName()
                                                         + " '"
                                                         + KomodoObjectLabelProvider.ROOT_DISPLAY_NAME
                                                         + '\'' ),
                    is( true ) );
    }

    @Test
    public void shouldBeAvailableAtWorkspace() throws Exception {
        final String[] commands = { "workspace",
                                    "show-summary" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String writerOutput = getCommandOutput();
        assertThat( writerOutput, writerOutput.contains( KomodoObject.class.getSimpleName()
                                                         + " '"
                                                         + KomodoObjectLabelProvider.WORKSPACE_DISPLAY_PATH
                                                         + '\'' ),
                    is( true ) );
    }

}
