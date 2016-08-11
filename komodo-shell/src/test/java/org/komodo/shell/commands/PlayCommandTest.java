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
import org.komodo.shell.api.WorkspaceStatus;

@SuppressWarnings( { "javadoc", "nls" } )
public final class PlayCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailToPlayNonExistentFile() throws Exception {
        setup( "bogus.txt" );
        execute();
    }

    @Test
    public void shouldPlayFile() throws Exception {
        setup( "test-command-file.txt" );
        final CommandResult result = execute();

        assertCommandResultOk( result );
        assertContextIs( "/tko:komodo/tko:workspace/" + TEST_USER + "/blah/blahblah" );
    }

    @Test
    public void shouldKeepAutoCommitValueWhenSetInCommandsFile() throws Exception {
        assertThat( this.wsStatus.isAutoCommit(), is( true ) );

        final boolean expected = false; // set in command file
        setup( "set-auto-commit.txt" );
        final CommandResult result = execute();

        assertCommandResultOk( result );
        assertThat( this.wsStatus.isAutoCommit(), is( expected ) );
    }

    @Test
    public void shouldNotAffectAutoCommitFlagWhenAutoCommitIsOff() throws Exception {
        final boolean expected = false;
        this.wsStatus.setGlobalProperty( WorkspaceStatus.AUTO_COMMIT, Boolean.valueOf( expected ).toString() );
        assertThat( this.wsStatus.isAutoCommit(), is( expected ) );

        setup( "test-command-file.txt" );
        final CommandResult result = execute();

        assertCommandResultOk( result );
        assertThat( this.wsStatus.isAutoCommit(), is( expected ) );
    }

    @Test
    public void shouldNotAffectAutoCommitFlagWhenAutoCommitIsOn() throws Exception {
        final boolean expected = true;
        this.wsStatus.setGlobalProperty( WorkspaceStatus.AUTO_COMMIT, Boolean.valueOf( expected ).toString() );
        assertThat( this.wsStatus.isAutoCommit(), is( expected ) );

        setup( "test-command-file.txt" );
        final CommandResult result = execute();

        assertCommandResultOk( result );
        assertThat( this.wsStatus.isAutoCommit(), is( expected ) );
    }

}
