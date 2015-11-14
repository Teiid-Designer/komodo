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
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;

/**
 * Test class for {@link CommitCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class CommitCommandTest extends AbstractCommandTest {

    @Test
    public void shouldCommitEvenIfPlayCommandFails() throws Exception {
        final String child = "blah";
        final String[] commands = { "workspace",
                                    "add-child " + child,
                                    "commit", // should add child and *not* be rollbacked by the play command
                                    "home",
                                    "delete-child workspace" }; // should fail

        try {
            execute( commands );
            fail(); // delete-child should throw exception so should not get here
        } catch ( final Throwable e ) {
            // make sure the work done before the commit was persisted
            assertThat( _repo.komodoWorkspace( getTransaction() ).getChild( getTransaction(), child ), is( notNullValue() ) );
        }
    }

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs() throws Exception {
        final String[] commands = { "commit extraArg" };
        execute( commands );
    }

}
