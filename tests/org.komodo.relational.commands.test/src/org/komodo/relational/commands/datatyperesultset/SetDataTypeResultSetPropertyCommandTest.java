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
package org.komodo.relational.commands.datatyperesultset;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetDataTypeResultSetPropertyCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class SetDataTypeResultSetPropertyCommandTest extends DataTypeResultSetCommandTest {

    @Test
    public void shouldSetProperty() throws Exception {
        final long expected = 99;
        final String[] commands = { "set-property datatypeLength " + expected };

        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertThat( get().getLength( getTransaction() ), is( expected ) );
    }

}
