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
 * Test Class to test {@link UnsetDataTypeResultSetPropertyCommand}.
 */
@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class UnsetDataTypeResultSetPropertyCommandTest extends DataTypeResultSetCommandTest {

    @Test
    public void shouldUnsetProperty() throws Exception {
        final String property = "ANNOTATION";
        final String[] commands = { "set-property datatypeLength 99",
                                    "set-property " + property + " blah",
                                    "unset-property " + property };

        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertThat( get().hasProperty( getTransaction(), property ), is( false ) );
    }

}
