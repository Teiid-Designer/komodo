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
import org.komodo.spi.repository.KomodoObject;

/**
 * Test Class to test {@link RenameCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class RenameCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldNotRenameAtRoot() throws Exception {
        final String[] commands = { "rename workspace myWorkspace" }; // rename is not available at root
        setup( commands );
        execute();
    }
    
    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String childName = "blah";
        final String newChildName = "blech";
        final String[] commands = { 
            "workspace", 
            "add-child " + childName,
            "rename " + childName + " " + newChildName + " extraArg"};
        
        setup( commands );
        execute();
    }
    
    @Test
    public void shouldRenameChild() throws Exception {
        final String childName = "blah";
        final String newChildName = "blech";
        final String[] commands = { 
            "workspace", 
            "add-child " + childName,
            "rename " + childName + " " + newChildName};
        
        setup( commands );

        final CommandResult result = execute();
        assertThat( result.isOk(), is( true ) );

        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( newChildName ) );
    }

}
