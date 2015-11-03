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

import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;

/**
 * Test Class to test {@link SetPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class SetPropertyCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "set-property anArg extraArg" };
        setup( commands );
        execute();
    }
    
//    @Test
//    public void shouldSetProperty() throws Exception {
//        final String childName = "blah";
//        final String propName = "primaryType";
//        final String propValue = "nt:unstructured";
//        final String[] commands = { 
//            "workspace", 
//            "add-child " + childName,
//            "cd " + childName,
//            "set-property " + propName + " " + propValue };
//        setup( commands );
//
//        final CommandResult result = execute();
//        assertThat( result.isOk(), is( true ) );
//
//        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
//        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
//        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
//        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getProperty( getTransaction(), propName ).getStringValue(getTransaction()), is( propValue ) );
//    }

}
