/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands;

import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;

/**
 * Test class for the {@link AddDescriptorCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class AddDescriptorCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { 
            "workspace", 
            "add-child blah",
            "cd blah",
            "add-descriptor aDescriptor extraArg"};
        setup( commands );
        execute();
    }
    
    @Test( expected = AssertionError.class )
    public void shouldNotAddDescriptorAtRoot() throws Exception {
        final String[] commands = { "add-descriptor blah" }; // add-descriptor is not available at root
        setup( commands );
        execute();
    }

    @Test( expected = AssertionError.class )
    public void shouldNotAddBadDescriptor() throws Exception {
        final String childName = "blah";
        final String[] commands = { 
            "workspace", 
            "add-child " + childName,
            "cd " + childName,
            "add-descriptor aDescriptor"};
        setup( commands );
        execute();
    }
    
//    @Test
//    public void shouldAddDescriptor() throws Exception {
//        final String childName = "blah";
//        final String[] commands = { 
//            "workspace", 
//            "add-child " + childName,
//            "cd " + childName,
//            "add-descriptor nt:nodeType"};
//        setup( commands );
//
//        final CommandResult result = execute();
//        assertThat( result.isOk(), is( true ) );
//
//        final KomodoObject workspace = _repo.komodoWorkspace( getTransaction() );
//        assertThat( workspace.getChildren( getTransaction() ).length, is( 1 ) );
//        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getName( getTransaction() ), is( childName ) );
//        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getDescriptors(getTransaction()).length, is( 1 ) );
//        assertThat( workspace.getChildren( getTransaction() )[ 0 ].getDescriptors(getTransaction())[0].getName(), is( "nt:nodeType" ) );
//    }

}
