/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.core;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.repository.test.TestLocalRepository;

@SuppressWarnings( "javadoc" )
@RunWith( Suite.class )
@Suite.SuiteClasses( {TestLocalRepository.class} )
public class AllTests {

    public AllTests() {
        // nothing to do
    }

}
