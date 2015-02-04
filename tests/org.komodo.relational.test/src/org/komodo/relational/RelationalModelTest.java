/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational;

import org.junit.Rule;
import org.junit.rules.TestName;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( {"javadoc"} )
public abstract class RelationalModelTest extends AbstractLocalRepositoryTest {

    @Rule
    public TestName name = new TestName();

}
