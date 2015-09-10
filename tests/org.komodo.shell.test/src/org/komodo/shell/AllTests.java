package org.komodo.shell;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All Tests for CommandLine
 */
@RunWith(Suite.class)
@Suite.SuiteClasses( { CdCommandTest.class, DeleteCommandTest.class, ListCommandTest.class,
                      PlayCommandTest.class, SetCommandTest.class,
                      ShowCommandTest.class, UnsetCommandTest.class } )
public class AllTests {
    // nothing to do
}
