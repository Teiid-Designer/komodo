package org.komodo.shell;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All Tests for CommandLine
 */
@RunWith(Suite.class)
@Suite.SuiteClasses( { CdCommandTest.class, CreateCommandTest.class, DeleteCommandTest.class, ExportCommandTest.class,
                      ListCommandTest.class, SetCommandTest.class, ShowCommandTest.class, UnsetCommandTest.class } )
public class AllTests {
    // nothing to do
}
