package org.komodo.shell;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All Tests for CommandLine
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({ CdCommandTest.class, ListCommandTest.class, StatusCommandTest.class, CreateCommandTest.class, DeleteCommandTest.class, })
public class AllTests {
    // nothing to do
}
