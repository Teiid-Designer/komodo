package org.komodo.shell;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All Tests for CommandLine
 */
@RunWith(Suite.class)
@Suite.SuiteClasses( { CdCommandTest.class, HomeCommandTest.class, ListCommandTest.class,
                      PlayCommandTest.class, ShowChildrenCommandTest.class,
                      ShowGlobalCommandTest.class, ShowPropertiesCommandTest.class, ShowPropertyCommandTest.class,
                      ShowStatusCommandTest.class, ShowSummaryCommandTest.class} )
public class AllTests {
    // nothing to do
}
