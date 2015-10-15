package org.komodo.shell;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All Tests for CommandLine
 */
@RunWith(Suite.class)
@Suite.SuiteClasses( { CdCommandTest.class, CommitCommandTest.class, ExitCommandTest.class, HelpCommandTest.class,
                       HomeCommandTest.class, LibraryCommandTest.class, ListCommandTest.class, PlayCommandTest.class, RenameCommandTest.class,
                       RollbackCommandTest.class, SetAutoCommitCommandTest.class, SetGlobalPropertyCommandTest.class,
                       SetPropertyCommandTest.class, SetRecordCommandTest.class, ShowChildrenCommandTest.class,
                       ShowGlobalCommandTest.class, ShowPropertiesCommandTest.class, ShowPropertyCommandTest.class,
                       ShowStatusCommandTest.class, ShowSummaryCommandTest.class, UnsetPropertyCommandTest.class, WorkspaceCommandTest.class} )
public class AllTests {
    // nothing to do
}
