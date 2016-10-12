package org.komodo.shell;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.komodo.core.KEngine;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.KomodoShell;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.commands.PlayCommand;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.mockito.Mockito;

/**
 * AbstractCommandTest
 */
@SuppressWarnings({"javadoc", "nls"})
public abstract class AbstractCommandTest extends AbstractLocalRepositoryTest {

    private static final KLog LOGGER = KLog.getLogger();
    private static KEngine kEngine = KEngine.getInstance();
    protected static Path _shellDataDirectory;

    private Writer commandWriter;
    private PlayCommand playCmd;
    protected WorkspaceStatusImpl wsStatus;
    protected Properties globalProperties = new Properties();

    /**
     * @param kEngine
     * @throws Exception
     */
    @BeforeClass
    public static void startKEngine() throws Exception {
        assertNotNull(_repo);
        final long startTime = System.currentTimeMillis();
        LOGGER.debug( "AbstractCommandTest:startKEngine" );

        // create data directory for shell
        _shellDataDirectory = Files.createTempDirectory( "VdbBuilderDataDir" );
        System.setProperty( SystemConstants.VDB_BUILDER_DATA_DIR, _shellDataDirectory.toString() );

        kEngine.setDefaultRepository(_repo);
        kEngine.start();

        assertEquals(RepositoryClient.State.STARTED, kEngine.getState());
        assertEquals(Repository.State.REACHABLE, kEngine.getDefaultRepository().getState());
        LOGGER.debug( "AbstractCommandTest:startKEngine time to start engine: {0}",
                      ( System.currentTimeMillis() - startTime ) );
    }

    /**
     * @throws Exception
     */
    @AfterClass
    public static void stopKEngine() throws Exception {
        assertNotNull(kEngine);
        final long startTime = System.currentTimeMillis();
        LOGGER.debug( "AbstractCommandTest:stopKEngine" );

        // delete data directory
        FileUtils.removeDirectoryAndChildren( _shellDataDirectory.toFile() );

        // Reset the latch to signal when repo has been shutdown
        _repoObserver.resetLatch();
        kEngine.shutdown();

        if (! _repoObserver.getLatch().await(1, TimeUnit.MINUTES)) {
            throw new RuntimeException("Local repository was not stopped");
        }

        kEngine.setDefaultRepository(null);
        assertEquals(RepositoryClient.State.SHUTDOWN, kEngine.getState());
        assertEquals(Repository.State.NOT_REACHABLE, _repo.getState());
        LOGGER.debug( "AbstractCommandTest:stopKEngine time to stop engine: {0}",
                      ( System.currentTimeMillis() - startTime ) );
    }

    @Before
    public void beforeEach() throws Exception {
        assertEquals( RepositoryClient.State.STARTED, kEngine.getState() );
        assertEquals( Repository.State.REACHABLE, kEngine.getDefaultRepository().getState() );

        KomodoShell komodoShell = Mockito.mock( KomodoShell.class );
        Mockito.when( komodoShell.getEngine() ).thenReturn( kEngine );
        Mockito.when( komodoShell.getInputStream() ).thenReturn( System.in );
        Mockito.when( komodoShell.getOutputWriter() ).thenReturn( new StringWriter() );
        Mockito.when( komodoShell.getShellDataLocation() ).thenReturn( _shellDataDirectory.toString() );
        Mockito.when( komodoShell.getShellPropertiesFile() ).thenReturn( "vdbbuilderShell.properties" );

        this.wsStatus = new WorkspaceStatusImpl( super.getTransaction(), komodoShell, globalProperties );
        this.commandWriter = new StringWriter();
    }

    protected CommandResult execute( final String[] commands ) throws Exception {
        setup( commands );
        return execute();
    }

    protected void setup( final String[] commands ) throws Exception {
        final File cmdFile = File.createTempFile( "TestCommand", ".txt" ); //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();

        try ( final FileWriter writer = new FileWriter( cmdFile ) ) {
            for ( final String command : commands ) {
                writer.write( command + NEW_LINE );
            }
        }

        final String commandFilePath = cmdFile.getAbsolutePath();
        setup( commandFilePath );
    }

    protected void assertCommandsAvailable( final String... cmdNames ) throws Exception {
        final Collection< String > available = Arrays.asList( this.wsStatus.getAvailableCommandNames() );

        for ( final String name : cmdNames ) {
            if ( !available.contains( name ) ) {
                Assert.fail( "Command " + name + " should be available" );
            }
        }
    }

    protected void assertCommandsNotAvailable( final String... cmdNames ) throws Exception {
        final Collection< String > available = Arrays.asList( this.wsStatus.getAvailableCommandNames() );

        for ( final String name : cmdNames ) {
            if ( available.contains( name ) ) {
                Assert.fail( "Command " + name + " should not be available" );
            }
        }
    }

    protected static File getResourceFile(Class<?> theClass, String directory, String fileName) {
        try {
            InputStream stream = TestUtilities.getResourceAsStream(theClass, directory, fileName);
            assertNotNull(stream);

            String suffix = DOT + "tmp";
            int dot = fileName.indexOf(DOT);
            if (dot > -1) {
                suffix = fileName.substring(dot);
                fileName = fileName.substring(0, dot);
            }

            File tmpFile = File.createTempFile(fileName, suffix);
            tmpFile.deleteOnExit();
            FileUtils.write(stream, tmpFile);

            return tmpFile;
        } catch (Exception ex) {
            fail(ex.getMessage());
            return null;
        }
    }

    protected static File getResourceFile(Class<?> theClass, String fileName) {
        return getResourceFile(theClass, null, fileName);
    }

    protected void setup( final String commandFilePath ) throws Exception {
    	String filePath = null;
    	if (new File( commandFilePath ).isAbsolute())
    		filePath = commandFilePath;
    	else {
    	    File file = getResourceFile(getClass(), commandFilePath);
    	    assertNotNull(file);
    	    assertTrue(file.exists());
    	    filePath = file.getAbsolutePath();
    	}
    	assertNotNull(filePath);

        // setup arguments for play command
        final Arguments args = new Arguments( filePath );

        // construct play command
        this.playCmd = new PlayCommand( this.wsStatus );
        this.playCmd.setArguments( args );
        this.playCmd.setWriter( this.commandWriter );
    }

	/**
	 * Runs command file in resources folder
	 *
	 * @param folderName
	 *            name of the folder in which is located the command file
	 *            relative to ./resources patch
	 * @param commandFileName
	 *            name of the command file
	 * @throws Exception
	 */
	protected void setup(String folderName, String commandFileName) throws Exception {
	    File file = getResourceFile(getClass(), folderName, commandFileName);
		if (!file.exists()) {
			throw new IllegalArgumentException("Command file does not exist: " + file.getAbsolutePath());
		}
		setup(file.getAbsolutePath());
		assertCommandResultOk(execute());
	}

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.test.utils.AbstractLocalRepositoryTest#commit()
     */
    @Override
    protected void commit() throws Exception {
        this.wsStatus.commit( getClass().getSimpleName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.test.utils.AbstractLocalRepositoryTest#rollback()
     */
    @Override
    protected final void rollback() throws Exception {
        this.wsStatus.rollback( getClass().getSimpleName() );
    }

    @After
    public void teardown( ) {
        this.commandWriter = null;
        this.playCmd = null;
    }

    /**
     * Executes the command file, putting the result output into writer.
     */
    protected CommandResult execute() {
        CommandResult result = null;
        try {
            result = this.playCmd.execute();

            if(result.isOk()) {
                commit();
            } else {
                rollback();

                if ( result.getError() != null ) {
                    throw result.getError();
                }

                Assert.fail( "Failed : " + result.getMessage() ); //$NON-NLS-1$
            }
        } catch ( InvalidCommandArgumentException e ) {
            Assert.fail( "Failed - invalid command: " + e.getMessage() ); //$NON-NLS-1$
        } catch ( Exception e ) {
            Assert.fail( "Failed : " + e.getMessage() ); //$NON-NLS-1$
            e.printStackTrace();
        }
        return result;
    }

    protected void assertCommandResultOk(CommandResult result) {
        if(!result.isOk()) {
            if(result.getError()!=null) {
                Assert.fail("Failed : " + result.getError().getMessage()); //$NON-NLS-1$
            } else {
                Assert.fail("Failed : " + result.getMessage()); //$NON-NLS-1$
            }
        }
    }

    protected void assertContextIs( final String absolutePath ) {
        if ( !this.wsStatus.getCurrentContext().getAbsolutePath().equals( absolutePath ) ) {
            fail( "Expected context '"
                  + absolutePath
                  + "' but was '"
                  + this.wsStatus.getCurrentContext().getAbsolutePath()
                  + '\'' );
        }
    }

    /**
     * Get command output.  Contains only the output of the command being tested.
     * @return the output
     */
    protected String getCommandOutput() {
        return this.commandWriter.toString();
    }

    protected Writer getOutputWriter() {
        return this.commandWriter;
    }

    /**
     * Get the message indent string
     * @return the indent string
     */
    protected static String getIndentStr() {
        int indent = CompletionConstants.MESSAGE_INDENT;
        StringBuffer sb = new StringBuffer();
        for(int i=0; i<indent; i++) {
            sb.append(StringConstants.SPACE);
        }
        return sb.toString();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.test.utils.AbstractLocalRepositoryTest#getTransaction()
     */
    @Override
    protected UnitOfWork getTransaction() {
        return this.wsStatus.getTransaction();
    }

/**
 * Asserts tab completion
 * @param line line to be autocompleted
 * @param expectedCandidates expected candidas of autocompletion
 * @throws Exception
 */
    protected void assertTabCompletion(String line, final List<? extends CharSequence> expectedCandidates) throws Exception{
    	List<CharSequence> candidates=new LinkedList<>();
    	Arguments arguments = null;
		try {
			arguments = new Arguments(line, true);
		} catch (InvalidCommandArgumentException e1) {
			// should never happen...but if it does, just bail
		}
		String commandName = arguments.removeCommandName();
		String lastArgument = null;
		if (arguments.size() > 0 && !line.endsWith(" ")) {
			lastArgument = arguments.remove(arguments.size() - 1);
		}
		ShellCommand command=wsStatus.getCommand(commandName);
		command.setArguments(arguments);
		command.tabCompletion(lastArgument, candidates);
		String compareMessage="Expected: "+Arrays.toString(expectedCandidates.toArray())+" Actual: "+Arrays.toString(candidates.toArray());
        assertThat("Invalid number of autocompletion candidates "+compareMessage,candidates.size(), is(expectedCandidates.size()));
        assertThat("Invalid elements: "+compareMessage, candidates.containsAll(expectedCandidates), is(true));
    }
}
