package org.komodo.relational.commands;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.komodo.core.KEngine;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.WorkspaceStatusImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.KomodoShell;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.PlayCommand;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.mockito.Mockito;

@SuppressWarnings( { "javadoc", "nls" } )
public abstract class AbstractCommandTest extends AbstractLocalRepositoryTest {

    private static final KLog LOGGER = KLog.getLogger();

    private static KEngine kEngine = KEngine.getInstance();
    private static Path _shellDataDirectory;

    @BeforeClass
    public static void startKEngine() throws Exception {
        assertNotNull( _repo );
        final long startTime = System.currentTimeMillis();
        LOGGER.debug( "AbstractCommandTest:startKEngine" );

        // create data directory for shell
        _shellDataDirectory = Files.createTempDirectory( "VdbBuilderDataDir" );
        System.setProperty( SystemConstants.VDB_BUILDER_DATA_DIR, _shellDataDirectory.toString() );
        LOGGER.debug( "AbstractCommandTest:_shellDataDirectory = {0}", _shellDataDirectory );

        final Path commandsDir = Paths.get( _shellDataDirectory.toString() + "/commands" );
        Files.createDirectory( commandsDir );

        System.setProperty( "komodo.shell.commandsDir", commandsDir.toString() );
        LOGGER.debug( "AbstractCommandTest: commands directory is {0}", commandsDir );

        {// find relational command provider jar and copy over to commands directory so it can be discovered
            final String relativeTargetPath = "../../plugins/org.komodo.relational.commands/target";
            final Path targetDir = Paths.get( relativeTargetPath );
            LOGGER.debug( "AbstractCommandTest: Looking for jar here: {0}", targetDir );

            try ( final DirectoryStream< Path > stream = Files.newDirectoryStream( targetDir, "*-with-dependencies.jar" ) ) {
                final Iterator< Path > itr = stream.iterator();

                if ( itr.hasNext() ) {
                    final Path path = itr.next();
                    final String pathString = path.toString();
                    LOGGER.debug( "AbstractCommandTest: found jar {0}", pathString );

                    if ( itr.hasNext() ) {
                        Assert.fail( "*** Found more than one relational command provider jar ***" ); //$NON-NLS-1$
                    }

                    // copy
                    Files.copy( path, Paths.get( commandsDir.toString() + '/' + path.getFileName() ) );
                    LOGGER.debug( "AbstractCommandTest: copying jar to {0}",
                                  ( commandsDir.toString() + '/' + path.getFileName() ) );
                } else {
                    Assert.fail( "*** Failed to find relational command provider jar ***" ); //$NON-NLS-1$
                }
            } catch ( final IOException e ) {
                Assert.fail( "Failed to copy jar to commands directory: " + e.getMessage() ); //$NON-NLS-1$
            }
        }

        kEngine.setDefaultRepository( _repo );
        kEngine.start();

        assertEquals( RepositoryClient.State.STARTED, kEngine.getState() );
        assertEquals( Repository.State.REACHABLE, kEngine.getDefaultRepository().getState() );
        LOGGER.debug( "AbstractCommandTest:startKEngine finished. Time to start: {0}",
                      ( System.currentTimeMillis() - startTime ) );
    }

    @AfterClass
    public static void stopKEngine() throws Exception {
        assertNotNull( kEngine );

        // delete data directory
        FileUtils.removeDirectoryAndChildren( _shellDataDirectory.toFile() );

        // Reset the latch to signal when repo has been shutdown
        _repoObserver.resetLatch();
        kEngine.shutdown();

        if ( !_repoObserver.getLatch().await( 1, TimeUnit.MINUTES ) ) {
            throw new RuntimeException( "Local repository was not stopped" );
        }

        kEngine.setDefaultRepository( null );
        assertEquals( RepositoryClient.State.SHUTDOWN, kEngine.getState() );
        assertEquals( Repository.State.NOT_REACHABLE, _repo.getState() );
    }

    protected Writer commandWriter;
    private PlayCommand playCmd;
    protected WorkspaceStatus wsStatus;

    @SuppressWarnings( "unused" )
    private UnitOfWork uow; // hides superclass variable from subclasses (always null as transaction from WorkspaceStatus is used)

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

        this.wsStatus = new WorkspaceStatusImpl( super.getTransaction(), komodoShell );
        this.commandWriter = new StringWriter();
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

    protected void setup( final String commandFilePath ) throws Exception {
        final String filePath = ( new File( commandFilePath ).isAbsolute() ) ? commandFilePath
                                                                             : ( "./resources/" + commandFilePath );
        final Arguments args = new Arguments( filePath );

        // construct play command
        this.playCmd = new PlayCommand( this.wsStatus );
        this.playCmd.setArguments( args );
        this.playCmd.setWriter( this.commandWriter );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.test.utils.AbstractLocalRepositoryTest#commit()
     */
    @Override
    protected final void commit() throws Exception {
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
    public void teardown() {
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

            if ( result.isOk() ) {
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
        }
        return result;
    }

    protected void assertCommandResultOk( CommandResult result ) {
        if ( !result.isOk() ) {
            if ( result.getError() != null ) {
                Assert.fail( "Failed : " + result.getError().getMessage() ); //$NON-NLS-1$
            } else {
                Assert.fail( "Failed : " + result.getMessage() ); //$NON-NLS-1$
            }
        }
    }

    /**
     * Get command output. Contains only the output of the command being tested.
     *
     * @return the output
     */
    protected String getCommandOutput() {
        return this.commandWriter.toString();
    }

    /**
     * Get the message indent string
     *
     * @return the indent string
     */
    protected static String getIndentStr() {
        int indent = CompletionConstants.MESSAGE_INDENT;
        StringBuffer sb = new StringBuffer();
        for ( int i = 0; i < indent; i++ ) {
            sb.append( StringConstants.SPACE );
        }
        return sb.toString();
    }

}
