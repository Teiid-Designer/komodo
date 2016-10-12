package org.komodo.relational.commands;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.komodo.relational.commands.server.WkspStatusServerManager;
import org.komodo.repository.RepositoryImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.utils.KLog;

@SuppressWarnings( { "javadoc",
                     "nls" } )
public abstract class AbstractCommandTest extends org.komodo.shell.AbstractCommandTest {

    private static final KLog LOGGER = KLog.getLogger();
    private static final Path SHELL_DATA_DIRECTORY;

    private static Path _saveShellDataDirectory;

    static {
        Path tempDataDir = null;

        // create data directory for shell
        try {
            tempDataDir = Files.createTempDirectory( "VdbBuilderDataDir" );
            tempDataDir.toFile().deleteOnExit();
            System.setProperty( SystemConstants.VDB_BUILDER_DATA_DIR, tempDataDir.toString() );
            LOGGER.debug( "AbstractCommandTest:_shellDataDirectory = {0}", tempDataDir );

            final Path commandsDir = Paths.get( tempDataDir.toString() + "/commands" );
            commandsDir.toFile().deleteOnExit();
            Files.createDirectory( commandsDir );

            System.setProperty( "komodo.shell.commandsDir", commandsDir.toString() );
            LOGGER.debug( "AbstractCommandTest: commands directory is {0}", commandsDir );

            {// find relational command provider jar and copy over to commands directory so it can be discovered
                final String relativeTargetPath = "target";
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
                        Path filePath = Paths.get( commandsDir.toString() + '/' + path.getFileName() );
                        Files.copy( path, filePath );
                        filePath.toFile().deleteOnExit();
                        LOGGER.debug( "AbstractCommandTest: copying jar to {0}",
                                      ( commandsDir.toString() + '/' + path.getFileName() ) );
                    } else {
                        Assert.fail( "*** Failed to find relational command provider jar ***" ); //$NON-NLS-1$
                    }
                } catch ( final IOException e ) {
                    Assert.fail( "Failed to copy jar to commands directory: " + e.getMessage() ); //$NON-NLS-1$
                }
            }
        } catch ( final Exception e ) {
            Assert.fail( e.getLocalizedMessage() );
        }

        SHELL_DATA_DIRECTORY = tempDataDir;
    }

    @AfterClass
    public static void restoreShellDataDirectory() {
        _shellDataDirectory = _saveShellDataDirectory;
        System.setProperty( SystemConstants.VDB_BUILDER_DATA_DIR, _shellDataDirectory.toString() );
    }

    @BeforeClass
    public static void setShellDataDirectory() {
        _saveShellDataDirectory = _shellDataDirectory;
        _shellDataDirectory = SHELL_DATA_DIRECTORY;
        System.setProperty( SystemConstants.VDB_BUILDER_DATA_DIR, _shellDataDirectory.toString() );
    }
    
    @Before
    @Override
    public void beforeEach() throws Exception {
        // Default - do not attempt to connect to server on startup
        globalProperties.put("SERVER_CONNECT_ON_STARTUP", "false|java.lang.Boolean");
        
        super.beforeEach();
        
        // Startup in the workspace context
        final String[] commands = { "workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertContextIs( RepositoryImpl.komodoWorkspacePath(getTransaction()));
    }
    
    @After
    public void clearServer() throws Exception {
        // Make sure the server is cleared between tests
        WkspStatusServerManager.reset();
    }    

}
