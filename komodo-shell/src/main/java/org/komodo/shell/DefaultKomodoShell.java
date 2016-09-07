/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.komodo.core.KEngine;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.KomodoShell;
import org.komodo.shell.api.KomodoShellParent;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.shell.commands.HelpCommand;
import org.komodo.shell.commands.SetRecordCommand;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * An interactive shell for working with komodo. This class adapted from
 * https://github.com/Governance/s-ramp/blob/master/s-ramp-shell - altered to use WorkspaceStatus - altered display message -
 * utilizing different Messages class
 */
public class DefaultKomodoShell implements KomodoShell {

    private static final String LOCALE_PROPERTY = "komodo.shell.locale"; //$NON-NLS-1$
    private static final String PROPERTIES_FILE_NAME = "vdbbuilder.properties"; //$NON-NLS-1$
    private String msgIndentStr = StringConstants.EMPTY_STRING;

    /**
     * Main entry point.
     *
     * @param args
     *        the arguments
     */
    public static void main( String[] args ) {
        String locale_str = System.getProperty( LOCALE_PROPERTY );
        if ( locale_str != null ) {
            String lang = null;
            String region = null;
            String[] lsplit = locale_str.split( "_" ); //$NON-NLS-1$
            if ( lsplit.length > 0 ) {
                lang = lsplit[ 0 ];
            }
            if ( lsplit.length > 1 ) {
                region = lsplit[ 1 ];
            }
            if ( lang != null && region != null ) {
                Locale.setDefault( new Locale( lang, region ) );
            } else if ( lang != null ) {
                Locale.setDefault( new Locale( lang ) );
            }
        }

        final KomodoShellParent parent = new KomodoShellParent() {

            @Override
            public void exit() {
                System.exit( 0 );
            }
        };

        final DefaultKomodoShell shell = new DefaultKomodoShell( parent, KEngine.getInstance(), System.in, System.out );

        // Terminate if startup input is not valid
        if( args.length == 1 && !args[0].equals("-simple") ) {  //$NON-NLS-1$
            PrintUtils.print( shell.getOutputWriter(), false, 0, I18n.bind( ShellI18n.invalidStartupArgs, args[0] ) );
            System.exit(0);
        } else if( args.length > 1 ) {
            if(!args[0].equals("-f")) {  //$NON-NLS-1$
                PrintUtils.print( shell.getOutputWriter(),
                                  false,
                                  0,
                                  I18n.bind( ShellI18n.invalidStartupArgs, args[ 0 ] + " " + args[ 1 ] ) ); //$NON-NLS-1$
                System.exit(0);
            } else {
                File inputFile = new File(args[1]);
                if(!inputFile.isFile()) {
                    PrintUtils.print( shell.getOutputWriter(), false, 0, I18n.bind( ShellI18n.invalidStartupFile, args[1] ) );
                    System.exit(0);
                }
            }
        }

        Thread shutdownHook = new Thread( new Runnable() {
            @Override
            public void run() {
                shell.shutdown();
            }
        } );
        Runtime.getRuntime().addShutdownHook( shutdownHook );
        try {
            shell.run( args );
        } catch ( Exception e ) {
            throw new RuntimeException( e );
        }
    }

    private String dataDirectory;
    private WorkspaceStatus wsStatus;
    private ShellCommandReader reader;
    private boolean shutdown = false;

    private final KomodoShellParent parent;
    private final KEngine kEngine;
    private final InputStream inStream;
    private final Writer outputWriter;

    /**
     * Constructor.
     *
     * @param parent
     *        the UI parent of this shell
     * @param kEngine
     *        the komodo engine for this shell
     * @param inStream
     *        input stream
     * @param outStream
     *        output stream
     */
    public DefaultKomodoShell( KomodoShellParent parent,
                               KEngine kEngine,
                               InputStream inStream,
                               PrintStream outStream ) {
        this.parent = parent;
        this.kEngine = kEngine;
        this.inStream = inStream;
        this.outputWriter = new OutputStreamWriter( outStream );

        StringBuffer sb = new StringBuffer();
        for ( int i = 0; i < CompletionConstants.MESSAGE_INDENT; i++ ) {
            sb.append( StringConstants.SPACE );
        }
        msgIndentStr = sb.toString();
    }

    @Override
    public KEngine getEngine() {
        return kEngine;
    }

    @Override
    public InputStream getInputStream() {
        return inStream;
    }

    @Override
    public Writer getOutputWriter() {
        return outputWriter;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoShell#getShellDataLocation()
     */
    @Override
    public String getShellDataLocation() {
        if ( StringUtils.isBlank( this.dataDirectory ) ) {
            this.dataDirectory = System.getProperty( SystemConstants.VDB_BUILDER_DATA_DIR );

            // if blank set to default
            if ( StringUtils.isBlank( this.dataDirectory ) ) {
                final String engineDataDirectory = System.getProperty( SystemConstants.ENGINE_DATA_DIR );
                assert( engineDataDirectory != null );

                this.dataDirectory = ( engineDataDirectory + File.separatorChar + "vdbbuilder" ); //$NON-NLS-1$
                System.setProperty( SystemConstants.VDB_BUILDER_DATA_DIR, this.dataDirectory );
            }
        }

        return this.dataDirectory;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoShell#setShellDataLocation(java.lang.String)
     */
    @Override
    public void setShellDataLocation( final String dataDirectory ) {
        // if the workspace status is null we are not running so allow data directory to be set
        if ( hasNotBeenRun() ) {
            if ( StringUtils.isBlank( dataDirectory ) ) {
                this.dataDirectory = null;
            } else {
                this.dataDirectory = dataDirectory;
            }
        }
    }

    private boolean hasNotBeenRun() {
        return ( ( this.wsStatus == null ) && !this.shutdown );
    }

    @Override
    public void exit() {
        shutdown();
        parent.exit();
    }

    /**
     * Runs the shell.
     *
     * @param args
     *        the arguments
     * @throws Exception
     *         the exception
     */
    public void run( String[] args ) throws Exception {
        // This will block and await the start of both the engine and its default repository
        startKEngine();

        wsStatus = new WorkspaceStatusImpl( this );

        reader = ShellCommandReaderFactory.createCommandReader( args, wsStatus );
        reader.open();

        displayWelcomeMessage();

        // run help command
        final ShellCommand helpCmd = this.wsStatus.getCommand( HelpCommand.NAME );
        helpCmd.setArguments( new Arguments( EMPTY_STRING ) );
        helpCmd.setWriter( getOutputWriter() );

        // if help command successfully executes set done flag to false
        boolean done = !helpCmd.execute().isOk();

        while ( !done && !shutdown ) {
            ShellCommand command = null;
            try {
                if ( shutdown ) break;

                command = reader.read();
                if ( command == null ) {
                    done = true;
                    exit();
                } else {
                    // execute
                    final CommandResult result = command.execute();
                    KLog.getLogger().debug( "Command: {0}, Succeeded: {1}", command, result.isOk() ); //$NON-NLS-1$

                    // display command execution message
                    if ( !StringUtils.isBlank( result.getMessage() ) ) {
                        PrintUtils.print( getOutputWriter(), CompletionConstants.MESSAGE_INDENT, result.getMessage() );
                    }

                    if ( result.isOk() ) {
                        // record
                        if ( this.wsStatus.getRecordingStatus() && !( command instanceof SetRecordCommand ) ) {
                            writeCommandToRecordingFile( command );
                        }

                        // persist
                        if ( this.wsStatus.isAutoCommit() && result.isPersistable() ) {
                            this.wsStatus.commit( command.getClass().getSimpleName() );
                        }
                    } else {
                        // log error
                        if ( result.getError() != null ) {
                            final String errorMsg = I18n.bind( ShellI18n.commandFailure, command.toString() )
                                                    + ' '
                                                    + result.getError().getLocalizedMessage();
                            KLog.getLogger().debug( errorMsg, result.getError() );
                            PrintUtils.print( getOutputWriter(), CompletionConstants.MESSAGE_INDENT, errorMsg );
                        }

                        // rollback (dont rollback for Exit - user must specify)
                        if ( result.isPersistable() && !ExitCommand.NAME.equals( command.getName() ) ) {
                            this.wsStatus.rollback( command.getClass().getSimpleName() );
                        }

                        // shutdown if necessary
                        if ( this.reader.isBatch() ) {
                            done = true;

                            if ( !this.shutdown ) {
                                shutdown();
                            }
                        }
                    }
                }
            } catch ( InvalidCommandArgumentException e ) {
                PrintUtils.print( getOutputWriter(),
                                  CompletionConstants.MESSAGE_INDENT,
                                  I18n.bind( ShellI18n.invalidArg, e.getMessage() ) );

                if ( command != null ) {
                    PrintUtils.print( getOutputWriter(), CompletionConstants.MESSAGE_INDENT, I18n.bind( ShellI18n.usage ) );
                    command.printUsage( CompletionConstants.MESSAGE_INDENT );
                }

                if ( this.reader.isBatch() ) {
                    done = true;

                    if ( !this.shutdown ) {
                        shutdown();
                    }
                }
            } catch ( final Exception e ) {
                // transaction name could have information about error (see WorkspaceStatusImpl commit and rollback)
                String msg = ( this.wsStatus.getTransaction().getName() + ':' );

                // log
                msg += ( ( e.getLocalizedMessage() == null ) ? e.getClass().getSimpleName() : e.getLocalizedMessage() );
                displayMessage( msgIndentStr + msg + NEW_LINE );

                { // print out error message for causes of error
                    Throwable error = e;

                    while ( error.getCause() != null ) {
                        error = error.getCause();
                        displayMessage( msgIndentStr + error.getLocalizedMessage() + NEW_LINE );
                    }
                }

                KLog.getLogger().debug( msg, e );

                // rollback
                this.wsStatus.rollback( DefaultKomodoShell.class.getSimpleName() );

                // exit
                if ( this.reader.isBatch() ) {
                    done = true;

                    if ( !this.shutdown ) {
                        shutdown();
                    }
                }
            }
        }
    }

    private void startKEngine() throws KException, InterruptedException {
        final Repository defaultRepo = kEngine.getDefaultRepository();
        
        // Latch for awaiting the start of the default repository
        final CountDownLatch updateLatch = new CountDownLatch( 1 );
        final Throwable[] engineError = new Throwable[1];

        // Observer attached to the default repository for listening for the change of state
        RepositoryObserver stateObserver = new RepositoryObserver() {

            @Override
            public void eventOccurred() {
                updateLatch.countDown();
            }

            @Override
            public void errorOccurred(Throwable e) {
                engineError[0] = e;
                updateLatch.countDown();
            }
        };
        defaultRepo.addObserver( stateObserver );

        displayMessage( I18n.bind( ShellI18n.engineStarting ) + NEW_LINE );
        kEngine.start();
        displayMessage( SPACE + I18n.bind( ShellI18n.componentStarted ) + NEW_LINE );

        displayMessage( I18n.bind( ShellI18n.localRepositoryStarting ) );

        TimerTask progressTask = new TimerTask() {
            @Override
            public void run() {
                displayMessage( DOT );
            }
        };

        Timer timer = new Timer();
        timer.schedule( progressTask, 0, 500 );

        // Block the thread until the latch has counted down or timeout has been reached
        boolean localRepoWaiting = updateLatch.await( 3, TimeUnit.MINUTES );

        // Cancel timer and display repository message
        timer.cancel();

        if ( localRepoWaiting && engineError[0] == null)
            displayMessage( SPACE + I18n.bind( ShellI18n.componentStarted ) );
        else if (engineError[0] != null)
            displayMessage(I18n.bind( ShellI18n.engineStartingError, engineError[0].getMessage()));
        else
            displayMessage( SPACE + I18n.bind( ShellI18n.localRepositoryTimeoutError ) );

        displayMessage( NEW_LINE );
        displayMessage( NEW_LINE );
    }

    /**
     * Shuts down the shell.
     */
    public void shutdown() {
        if ( this.shutdown ) {
            return; // avoid shutting down twice
        }

        displayMessage( this.msgIndentStr + I18n.bind( ShellI18n.shuttingDown ) + NEW_LINE );
        this.shutdown = true;

        // save properties
        try {
            final String shellDataDir = getShellDataLocation();
            final Path propFile = Paths.get( shellDataDir, PROPERTIES_FILE_NAME );

            if ( !Files.exists( propFile ) ) {
                if ( !Files.exists( propFile.getParent() ) ) {
                    Files.createDirectories( propFile.getParent() );
                }

                Files.createFile( propFile );
            }

            Properties allProps = this.wsStatus.getGlobalProperties(true);
            Properties providedProps = this.wsStatus.getProvidedGlobalProperties();
            // For provided types, adds the expected type before output
            Properties providedWithTypes = new Properties();
            for(String pName : providedProps.stringPropertyNames()) {
                String pVal = providedProps.getProperty(pName);
                String valType = this.wsStatus.getProvidedGlobalPropertyTypes().get(pName);
                providedWithTypes.put(pName, pVal+'|'+valType);
            }
            allProps.putAll(providedWithTypes);
            allProps.store( new FileOutputStream( propFile.toString() ), null );
            this.wsStatus.closeRecordingWriter();

            // should not have any changes at this point unless user ctrl-c
            this.wsStatus.rollback( DefaultKomodoShell.class.getSimpleName() );
        } catch ( final Exception e ) {
            displayMessage( "Error during shutdown saving workspace status: " + e.getLocalizedMessage() ); //$NON-NLS-1$
        }

        try {
            if ( this.kEngine != null ) {
                this.kEngine.shutdownAndWait();
            }
        } catch ( final Exception e ) {
            displayMessage( "Error during shutdown shutting down KEngine: " + e.getLocalizedMessage() ); //$NON-NLS-1$
        }

        try {
            this.reader.close();
        } catch ( final IOException e ) {
            displayMessage( "Error during shutdown closing shell reader: " + e.getLocalizedMessage() ); //$NON-NLS-1$
        }

        displayMessage( this.msgIndentStr + I18n.bind( ShellI18n.goodBye ) + NEW_LINE );
    }

    private void displayMessage( String message ) {
        PrintUtils.print( outputWriter, false, 0, message );
    }

    /**
     * Displays a welcome message to the user.
     */
    private void displayWelcomeMessage() {
        displayMessage( I18n.bind( ShellI18n.welcomeMessage, getVersion() ) );
    }

    /**
     * Write the supplied line to the recording output file.
     *
     * @param line
     *        the line to output
     */
    private void writeCommandToRecordingFile( ShellCommand command ) {
        Writer recordingWriter = wsStatus.getRecordingWriter();
        if ( recordingWriter != null ) {
            try {
                recordingWriter.write( command.toString() + StringConstants.NEW_LINE );
                recordingWriter.flush();
            } catch ( IOException ex ) {
                String filePath = wsStatus.getGlobalProperties(false).getProperty( WorkspaceStatus.RECORDING_FILE_KEY );
                PrintUtils.print( recordingWriter, 0, I18n.bind( ShellI18n.recordingFileOutputError, filePath ) );
            }
            // Print error message if the recording file was not defined
        } else {
            PrintUtils.print( recordingWriter, 0, I18n.bind( ShellI18n.recordingFileNotDefined ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoShell#getShellPropertiesFile()
     */
    @Override
    public String getShellPropertiesFile() {
        return PROPERTIES_FILE_NAME;
    }
    
    /**
     * Method that reads version info from properties file
     * 
     * @return version info
     */
    private String getVersion(){
    	
    	Properties properties = new Properties();
    	try {
    		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
			properties.load(classLoader.getResourceAsStream("info.properties")); //$NON-NLS-1$
			return properties.getProperty("komodo.version"); //$NON-NLS-1$
		} catch (Exception e) {
			displayMessage( I18n.bind( ShellI18n.errorLoadingVersion ) + e.getLocalizedMessage() );
			return I18n.bind( ShellI18n.errorObtainingVersion );
		}
    		
    }

}
