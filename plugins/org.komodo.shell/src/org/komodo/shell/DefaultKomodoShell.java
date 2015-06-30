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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.komodo.core.KEngine;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.KomodoShell;
import org.komodo.shell.api.KomodoShellParent;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.HelpCommand;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.utils.StringUtils;

/**
 * An interactive shell for working with komodo.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 * - altered display message
 * - utilizing different Messages class
 *
 */
public class DefaultKomodoShell implements KomodoShell {

    private static final String LOCALE_PROPERTY = "komodo.shell.locale"; //$NON-NLS-1$
    private String msgIndentStr = StringConstants.EMPTY_STRING;

    /**
     * Main entry point.
     * @param args the arguments
     */
    public static void main(String[] args) {
        String locale_str = System.getProperty(LOCALE_PROPERTY);
        if (locale_str != null) {
            String lang = null;
            String region = null;
            String[] lsplit = locale_str.split("_"); //$NON-NLS-1$
            if (lsplit.length > 0) {
                lang = lsplit[0];
            }
            if (lsplit.length > 1) {
                region = lsplit[1];
            }
            if (lang != null && region != null) {
                Locale.setDefault(new Locale(lang, region));
            } else if (lang != null) {
                Locale.setDefault(new Locale(lang));
            }
        }

        final KomodoShellParent parent = new KomodoShellParent() {

            @Override
            public void exit() {
                System.exit(0);
            }
        };

        final DefaultKomodoShell shell = new DefaultKomodoShell(parent, KEngine.getInstance(), System.in, System.out);
        Thread shutdownHook = new Thread(new Runnable() {
            @Override
            public void run() {
                shell.shutdown();
            }
        });
        Runtime.getRuntime().addShutdownHook(shutdownHook);
        try {
            shell.run(args);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private String dataDirectory;
    private WorkspaceStatus wsStatus;
    private ShellCommandFactory factory;
    private ShellCommandReader reader;
    private boolean shutdown = false;

    private final KomodoShellParent parent;
    private final KEngine kEngine;
    private final InputStream inStream;
    private final PrintStream outStream;
    private final Writer commandOutput;

    /**
     * Constructor.
     * @param parent the UI parent of this shell
     * @param kEngine the komodo engine for this shell
     * @param inStream input stream
     * @param outStream output stream
     */
    public DefaultKomodoShell(KomodoShellParent parent, KEngine kEngine, InputStream inStream, PrintStream outStream) {
        this.parent = parent;
        this.kEngine = kEngine;
        this.inStream = inStream;
        this.outStream = outStream;
        this.commandOutput = new OutputStreamWriter(outStream);

        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < CompletionConstants.MESSAGE_INDENT; i++) {
            sb.append(StringConstants.SPACE);
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
    public PrintStream getOutputStream() {
        return outStream;
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
                assert ( engineDataDirectory != null );

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
	public Writer getCommandOutput() {
    	return commandOutput;
    }

    @Override
    public void exit() {
        shutdown();
        parent.exit();
    }

    /**
     * Runs the shell.
     * @param args the arguments
     * @throws Exception the exception
     */
    public void run(String[] args) throws Exception {
        // This will block and await the start of both the engine and its default repository
        startKEngine();

        wsStatus = new WorkspaceStatusImpl(this);
        factory = new ShellCommandFactory(wsStatus);
//
//        File startupPropertiesFile = new File(STARTUP_PROPERTIES_FILEPATH);
//        if (startupPropertiesFile.exists() && startupPropertiesFile.isFile() && startupPropertiesFile.canRead()) {
//            Properties props = new Properties();
//
//            try {
//                props.load(new FileInputStream(startupPropertiesFile));
//            } catch (Exception e) {
//                // ignore
//            }
//            wsStatus.setProperties(props);
//        }
//
        reader = ShellCommandReaderFactory.createCommandReader(args, factory, wsStatus);
        reader.open();

        displayWelcomeMessage();

        // run help command
        final ShellCommand helpCmd = factory.getCommand( HelpCommand.NAME );
        helpCmd.setArguments( new Arguments( EMPTY_STRING ) );
        helpCmd.setOutput( getCommandOutput() );
        helpCmd.execute();

        boolean done = false;
        while (!done && !shutdown) {
            ShellCommand command = null;
            try {
                if (shutdown)
                    break;

                command = reader.read();
                if (command == null) {
                    done = true;
                } else {
                    boolean success = command.execute();

                    if ( success ) {
                        if ( this.wsStatus.getRecordingStatus() ) {
                            command.record();
                        }
                    } else if ( this.reader.isBatch() ) {
                        shutdown();
                    }
                }
            } catch (InvalidCommandArgumentException e) {
                outStream.println(msgIndentStr + Messages.getString(SHELL.INVALID_ARG, e.getMessage()));
                if (command != null) {
                    outStream.println(msgIndentStr + Messages.getString(SHELL.USAGE));
                    command.printUsage(CompletionConstants.MESSAGE_INDENT);
                }
                if (reader.isBatch())
                    shutdown();
            } catch (Exception e) {
                String msg = "Exception Occurred: " + (e.getLocalizedMessage() == null ? e.getClass().getSimpleName() : e.getLocalizedMessage()); //$NON-NLS-1$
                displayMessage(msgIndentStr + msg + NEW_LINE);
                if (reader.isBatch())
                    shutdown();
            }
        }
    }

    private void startKEngine() throws KException, InterruptedException {
        final Repository defaultRepo = kEngine.getDefaultRepository();

        // Latch for awaiting the start of the default repository
        final CountDownLatch updateLatch = new CountDownLatch(1);

        // Observer attached to the default repository for listening for the change of state
        RepositoryObserver stateObserver = new RepositoryObserver() {

            @Override
            public void eventOccurred() {
                updateLatch.countDown();
            }
        };
        defaultRepo.addObserver(stateObserver);

        displayMessage(Messages.getString(SHELL.ENGINE_STARTING));
        kEngine.start();
        displayMessage(SPACE + Messages.getString(SHELL.COMPONENT_STARTED) + NEW_LINE);

        displayMessage(Messages.getString(SHELL.LOCAL_REPOSITORY_STARTING));

        TimerTask progressTask = new TimerTask() {
            @Override
            public void run() {
                displayMessage(DOT);
            }
        };

        Timer timer = new Timer();
        timer.schedule(progressTask, 0, 500);

        // Block the thread until the latch has counted down or timeout has been reached
        boolean localRepoWaiting = updateLatch.await(3, TimeUnit.MINUTES);

        // Cancel timer and display repository message
        timer.cancel();

        if (localRepoWaiting) displayMessage(SPACE + Messages.getString(SHELL.COMPONENT_STARTED));
        else displayMessage(SPACE + Messages.getString(SHELL.LOCAL_REPOSITORY_TIMEOUT_ERROR));

        displayMessage(NEW_LINE);
        displayMessage(NEW_LINE);
    }

    /**
     * Shuts down the shell.
     */
    public void shutdown() {
        if ( this.shutdown ) {
            return; // avoid shutting down twice
        }

        displayMessage( this.msgIndentStr + Messages.getString( SHELL.SHUTTING_DOWN ) );
        this.shutdown = true;

        // workspace status must be saved before engine is stopped
        try {
            this.wsStatus.save();
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

        displayMessage( this.msgIndentStr + Messages.getString( SHELL.DONE ) );
    }

    private void displayMessage(String message) {
        outStream.print(message);
    }

    /**
     * Displays a welcome message to the user.
     */
    private void displayWelcomeMessage() {
        displayMessage(Messages.getString(SHELL.WelcomeMessage));
    }
}
