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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Locale;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.komodo.core.KEngine;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryObserver;


/**
 * An interactive shell for working with komodo.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 * - altered display message
 * - utilizing different Messages class
 * 
 */
public class KomodoShell implements StringConstants {

    private static final String LOCALE_PROPERTY = "komodo.shell.locale"; //$NON-NLS-1$
    private static final String STARTUP_PROPERTIES_FILEPATH = "./komodoShell.properties"; //$NON-NLS-1$
    private String msgIndentStr = StringConstants.EMPTY_STRING;

	/**
	 * Main entry point.
	 * @param args the arguments
	 */
	public static void main(String [] args) {
	    String locale_str = System.getProperty(LOCALE_PROPERTY);
	    if (locale_str != null) {
	        String lang = null;
	        String region = null;
	        String [] lsplit = locale_str.split("_"); //$NON-NLS-1$
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

		final KomodoShell shell = new KomodoShell(KEngine.getInstance(), System.in, System.out);
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

    private WorkspaceStatus wsStatus;
    private ShellCommandFactory factory;
    private ShellCommandReader reader;
    private boolean shutdown = false;

    private final KEngine kEngine;
    private final InputStream inStream;
    private final PrintStream outStream;

	/**
	 * Constructor.
	 * @param kEngine the komodo engine for this shell
	 * @param inStream input stream
	 * @param outStream output stream
	 */
	public KomodoShell(KEngine kEngine, InputStream inStream, PrintStream outStream) {
	    this.kEngine = kEngine;
        this.inStream = inStream;
        this.outStream = outStream;
	    
		StringBuffer sb = new StringBuffer();
		for(int i=0; i<CompletionConstants.MESSAGE_INDENT; i++) {
			sb.append(StringConstants.SPACE);
		}
		msgIndentStr = sb.toString();
	}

	/**
	 * Runs the shell.
	 * @param args the arguments
	 * @throws Exception the exception
	 */
	public void run(String[] args) throws Exception {
		// This will block and await the start of both the engine and its default repository
		startKEngine();      

		wsStatus = new WorkspaceStatusImpl(kEngine, inStream, outStream);
        factory = new ShellCommandFactory(wsStatus);

        File startupPropertiesFile = new File(STARTUP_PROPERTIES_FILEPATH);
        if(startupPropertiesFile.exists() && startupPropertiesFile.isFile() && startupPropertiesFile.canRead()) {
            Properties props = new Properties();

            try {
                props.load(new FileInputStream(startupPropertiesFile));
            } catch (Exception e) {
                // ignore
            }
            wsStatus.setProperties(props);
        }

        reader = ShellCommandReaderFactory.createCommandReader(args, factory, wsStatus);
        reader.open();

		displayWelcomeMessage();
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
					if (!success && reader.isBatch()) {
					    shutdown();
					}
				}
			} catch (InvalidCommandArgumentException e) {
			    outStream.println(msgIndentStr+Messages.getString(SHELL.INVALID_ARG, e.getMessage())); 
				if (command != null) {
				    outStream.println(msgIndentStr+Messages.getString(SHELL.USAGE)); 
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
            public void stateChanged() {
                updateLatch.countDown();
            }
        };
		defaultRepo.addObserver(stateObserver);

        displayMessage(Messages.getString(SHELL.ENGINE_STARTING));
        kEngine.start();
        displayMessage(SPACE + Messages.getString(SHELL.COMPONENT_STARTED)+ NEW_LINE);

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

        if (localRepoWaiting)
            displayMessage(SPACE + Messages.getString(SHELL.COMPONENT_STARTED));
        else
            displayMessage(SPACE + Messages.getString(SHELL.LOCAL_REPOSITORY_TIMEOUT_ERROR));

        displayMessage(NEW_LINE);
        displayMessage(NEW_LINE);
    }

	/**
	 * Shuts down the shell.
	 */
	public void shutdown() {
	    outStream.print(msgIndentStr + Messages.getString(SHELL.SHUTTING_DOWN));
        try {
            shutdown  = true;
            this.reader.close();
        } catch (IOException e) {
            // ignore
        }
        outStream.println(msgIndentStr+Messages.getString(SHELL.DONE));
	}

	private void displayMessage(String message) {
        outStream.print(message);
	}

	/**
	 * Displays a welcome message to the user.
	 */
	private void displayWelcomeMessage() {
	    displayMessage(
				"**********************************************************************\n" + //$NON-NLS-1$
				"  Welcome to Komodo Shell\n" + //$NON-NLS-1$
				"**********************************************************************\n" //$NON-NLS-1$
				);
	}
}
