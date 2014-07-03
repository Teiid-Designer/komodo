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

import java.io.IOException;
import java.util.Locale;

import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;


/**
 * An interactive shell for working with komodo.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 * - altered display message
 * - utilizing different Messages class
 * 
 */
public class KomodoShell {

    private static final String LOCALE_PROPERTY = "komodo.shell.locale"; //$NON-NLS-1$

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

		final KomodoShell shell = new KomodoShell();
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
			e.printStackTrace(System.err);
			System.out.println(Messages.getString(SHELL.EXITING));
		}
	}

	private WorkspaceStatus wsStatus = new TestWorkspaceStatus();
	private final ShellCommandFactory factory = new ShellCommandFactory(wsStatus);
	private ShellCommandReader reader;

	/**
	 * Constructor.
	 */
	public KomodoShell() {
	}

	/**
	 * Runs the shell.
	 * @param args the arguments
	 * @throws Exception the exception
	 */
	public void run(String[] args) throws Exception {
        reader = ShellCommandReaderFactory.createCommandReader(args, factory, wsStatus);
        reader.open();
		displayWelcomeMessage();
		boolean done = false;
		while (!done) {
			ShellCommand command = null;
			try {
	            command = reader.read();
				if (command == null) {
					done = true;
				} else {
					boolean success = command.execute();
					if (!success && reader.isBatch()) {
					    System.exit(1);
					}
				}
			} catch (InvalidCommandArgumentException e) {
				System.out.println(Messages.getString(SHELL.INVALID_ARG, e.getMessage())); 
				if (command != null) {
    				System.out.println(Messages.getString(SHELL.USAGE)); 
    				command.printUsage();
				}
				if (reader.isBatch())
				    System.exit(1);
			} catch (Exception e) {
				e.printStackTrace(System.err);
				if (reader.isBatch())
				    System.exit(1);
			}
		}
	}

	/**
	 * Shuts down the shell.
	 */
	public void shutdown() {
		System.out.print(Messages.getString(SHELL.SHUTTING_DOWN));
		try { this.reader.close(); } catch (IOException e) { }
		System.out.println(Messages.getString(SHELL.DONE));
	}

	/**
	 * Displays a welcome message to the user.
	 */
	private void displayWelcomeMessage() {
		System.out.println(
				"**********************************************************************\n" + //$NON-NLS-1$
				"  Welcome to Komodo Shell\n" + //$NON-NLS-1$
				"  Locale: " + Locale.getDefault().toString().trim() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
				"**********************************************************************" //$NON-NLS-1$
				);
	}
}
