/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.eclipse.shell.ui;

import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.part.IPageBookViewPage;
import org.komodo.core.KEngine;
import org.komodo.shell.KomodoShell;
import org.osgi.framework.Bundle;

/**
 *
 */
public class KomodoConsole extends IOConsole {

    private static final String PLUGIN_ID = "org.komodo.shell"; //$NON-NLS-1$

    private static final String DRAGON_ICON_PATH = "icons/32x32/dragon.png"; //$NON-NLS-1$

    private class KomodoShellThread extends Thread {

        private final PrintStream outStream;

        private KomodoShell shell;

        public KomodoShellThread(InputStream inStream, PrintStream outStream) {
            this.setDaemon(true);
            this.outStream = outStream;
            shell = new KomodoShell(KEngine.getInstance(), inStream, outStream);
        }

        @Override
        public void run() {
            try {
                shell.run(new String[0]);
            } catch (Exception ex) {
                if (! outStream.checkError())
                    outStream.print(ex.getLocalizedMessage());

                KEngine.getInstance().getErrorHandler().error(ex);
            }
        }

        public void dispose() {
            if (shell != null)
                shell.shutdown();
        }
    }

    private KomodoShellThread shellThread;

    private PrintStream outStream = null;

    /**
     *
     */
    public KomodoConsole() {
        super(Messages.consoleName, initImageDescriptor());

        IOConsoleOutputStream newOutputStream = newOutputStream();
        outStream  = new PrintStream(newOutputStream);

        shellThread = new KomodoShellThread(getInputStream(), outStream);
        shellThread.start();
    }

    private static ImageDescriptor initImageDescriptor() {
        Bundle bundle = Platform.getBundle(PLUGIN_ID);
        IPath path = new Path(DRAGON_ICON_PATH);
        URL url = FileLocator.find(bundle, path, null);
        return ImageDescriptor.createFromURL(url);
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.console.IConsole#createPage(org.eclipse.ui.console.IConsoleView)
     */
    @Override
    public IPageBookViewPage createPage(IConsoleView view) {
        return new KomodoConsolePage(this, view);
    }

    @Override
    public void dispose() {
        if (shellThread != null) {
            shellThread.dispose();
            shellThread = null;
        }

        if (outStream != null) {
            outStream.close();
            outStream = null;
        }

        super.dispose();
    }
}
