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

import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleFactory;
import org.eclipse.ui.console.IConsoleManager;

public class KomodoConsoleFactory implements IConsoleFactory {

    private static KomodoConsole console;

    @Override
    public void openConsole() {
        KomodoConsole console = getConsole();

        if (console != null) {
            IConsoleManager manager = ConsolePlugin.getDefault().getConsoleManager();
            IConsole[] existing = manager.getConsoles();
            boolean exists = false;
            for (int i = 0; i < existing.length; i++) {
                if (console == existing[i])
                    exists = true;
            }
            if (!exists)
                manager.addConsoles(new IConsole[] {console});

            manager.showConsoleView(console);
        }
    }

    public static KomodoConsole getConsole() {
        if (console == null) {
            console = new KomodoConsole();
        }
        return console;
    }

}
