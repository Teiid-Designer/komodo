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
package org.komodo.shell.api;

import java.io.InputStream;
import java.io.Writer;
import org.komodo.core.KEngine;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;

/**
 * The KomodoShell interface
 */
public interface KomodoShell extends StringConstants {

    /**
     * @return the shell's komodo engine
     */
    KEngine getEngine();

    /**
     * @return the shell's input stream
     */
    InputStream getInputStream();

    /**
     * @return the shell's output writer
     */
    Writer getOutputWriter();

    /**
     * The directory where the shell saves user settings, preferences, or any other data needed to restore a user session.
     *
     * @return the shell's workspace directory (never empty)
     * @see SystemConstants#VDB_BUILDER_DATA_DIR
     */
    String getShellDataLocation();
    
    /**
     * Get the global shell properties file
     * @return the global shell properties file
     */
    String getShellPropertiesFile();
    
    /**
     * Must be called before the shell is running.
     *
     * @param dataDirectory
     *        the directory where the shell can persist preferences, settings, etc. (can be empty if the default location should
     *        be used)
     */
    void setShellDataLocation( final String dataDirectory );

    /**
     * Exit the shell
     */
    void exit();


}
