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
package org.komodo.modeshape.lib;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;

/**
 *
 */
public class LogConfigurator {

    private static final String CONSOLE_APPENDER = "KLOG-CONSOLE"; //$NON-NLS-1$

    private static final String FILE_APPENDER = "KLOG-FILE"; //$NON-NLS-1$

    private String logPath;

    private String level = "INFO"; //$NON-NLS-1$

    private RollingFileAppender fileAppender;

    private static LogConfigurator instance;

    /**
     * @return singleton instance
     * @throws Exception exception if singleton fails
     */
    public static LogConfigurator getInstance() throws Exception {
        if (instance == null) {
            instance = new LogConfigurator();
            instance.initContext();
        }

        return instance;
    }

    private void initContext() throws Exception {
        // get default log file path if necessary
        if ( ( this.logPath == null ) || this.logPath.isEmpty() ) {
            String tempPath = System.getProperty( "komodo.dataDir" ); //$NON-NLS-1$
            if (tempPath == null) {
                tempPath = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
            }
            tempPath += File.separator + "komodo.log"; //$NON-NLS-1$
            this.logPath = tempPath;
        }

        // make sure log file exists
        final Path logFilePath = Paths.get( this.logPath );

        if ( !Files.exists( logFilePath ) ) {
            if ( !Files.exists( logFilePath.getParent() ) ) {
                Files.createDirectories( logFilePath.getParent() );
            }

            Files.createFile( logFilePath );
        }

        //This is the root logger provided by log4j
        Logger rootLogger = Logger.getRootLogger();
        rootLogger.setLevel(org.apache.log4j.Level.toLevel(this.level));

        if (rootLogger.getAppender(CONSOLE_APPENDER) == null) {
            //Add console appender to root logger
            PatternLayout layout = new PatternLayout("%d{ISO8601} [%t] %-5p %c %x - %m%n"); //$NON-NLS-1$
            ConsoleAppender consoleAppender = new ConsoleAppender(layout);
            consoleAppender.setName(CONSOLE_APPENDER);
            rootLogger.addAppender(consoleAppender);
        }

        if (rootLogger.getAppender(FILE_APPENDER) == null) {
            //Add console appender to root logger
            PatternLayout layout = new PatternLayout("%d{ISO8601} [%t] %-5p %c %x - %m%n"); //$NON-NLS-1$
            fileAppender = new RollingFileAppender(layout, this.logPath);
            fileAppender.setName(FILE_APPENDER);
            rootLogger.addAppender(fileAppender);
        }
    }

    /**
     * @return the logPath
     */
    public String getLogPath() {
        return this.logPath;
    }

    /**
     * @param logPath the logPath to set
     * @throws Exception exception
     */
    public void setLogPath(String logPath) throws Exception {
        Logger rootLogger = Logger.getRootLogger();

        if (fileAppender != null) {
            fileAppender.close();
            rootLogger.removeAppender(fileAppender);
            fileAppender = null;
        }

        if (this.logPath != null) {
            // Tidy up the old log path if it exists and is empty
            File oldLog = new File(this.logPath);
            if (oldLog.canRead() && oldLog.length() == 0L) {
                Files.deleteIfExists(oldLog.toPath());
            }
        }

        String logPathMsg = "Location of old log file was " + this.logPath; //$NON-NLS-1$

        this.logPath = logPath;
        initContext();

        // Log the location of the old log file in case anyone ever needs it
        rootLogger.debug(logPathMsg);
    }

    /**
     * Set the level of the logging configuration. Levels are not completely identical
     * to Modeshape logging levels so method approximates and converts accordingly.
     *
     * @param level level for logging
     * @throws Exception exception
     */
    public void setLevel(Level level) throws Exception {
        if (Level.OFF.equals(level) || Level.ALL.equals(level) || Level.INFO.equals(level))
            this.level = level.getName();
        else if (Level.SEVERE.equals(level))
            this.level = org.apache.log4j.Level.FATAL.toString();
        else if (Level.WARNING.equals(level))
            this.level = org.apache.log4j.Level.WARN.toString();
        else if (Level.FINE.equals(level))
            this.level = org.apache.log4j.Level.DEBUG.toString();
        else if (Level.FINER.equals(level) || Level.FINEST.equals(level))
            this.level = org.apache.log4j.Level.TRACE.toString();
        else
            this.level = org.apache.log4j.Level.INFO.toString();

        initContext();
    }

    /**
     * Dispose of this configurator
     */
    public void dispose() {
        // Nothing to do
    }
}
