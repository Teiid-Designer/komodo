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
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.slf4j.ILoggerFactory;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.joran.JoranConfigurator;
import ch.qos.logback.core.ConsoleAppender;
import ch.qos.logback.core.FileAppender;

/**
 *
 */
public class LogConfigurator {

    private static final String CONFIGURATION = "configuration"; //$NON-NLS-1$

    private static final String APPENDER = "appender"; //$NON-NLS-1$

    private static final String NAME = "name"; //$NON-NLS-1$

    private static final String FILE = "file"; //$NON-NLS-1$

    private static final String CLASS = "class"; //$NON-NLS-1$

    private static final String ENCODER = "encoder"; //$NON-NLS-1$

    private static final String PATTERN = "pattern"; //$NON-NLS-1$

    private static final String STDOUT = "STDOUT"; //$NON-NLS-1$

    private static final String ROOT = "ROOT"; //$NON-NLS-1$

    private static final String LEVEL = "level"; //$NON-NLS-1$

    private static final String APPENDER_REF = "appender-ref"; //$NON-NLS-1$

    private static final String REF = "ref"; //$NON-NLS-1$

    private String logPath;

    private String level = "INFO"; //$NON-NLS-1$

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

        ILoggerFactory factory = LoggerFactory.getILoggerFactory();
        if (factory instanceof LoggerContext) {
            LoggerContext context = (LoggerContext)LoggerFactory.getILoggerFactory();
            JoranConfigurator configurator = new JoranConfigurator();
            configurator.setContext(context);

            /*
             * Call context.reset() to clear any previous configuration, e.g. default configuration.
             */
            context.reset();

            File configFile = deriveConfigFile();
            configurator.doConfigure(configFile.getAbsolutePath());
        } else {
            try {
                String msg = "While trying to init KLog context, the instance of ILoggerFactory is NOT an instance of LoggerContext." + //$NON-NLS-1$
                                    " The class instance of ILoggerFactory is " + factory.getClass().getName(); //$NON-NLS-1$

                throw new UnsupportedOperationException(msg);
            } catch (RuntimeException ex) {
                System.err.println(ex);
            }
        }
    }

    /**
     * @return
     */
    private File deriveConfigFile() throws Exception {
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
        Document doc = docBuilder.newDocument();

        //<configuration>
        Element configuration = doc.createElement(CONFIGURATION);
        doc.appendChild(configuration);

        //<appender name="FILE" class="ch.qos.logback.core.FileAppender">
        Element appender1 = doc.createElement(APPENDER);
        configuration.appendChild(appender1);
        appender1.setAttribute(NAME, FILE.toUpperCase());
        appender1.setAttribute(CLASS, FileAppender.class.getName());

        //<file>${user.home}/.komodo/komodo.log</file>
        Element file = doc.createElement(FILE);
        appender1.appendChild(file);
        file.setTextContent(getLogPath());

        // <encoder>
        Element encoder1 = doc.createElement(ENCODER);
        appender1.appendChild(encoder1);

        // <pattern>%date %level [%thread] %logger{10} [%file:%line] %msg%n</pattern>
        Element pattern1 = doc.createElement(PATTERN);
        encoder1.appendChild(pattern1);
        pattern1.setTextContent("%date %level [%thread] %logger{10} [%file:%line] %msg%n"); //$NON-NLS-1$

        // <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
        Element appender2 = doc.createElement(APPENDER);
        configuration.appendChild(appender2);
        appender2.setAttribute(NAME, STDOUT);
        appender2.setAttribute(CLASS, ConsoleAppender.class.getName());

        // <encoder>
        Element encoder2 = doc.createElement(ENCODER);
        appender2.appendChild(encoder2);

        // <pattern>%msg%n</pattern>
        Element pattern2 = doc.createElement(PATTERN);
        encoder2.appendChild(pattern2);
        pattern2.setTextContent("%msg%n"); //$NON-NLS-1$

        // <root level="level defined by field">
        Element root = doc.createElement(ROOT);
        configuration.appendChild(root);
        root.setAttribute(LEVEL, level);

        // <appender-ref ref="STDOUT" />
        Element appendRef1 = doc.createElement(APPENDER_REF);
        root.appendChild(appendRef1);
        appendRef1.setAttribute(REF, STDOUT);

        //<appender-ref ref="FILE" />
        Element appendRef2 = doc.createElement(APPENDER_REF);
        root.appendChild(appendRef2);
        appendRef2.setAttribute(REF, FILE.toUpperCase());

        // write the content into xml file
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        DOMSource source = new DOMSource(doc);

        File tmpFile = File.createTempFile("Logger", ".xml");  //$NON-NLS-1$//$NON-NLS-2$
        tmpFile.deleteOnExit();
        StreamResult result = new StreamResult(tmpFile);
        transformer.transform(source, result);

        return tmpFile;
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
        this.logPath = logPath;
        initContext();
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
            this.level = "ERROR"; //$NON-NLS-1$
        else if (Level.WARNING.equals(level))
            this.level = "WARN"; //$NON-NLS-1$
        else if (Level.FINE.equals(level))
            this.level = "DEBUG"; //$NON-NLS-1$
        else if (Level.FINER.equals(level) || Level.FINEST.equals(level))
            this.level = "TRACE"; //$NON-NLS-1$
        else
            this.level = "INFO"; //$NON-NLS-1$

        initContext();
    }

    /**
     * Dispose of this configurator
     */
    public void dispose() {
        LoggerContext loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();
        loggerContext.stop();
    }
}
