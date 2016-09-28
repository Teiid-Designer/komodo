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
package org.komodo.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.logging.KLogger;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestKLog {

    private static Path _dataDirectory;

    @BeforeClass
    public static void initDataDirectory() throws Exception {
        // create data directory for engine logging
        _dataDirectory = Files.createTempDirectory( "KomodoEngineDataDir" );
        System.setProperty( SystemConstants.ENGINE_DATA_DIR, _dataDirectory.toString() );
    }

    @AfterClass
    public static void removeDataDirectory() throws Exception {
        FileUtils.removeDirectoryAndChildren( _dataDirectory.toFile() );
    }

    private KLog logger;

    @Before
    public void setup() {
        logger = KLog.getLogger();
    }

    @After
    public void cleanup() {
        logger.dispose();
    }

    @Test
    public void testLogInit() {
        try {
            logger = KLog.getLogger();
            assertNotNull(logger);
        } catch (Throwable throwable) {
            fail("Should not throw an exception " + throwable.getLocalizedMessage());
        }
    }

    private File configureLogPath(KLog logger) throws IOException, Exception {
        File newLogFile = File.createTempFile("TestKLog", ".log");
        newLogFile.deleteOnExit();

        logger.setLogPath(newLogFile.getAbsolutePath());
        assertEquals(newLogFile.getAbsolutePath(), logger.getLogPath());
        return newLogFile;
    }

    private String retrieveLogContents(File newLogFile) throws Exception {
        try ( BufferedReader reader = new BufferedReader( new FileReader( newLogFile ) ) ) {
            StringBuilder builder = new StringBuilder();
            String line;
            while((line = reader.readLine()) != null) {
                builder.append(line);
                builder.append("\n");
            }

            return builder.toString();
        }
    }

    @Test
    public void testSetLogPath() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String logMsg = "Test Log Message";
        logger.info(logMsg);

        String fileMsg = retrieveLogContents(newLogFile);
        assertTrue(fileMsg.contains(logMsg));
    }

    @Test
    public void testLogInfo() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String msg = "This is a test";
        logger.info(msg);

        String fileMsg = retrieveLogContents(newLogFile);
        System.out.println("The File Message: " + fileMsg);
        assertTrue(fileMsg.contains("INFO  " + KLogger.class.getName() + "  - " + msg));
    }

    @Test
    public void testLogWarning() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String msg = "This is a {0} test";
        String param1 = "warning";

        logger.warn(msg, param1);

        String fileMsg = retrieveLogContents(newLogFile);
        System.out.println("The File Message: " + fileMsg);
        assertTrue(fileMsg.contains("WARN  " + KLogger.class.getName() + "  - " + msg.replace("{0}", param1)));
    }

    @Test
    public void testLogError() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String msg = "This is a exception test";
        Exception testException = new Exception("This is a test exception");
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        testException.printStackTrace(pw);

        logger.error(msg, testException);

        String fileMsg = retrieveLogContents(newLogFile);
        System.out.println("The File Message: " + fileMsg);
        assertTrue(fileMsg.contains("ERROR " + KLogger.class.getName() + "  - " + msg));
        assertTrue(fileMsg.contains(msg));
    }

}
