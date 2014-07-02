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
package org.komodo.utils.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.OutputStream;
import java.io.PrintStream;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.utils.KLog;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestKLog {

    public class TestOutputStream extends OutputStream {

        private final String testMessage;

        private Exception testException;

        private StringBuilder receivedMessage = new StringBuilder();

        public TestOutputStream(String msg) {
            this.testMessage = msg;
        }

        /**
         * @param msg
         * @param testException
         */
        public TestOutputStream(String msg, Exception testException) {
            this(msg);
            this.testException = testException;
        }

        @Override
        public void write(int b) {
            receivedMessage.append(String.valueOf((char)b));
        }

        public void testMessageEquivalence() {
            assertTrue(receivedMessage.toString().contains(testMessage));
            if (testException != null)
                assertTrue(receivedMessage.toString().contains(testException.getLocalizedMessage()));
        }
    }

    private static PrintStream originalOut;

    private static PrintStream originalErr;

    @BeforeClass
    public static void setupClass() {
        originalOut = System.out;
        originalErr = System.err;
    }

    @AfterClass
    public static void cleanupClass() {
        System.setOut(originalOut);
        System.setErr(originalErr);
    }

    @Test
    public void testLogInit() {
        try {
            KLog logger = KLog.getLogger();
            assertNotNull(logger);
        } catch (Throwable throwable) {
            throwable.printStackTrace();
            fail("Should not throw an exception " + throwable.getLocalizedMessage());
        }
    }

    @Test
    public void testLogInfo() throws Exception {
        KLog logger = KLog.getLogger();
        assertNotNull(logger);
        String msg = "This is a test";

        TestOutputStream testOut = new TestOutputStream(msg);
        System.setOut(new PrintStream(testOut));
        logger.info(msg);
        testOut.testMessageEquivalence();
    }

    @Test
    public void testLogWarning() throws Exception {
        KLog logger = KLog.getLogger();
        assertNotNull(logger);
        String msg = "This is a {0} test";
        String param1 = "warning";

        TestOutputStream testOut = new TestOutputStream(msg.replace("{0}", param1));
        System.setOut(new PrintStream(testOut));
        logger.warn(msg, param1);
        testOut.testMessageEquivalence();
    }

    @Test
    public void testLogError() throws Exception {
        KLog logger = KLog.getLogger();
        assertNotNull(logger);
        String msg = "This is a exception test";
        Exception testException = new Exception("This is a test exception");

        TestOutputStream testOut = new TestOutputStream(msg, testException);
        System.setOut(new PrintStream(testOut));
        logger.error(msg, testException);
        testOut.testMessageEquivalence();
    }

}
