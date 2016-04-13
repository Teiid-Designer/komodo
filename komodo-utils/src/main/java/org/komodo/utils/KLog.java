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

import java.util.logging.Level;
import org.komodo.logging.ModeshapeKLogger;
import org.komodo.spi.logging.KLogger;

/**
 *
 */
public class KLog implements KLogger {

    private static KLog instance;

    /**
     * @return singleton instance of this logger
     */
    public static KLog getLogger() {
        if (instance == null)
            instance = new KLog();

        return instance;
    }

    private final KLogger kLogger;

    /**
     *
     */
    private KLog() {
        kLogger = new ModeshapeKLogger();
    }

    @Override
    public void dispose() {
        kLogger.dispose();
    }

    @Override
    public String getLogPath() throws Exception {
        return kLogger.getLogPath();
    }

    @Override
    public synchronized void setLogPath(String logPath) throws Exception {
        kLogger.setLogPath(logPath);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#setLevel(java.util.logging.Level)
     */
    @Override
    public synchronized void setLevel(Level level) throws Exception {
        kLogger.setLevel(level);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#info(java.lang.String, java.lang.Object[])
     */
    @Override
    public synchronized void info(String message, Object... args) {
        kLogger.info(message, args);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#info(java.lang.String, java.lang.Throwable, java.lang.Object[])
     */
    @Override
    public synchronized void info(String message, Throwable throwable, Object... args) {
        kLogger.info(message, throwable, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isInfoEnabled()
     */
    @Override
    public boolean isInfoEnabled() {
        return this.kLogger.isInfoEnabled();
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#warn(java.lang.String, java.lang.Object[])
     */
    @Override
    public synchronized void warn(String message, Object... args) {
        kLogger.warn(message, args);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#warn(java.lang.String, java.lang.Throwable, java.lang.Object[])
     */
    @Override
    public synchronized void warn(String message, Throwable throwable, Object... args) {
        kLogger.warn(message, throwable, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isWarnEnabled()
     */
    @Override
    public boolean isWarnEnabled() {
        return this.kLogger.isWarnEnabled();
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#error(java.lang.String, java.lang.Object[])
     */
    @Override
    public synchronized void error(String message, Object... args) {
        kLogger.error(message, args);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#error(java.lang.String, java.lang.Throwable, java.lang.Object[])
     */
    @Override
    public synchronized void error(String message, Throwable throwable, Object... args) {
        kLogger.error(message, throwable, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isErrorEnabled()
     */
    @Override
    public boolean isErrorEnabled() {
        return this.kLogger.isErrorEnabled();
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#debug(java.lang.String, java.lang.Object[])
     */
    @Override
    public synchronized void debug(String message, Object... args) {
        kLogger.debug(message, args);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#debug(java.lang.String, java.lang.Throwable, java.lang.Object[])
     */
    @Override
    public synchronized void debug(String message, Throwable throwable, Object... args) {
        kLogger.debug(message, throwable, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isDebugEnabled()
     */
    @Override
    public boolean isDebugEnabled() {
        return this.kLogger.isDebugEnabled();
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#trace(java.lang.String, java.lang.Object[])
     */
    @Override
    public synchronized void trace(String message, Object... args) {
        kLogger.trace(message, args);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.logging.KLogger#trace(java.lang.String, java.lang.Throwable, java.lang.Object[])
     */
    @Override
    public synchronized void trace(String message, Throwable throwable, Object... args) {
        kLogger.trace(message, throwable, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isTraceEnabled()
     */
    @Override
    public boolean isTraceEnabled() {
        return this.kLogger.isTraceEnabled();
    }

}
