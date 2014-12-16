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
package org.komodo.logging;


import java.util.logging.Level;
import org.komodo.modeshape.lib.LogConfigurator;
import org.komodo.spi.logging.KLogger;
import org.modeshape.common.i18n.TextI18n;
import org.modeshape.common.logging.Logger;

/**
 *
 */
public class ModeshapeKLogger implements KLogger {

    private Logger logger;

    @Override
    public void dispose() {
        try {
            LogConfigurator.getInstance().dispose();
        } catch (Exception ex) {
            error("", ex); //$NON-NLS-1$
        }
        logger = null;
    }

    @Override
    public String getLogPath() throws Exception {
        return LogConfigurator.getInstance().getLogPath();
    }

    @Override
    public void setLogPath(String logPath) throws Exception {
        LogConfigurator.getInstance().setLogPath(logPath);
    }

    @Override
    public void setLevel(Level level) throws Exception {
        LogConfigurator.getInstance().setLevel(level);
    }

    private Logger getLogger() {
        if (logger == null)
            logger = Logger.getLogger(KLogger.class);

        return logger;
    }

    /**
     * @param message
     * @return
     */
    private TextI18n getI18n(String message) {
        TextI18n ti18n = new TextI18n(message);
        return ti18n;
    }

    @Override
    public void info(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().info(ti18n, args);
    }

    @Override
    public void info(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().info(throwable, ti18n, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isInfoEnabled()
     */
    @Override
    public boolean isInfoEnabled() {
        return getLogger().isInfoEnabled();
    }

    @Override
    public void warn(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().warn(ti18n, args);
    }

    @Override
    public void warn(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().warn(throwable, ti18n, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isWarnEnabled()
     */
    @Override
    public boolean isWarnEnabled() {
        return isWarnEnabled();
    }

    @Override
    public void error(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().error(ti18n, args);
    }

    @Override
    public void error(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().error(throwable, ti18n, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isErrorEnabled()
     */
    @Override
    public boolean isErrorEnabled() {
        return getLogger().isErrorEnabled();
    }

    @Override
    public void debug(String message, Object... args) {
        getLogger().debug(message, args);
    }

    @Override
    public void debug(String message, Throwable throwable, Object... args) {
        getLogger().debug(throwable, message, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isDebugEnabled()
     */
    @Override
    public boolean isDebugEnabled() {
        return getLogger().isDebugEnabled();
    }

    @Override
    public void trace(String message, Object... args) {
        getLogger().trace(message, args);
    }

    @Override
    public void trace(String message, Throwable throwable, Object... args) {
        getLogger().trace(throwable, message, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isTraceEnabled()
     */
    @Override
    public boolean isTraceEnabled() {
        return getLogger().isTraceEnabled();
    }

}
