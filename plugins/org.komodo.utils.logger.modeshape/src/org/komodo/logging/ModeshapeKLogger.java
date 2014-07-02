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


import org.komodo.spi.logging.KLogger;
import org.modeshape.common.i18n.TextI18n;
import org.modeshape.common.logging.Logger;

/**
 *
 */
public class ModeshapeKLogger implements KLogger {

    private static final Logger logger = Logger.getLogger(KLogger.class);

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
        logger.info(ti18n, args);
    }

    @Override
    public void info(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        logger.info(throwable, ti18n, args);
    }

    @Override
    public void warn(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        logger.warn(ti18n, args);
    }

    @Override
    public void warn(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        logger.warn(throwable, ti18n, args);
    }

    @Override
    public void error(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        logger.error(ti18n, args);
    }

    @Override
    public void error(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        logger.error(throwable, ti18n, args);
    }

    @Override
    public void debug(String message, Object... args) {
        logger.debug(message, args);
    }

    @Override
    public void debug(String message, Throwable throwable, Object... args) {
        logger.debug(throwable, message, args);
    }

    @Override
    public void trace(String message, Object... args) {
        logger.trace(message, args);
    }

    @Override
    public void trace(String message, Throwable throwable, Object... args) {
        logger.trace(throwable, message, args);
    }
}
