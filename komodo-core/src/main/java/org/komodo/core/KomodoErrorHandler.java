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
package org.komodo.core;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.komodo.spi.KErrorHandler;
import org.komodo.utils.KLog;

/**
 * Composite error handler implemented to delegate errors and warnings
 * to multiple {@link KErrorHandler} implementations as well as logging
 * the message to {@link KLog}.
 */
public class KomodoErrorHandler implements KErrorHandler {

    private Set<KErrorHandler> errorHandlers = Collections.emptySet();

    /**
     * @param errorHandler the error handler
     */
    public void add(KErrorHandler errorHandler) {
        if (! (errorHandlers instanceof HashSet))
            errorHandlers = new HashSet<KErrorHandler>();

        errorHandlers.add(errorHandler);
    }

    /**
     * @param errorHandler the error handler
     */
    public void remove(KErrorHandler errorHandler) {
        if (errorHandlers.isEmpty())
            return;

        errorHandlers.remove(errorHandler);
    }

    @Override
    public void warn(String message) {
        KLog.getLogger().warn(message);

        for (KErrorHandler handler : errorHandlers) {
            handler.warn(message);
        }
    }

    @Override
    public void error(String message) {
        KLog.getLogger().error(message);

        for (KErrorHandler handler : errorHandlers) {
            handler.error(message);
        }
    }

    @Override
    public void error(Throwable ex) {
        KLog.getLogger().error(ex == null ? "<no message>" : ex.getLocalizedMessage(), ex); //$NON-NLS-1$

        for (KErrorHandler handler : errorHandlers) {
            handler.error(ex);
        }
    }

    @Override
    public void error(String message, Throwable ex) {
        KLog.getLogger().error(message, ex);

        for (KErrorHandler handler : errorHandlers) {
            handler.error(message, ex);
        }
    }
}
