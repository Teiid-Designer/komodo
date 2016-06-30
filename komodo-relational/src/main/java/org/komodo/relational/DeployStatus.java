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
package org.komodo.relational;

import java.util.ArrayList;
import java.util.List;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

public class DeployStatus implements StringConstants {

    // Progress Messages
    private List<String> progressMessages = new ArrayList<String>();

    // Error Messages
    private List<String> errorMessages = new ArrayList<String>();

    public boolean ok() {
        return errorMessages.isEmpty();
    }

    /**
     * Add an error message based on the supplied exception.
     * @param exception the exception
     */
    public void addErrorMessage(Throwable exception) {
        ArgCheck.isNotNull(exception, "error"); //$NON-NLS-1$

        while (exception.getCause() != null) {
            //
            // Zero down to the root cause of the exception
            //
            exception = exception.getCause();
        }

        String message = exception.getLocalizedMessage();
        if (message == null || message.isEmpty())
            message = StringUtils.exceptionToString(exception);

        errorMessages.add(message);
    }

    /**
     * Add an error message
     * @param message the error message
     */
    public void addErrorMessage(String message) {
        ArgCheck.isNotNull(message, "error message"); //$NON-NLS-1$

        if (errorMessages == null) {
            errorMessages = new ArrayList<String>();
        }
        errorMessages.add(message);
    }

    /**
     * Get the error messages
     * @return messages
     */
    public List<String> getErrorMessages() {
        if (errorMessages == null) {
            errorMessages = new ArrayList<String>();
        }

        return errorMessages;
    }

    /**
     * Get the error messages
     * @return error messages
     */
    public String errorMessagesToString() {
        if (errorMessages == null)
            return EMPTY_STRING;

        StringBuffer errorMsgs = new StringBuffer();
        for (String errorMsg : errorMessages) {
            errorMsgs.append(errorMsg);
            errorMsgs.append(NEW_LINE);
        }

        return errorMsgs.toString();
    }

    /**
     * Add an progress message
     * @param message the progress message
     */
    public void addProgressMessage(String message) {
        ArgCheck.isNotNull(message, "progress message"); //$NON-NLS-1$

        if (progressMessages == null) {
            progressMessages = new ArrayList<String>();
        }

        progressMessages.add(message);
    }

    public List<String> getProgressMessages() {
        if (progressMessages == null) {
            progressMessages = new ArrayList<String>();
        }

        return progressMessages;
    }
}
