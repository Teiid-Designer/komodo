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
package org.komodo.spi.outcome;

import org.komodo.spi.Messages;
import org.komodo.spi.outcome.IOutcome.Level;

/**
 *
 */
public class OutcomeFactory {

    private static class Outcome implements IOutcome {

        private String message;

        private Exception exception;

        private IOutcome.Level level;

        /**
         * @return the message
         */
        @Override
        public String getMessage() {
            return this.message;
        }

        /**
         * @param message the message to set
         */
        public void setMessage(String message) {
            this.message = message;
        }

        /**
         * @return the exception
         */
        @Override
        public Exception getException() {
            return this.exception;
        }

        /**
         * @param exception the exception to set
         */
        public void setException(Exception exception) {
            this.exception = exception;
        }

        /**
         * @return the level
         */
        @Override
        public IOutcome.Level getLevel() {
            return this.level;
        }

        /**
         * @param level the level to set
         */
        public void setLevel(IOutcome.Level level) {
            this.level = level;
        }

        /* (non-Javadoc)
         * @see org.komodo.spi.outcome.IOutcome#isOK()
         */
        @Override
        public boolean isOK() {
            return Level.OK == level;
        }
    }

    private static OutcomeFactory instance;

    public static OutcomeFactory getInstance() {
        if (instance == null)
            instance = new OutcomeFactory();

        return instance;
    }

    /**
     * @return default ok outcome
     */
    public IOutcome createOK() {
        Outcome outcome = new Outcome();
        outcome.setMessage(Messages.getString(Messages.OutcomeFactory.OK));
        outcome.setLevel(Level.OK);
        return outcome;
    }

    /**
     * @param msg
     * @return error outcome containing message
     */
    public IOutcome createError(String msg) {
        Outcome outcome = new Outcome();
        outcome.setMessage(msg);
        outcome.setLevel(Level.ERROR);
        return outcome;
    }

    /**
     * @param msg
     * @param ex
     * @return error outcome containing message and exception
     */
    public IOutcome createError(String msg, Exception ex) {
        Outcome outcome = new Outcome();
        outcome.setMessage(msg);
        outcome.setException(ex);
        outcome.setLevel(Level.ERROR);
        return outcome;
    }

    /**
     * @param msg
     * @return warning outcome containing message and exception
     */
    public IOutcome createWarning(String msg) {
        Outcome outcome = new Outcome();
        outcome.setMessage(msg);
        outcome.setLevel(Level.WARNING);
        return outcome;
    }

    /**
     * @param msg
     * @param ex
     * @return information outcome containing message and exception
     */
    public IOutcome createInformation(String msg, Exception ex) {
        Outcome outcome = new Outcome();
        outcome.setMessage(msg);
        outcome.setException(ex);
        outcome.setLevel(Level.INFO);
        return outcome;
    }
}
