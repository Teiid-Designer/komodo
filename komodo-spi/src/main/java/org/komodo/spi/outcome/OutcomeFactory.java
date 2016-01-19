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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.komodo.spi.Messages;
import org.komodo.spi.outcome.Outcome.Level;

/**
 *
 */
public class OutcomeFactory {

    /**
     * An implementation of an {@link Outcome outcome}.
     */
    public static class OutcomeImpl implements Outcome {

    	private List<Outcome> children;

        private String message;

        private Exception exception;

        private Outcome.Level level;

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
        public Outcome.Level getLevel() {
            return this.level;
        }

        /**
         * @param level the level to set
         */
        public void setLevel(Outcome.Level level) {
            this.level = level;
        }

        /* (non-Javadoc)
         * @see org.komodo.spi.outcome.IOutcome#isOK()
         */
        @Override
        public boolean isOK() {
            return Level.OK == level;
        }

		/* (non-Javadoc)
		 * @see org.komodo.spi.outcome.IOutcome#getOutcomes()
		 */
		@Override
		public List<Outcome> getOutcomes() {
			if (this.children == null)
				return Collections.emptyList();

			return this.children;
		}

		/* (non-Javadoc)
		 * @see org.komodo.spi.outcome.IOutcome#addOutcome(org.komodo.spi.outcome.IOutcome)
		 */
		@Override
		public void addOutcome(Outcome outcome) {
			if(this.children==null) this.children=new ArrayList<Outcome>();
			if(outcome.isMultiOutcome()) {
				addOutcomes(outcome.getOutcomes());
			} else {
				this.children.add(outcome);
				if(this.children.size()==1) {
					this.setMessage(outcome.getMessage());
				}
			}
    		// Set level to highest of the children
    		Level addedLevel = outcome.getLevel();
    		if(isGreaterThanCurrent(addedLevel)) {
    			setLevel(addedLevel);
    		}
		}

		/* (non-Javadoc)
		 * @see org.komodo.spi.outcome.IOutcome#addOutcomes(java.util.List)
		 */
		@Override
		public void addOutcomes(List<Outcome> outcomes) {
    		for(Outcome outcome : outcomes) {
    			addOutcome(outcome);
    		}
		}

		/* (non-Javadoc)
		 * @see org.komodo.spi.outcome.IOutcome#isMultiOutcome()
		 */
		@Override
		public boolean isMultiOutcome() {
			return (this.children!=null && !this.children.isEmpty()) ? true : false;
		}

    	private boolean isGreaterThanCurrent(Level level) {
    		Level currentLevel = getLevel();
    		boolean isGreater = false;
    		switch (level) {
    		case OK:
    			isGreater=false;
    			break;
    		case INFO:
    			if(currentLevel==Level.OK) isGreater=true;
    			break;
    		case WARNING:
    			if(currentLevel==Level.OK || currentLevel==Level.INFO) isGreater=true;
    			break;
    		case ERROR:
    			if(currentLevel==Level.OK || currentLevel==Level.INFO || currentLevel==Level.ERROR) isGreater=true;
    			break;
    		default:
    		}
    		return isGreater;
    	}

    	/* (non-Javadoc)
    	 * @see java.lang.Object#toString()
    	 */
    	@Override
    	public String toString() {
    		StringBuilder sb = new StringBuilder();
    		sb.append(this.getClass().getName());
    		if(!isMultiOutcome()) {
        		sb.append("\n\t").append("isMulti = ").append(isMultiOutcome()); //$NON-NLS-1$ //$NON-NLS-2$
    			sb.append("\n\t").append("Level   = ").append(getLevel()); //$NON-NLS-1$  //$NON-NLS-2$
    			sb.append("\n\t").append("Msg     = ").append(getMessage()); //$NON-NLS-1$  //$NON-NLS-2$
    		} else {
        		sb.append("\n\t").append("isMulti = ").append(isMultiOutcome()); //$NON-NLS-1$ //$NON-NLS-2$
        		sb.append(" (").append(getOutcomes().size()).append(" outcomes)"); //$NON-NLS-1$ //$NON-NLS-2$
    			sb.append("\n\t").append("Max Level = ").append(getLevel()); //$NON-NLS-1$  //$NON-NLS-2$
    			sb.append("\n\t").append("Msg = ").append(getMessage()); //$NON-NLS-1$  //$NON-NLS-2$
    		}
    		return sb.toString();
    	}
    }

    private static OutcomeFactory instance;

    public static OutcomeFactory getInstance() {
        if (instance == null)
            instance = new OutcomeFactory();

        return instance;
    }

    public Outcome createMultiOutcome(String message, List<Outcome> outcomes) {
    	OutcomeImpl multiOutcome = new OutcomeImpl();
    	multiOutcome.setMessage(message);
    	multiOutcome.setLevel(Level.OK);
    	multiOutcome.addOutcomes(outcomes);
    	return multiOutcome;
    }

    /**
     * @return default ok outcome
     */
    public Outcome createOK() {
        OutcomeImpl outcome = new OutcomeImpl();
        outcome.setMessage(Messages.getString(Messages.OutcomeFactory.OK));
        outcome.setLevel(Level.OK);
        return outcome;
    }

    /**
     * @return default ok outcome
     */
    public Outcome createOK(String msg) {
        OutcomeImpl outcome = new OutcomeImpl();
        outcome.setMessage(msg);
        outcome.setLevel(Level.OK);
        return outcome;
    }

    /**
     * @param msg
     * @return error outcome containing message
     */
    public Outcome createError(String msg) {
        OutcomeImpl outcome = new OutcomeImpl();
        outcome.setMessage(msg);
        outcome.setLevel(Level.ERROR);
        return outcome;
    }

    /**
     * @param msg
     * @param ex
     * @return error outcome containing message and exception
     */
    public Outcome createError(String msg, Exception ex) {
        OutcomeImpl outcome = new OutcomeImpl();
        outcome.setMessage(msg);
        outcome.setException(ex);
        outcome.setLevel(Level.ERROR);
        return outcome;
    }

    /**
     * @param msg
     * @return warning outcome containing message and exception
     */
    public Outcome createWarning(String msg) {
        OutcomeImpl outcome = new OutcomeImpl();
        outcome.setMessage(msg);
        outcome.setLevel(Level.WARNING);
        return outcome;
    }

    /**
     * @param msg
     * @param ex
     * @return information outcome containing message and exception
     */
    public Outcome createInformation(String msg, Exception ex) {
        OutcomeImpl outcome = new OutcomeImpl();
        outcome.setMessage(msg);
        outcome.setException(ex);
        outcome.setLevel(Level.INFO);
        return outcome;
    }
}
