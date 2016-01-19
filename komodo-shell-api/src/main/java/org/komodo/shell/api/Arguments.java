/*
 * Copyright 2012 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell.api;

import java.util.ArrayList;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A class that can parse the arguments that should be passed to a command.
 *
 * This class adapted from classes at https://github.com/Governance/s-ramp/blob/master/s-ramp-shell-api
 * - altered to use different Messages class
 *
 * @author eric.wittmann@redhat.com
 */
public class Arguments extends ArrayList<String> {

	private static final long serialVersionUID = 4475521615147664784L;

	private boolean partialLastArgumentAllowed = false;

    /**
     * Constructs empty arguments.
     */
    public Arguments() {
        // nothing to do
    }

	/**
     * @param arguments the command arguments (can be empty)
	 * @throws InvalidCommandArgumentException the exception
	 */
	public Arguments(String arguments) throws InvalidCommandArgumentException {
	    this(arguments, false);
	}

    /**
     * @param arguments the command arguments (can be empty)
     * @param partialLastArgumentAllowed is partial last arg allowed
     * @throws InvalidCommandArgumentException the exception
     */
    public Arguments(String arguments, boolean partialLastArgumentAllowed) throws InvalidCommandArgumentException {
        this.partialLastArgumentAllowed = partialLastArgumentAllowed;
        arguments = ( StringUtils.isBlank( arguments ) ? StringConstants.EMPTY_STRING : arguments );
        parseArguments(arguments);
    }

	/**
	 * Parses the arguments from the given string.
	 * @param arguments
	 */
	private void parseArguments(String arguments) throws InvalidCommandArgumentException {
		ScannerState state = ScannerState.scanningForStart;
		char quotChar = '\'';
		int startPos = -1;
		int endPos = -1;
		for (int position = 0; position < arguments.length(); position++) {
			char c = arguments.charAt(position);
			if (state == ScannerState.scanningForStart) {
				if (c == '\"' || c == '\'') {
					startPos = position;
					state = ScannerState.scanningForEndQuote;
					quotChar = c;
				} else if (!Character.isWhitespace(c)) {
					startPos = position;
					state = ScannerState.scanningForEnd;
				}
			} else if (state == ScannerState.scanningForEnd) {
				if (Character.isWhitespace(c)) {
					endPos = position;
					add(arguments.substring(startPos, endPos));
					state = ScannerState.scanningForStart;
				}
			} else if (state == ScannerState.scanningForEndQuote) {
				if (c == quotChar) {
					endPos = position;
					add(arguments.substring(startPos+1, endPos));
					state = ScannerState.scanningForStart;
				}
			}
		}
		if (state == ScannerState.scanningForEnd) {
			add(arguments.substring(startPos));
		} else if (state == ScannerState.scanningForEndQuote && partialLastArgumentAllowed) {
            add(arguments.substring(startPos+1));
		} else if (state == ScannerState.scanningForEndQuote && !partialLastArgumentAllowed) {
		    throw new InvalidCommandArgumentException(size(), I18n.bind(ShellApiI18n.invalidFinalArg));
		}
	}

	/**
	 * Remove and return the command name.
	 * @return the command name
	 */
	public String removeCommandName() {
		if (isEmpty())
			return null;

		String commandName = remove(0);

		return commandName;
	}

	private static enum ScannerState {
		scanningForStart,
		scanningForEnd,
		scanningForEndQuote
	}

}
