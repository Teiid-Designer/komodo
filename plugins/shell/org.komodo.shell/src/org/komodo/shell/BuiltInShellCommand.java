/*
 * Copyright 2013 JBoss Inc
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
package org.komodo.shell;

import org.komodo.shell.api.AbstractShellCommand;
import org.komodo.shell.Messages;

/**
 * Abstract base class for all built-in shell commands.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use different Messages class
 * 
 */
public abstract class BuiltInShellCommand extends AbstractShellCommand {

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage()
     */
    @Override
    public void printUsage() {
        print(Messages.getString(getClass().getSimpleName() + ".usage")); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp()
     */
    @Override
    public void printHelp() {
        print(Messages.getString(getClass().getSimpleName() + ".help")); //$NON-NLS-1$
    }

}
