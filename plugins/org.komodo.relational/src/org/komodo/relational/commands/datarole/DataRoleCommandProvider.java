/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.datarole;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.vdb.DataRole;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;

/**
 * A shell command provider for {@link DataRole}s.
 */
public class DataRoleCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for {@link DataRole} shell commands.
     */
    public DataRoleCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Map< String, Class< ? extends ShellCommand >> provideCommands() {
        final Map< String, Class< ? extends ShellCommand >> result = new HashMap<>();

        result.put( AddMappedRoleCommand.NAME, AddMappedRoleCommand.class );
        result.put( AddPermissionCommand.NAME, AddPermissionCommand.class );

        return result;
    }

}
