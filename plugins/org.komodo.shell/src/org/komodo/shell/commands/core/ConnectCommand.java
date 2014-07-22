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
package org.komodo.shell.commands.core;

import java.util.HashMap;
import java.util.Map;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.IShellTeiidParent;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.ITeiidInstance;
import org.komodo.spi.runtime.ITeiidJdbcInfo;

/**
 *
 */
public class ConnectCommand extends BuiltInShellCommand implements StringConstants {

    private static final String CONNECT = "connect"; //$NON-NLS-1$

    private enum ArgSwitch {
        HOST("h"), //$NON-NLS-1$
        ADMIN_PORT("o"), //$NON-NLS-1$
        ADMIN_USER("u"), //$NON-NLS-1$
        ADMIN_PASSWD("p"), //$NON-NLS-1$
        JDBC_PORT("jo"), //$NON-NLS-1$
        JDBC_USER("ju"), //$NON-NLS-1$
        JDBC_PASSWD("jp"); //$NON-NLS-1$

        private String symbol;

        private ArgSwitch(String symbol) {
            this.symbol = symbol;
        }

        /**
         * @return the symbol
         */
        public String getSymbol() {
            return this.symbol;
        }

        /**
         * @return the switch, eg. -p
         */
        public String getSwitch() {
            return HYPHEN + getSymbol();
        }

        /**
         * @param aSwitch
         * @return ArgSwitch representing the given switch
         */
        public static ArgSwitch find(String aSwitch) {
            for (ArgSwitch theSwitch : ArgSwitch.values()) {
                if (theSwitch.getSwitch().equals(aSwitch))
                    return theSwitch;
            }

            return null;
        }
    }

    private Map<ArgSwitch, String> argMap = new HashMap<ArgSwitch, String>();

    /**
     *
     */
    public ConnectCommand(WorkspaceStatus wsStatus) {
        super(CONNECT, wsStatus);
    }

    @Override
    public boolean execute() throws Exception {

        Arguments args = getArguments();
        if (!this.validate(args)) {
            printHelp(0);
            printUsage(0);
            return false;
        }

        WorkspaceStatus wStatus = getWorkspaceStatus();
        IShellTeiidParent parent = wStatus.getTeiidParent();
        ITeiidInstance instance = wStatus.getTeiidInstance();
        ITeiidJdbcInfo jdbcInfo = instance.getTeiidJdbcInfo();

        String host = argMap.get(ArgSwitch.HOST);
        String adminPortValue = argMap.get(ArgSwitch.ADMIN_PORT);
        int adminPort = -1;
        try {
            adminPort = Integer.parseInt(adminPortValue);
        } catch (NumberFormatException ex) {
            // Just will not set it if the number caused an exception
        }

        String adminUser = argMap.get(ArgSwitch.ADMIN_USER);
        String adminPasswd = argMap.get(ArgSwitch.ADMIN_PASSWD);

        String jdbcPortValue = argMap.get(ArgSwitch.JDBC_PORT);
        int jdbcPort = -1;
        try {
            jdbcPort = Integer.parseInt(jdbcPortValue);
        } catch (NumberFormatException ex) {
         // Just will not set it if the number caused an exception
        }

        String jdbcUser = argMap.get(ArgSwitch.JDBC_USER);
        String jdbcPasswd = argMap.get(ArgSwitch.JDBC_PASSWD);

        if (host != null)
            parent.setHost(host);

        if (adminPort > -1)
            parent.setPort(adminPort);

        if (adminUser != null)
            parent.setUserName(adminUser);

        if (adminPasswd != null)
            parent.setPassword(adminPasswd);

        if (jdbcPort > -1)
            jdbcInfo.setPort(jdbcPort);

        if (jdbcUser != null)
            jdbcInfo.setUsername(jdbcUser);

        if (jdbcPasswd != null)
            jdbcInfo.setPassword(jdbcPasswd);

        instance.connect();

        return instance.isConnected();
    }

    private boolean validate(Arguments args) {
        for (int i = 0; i < args.size(); ++i) {
            String arg = args.get(i).trim();
            if (arg == null || arg.length() == 0)
                return false;

            /*
             * Any other argument
             */
            if (arg.startsWith(HYPHEN)) {
                ArgSwitch argSwitch = ArgSwitch.find(arg);
                if (argSwitch == null)
                    return false;

                /*
                 * Must be followed by a value
                 */

                // Arg must be present
                if ((i + 1) >= args.size())
                    return false;

                // Arg cannot be a switch
                String nxtArg = args.get(i + 1).trim();
                if (nxtArg == null || nxtArg.startsWith(HYPHEN))
                    return false;

                argMap.put(argSwitch, nxtArg);

            } else {
                /*
                 * May be overkill but ensure that a value
                 * is preceded by a switch
                 */
                if ((i - 1) < 0)
                    return false;

                // Arg cannot be a switch
                String prevArg = args.get(i - 1).trim();
                ArgSwitch argSwitch = ArgSwitch.find(prevArg);
                if (argSwitch == null)
                    return false;

                argMap.put(argSwitch, arg);
            }
        }

        return true;
    }
}
