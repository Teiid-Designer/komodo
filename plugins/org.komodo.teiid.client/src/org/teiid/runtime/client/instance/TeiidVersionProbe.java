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
package org.teiid.runtime.client.instance;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.InetSocketAddress;
import java.net.Socket;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.utils.KLog;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.admin.v8.Admin8Factory;

/**
 *
 */
public class TeiidVersionProbe {

    private final TeiidParent parent;

    private TeiidVersion teiidVersion;

    /**
     * @param parent
     */
    public TeiidVersionProbe(TeiidParent parent) {
        this.parent = parent;
    }

    /**
     * @return true is parent can be connected to or false otherwise
     */
    public boolean isParentConnected() {
        Socket socket = null;
        Reader in = null;
        InetSocketAddress endPoint = new InetSocketAddress(parent.getHost(), parent.getPort());

        if (endPoint.isUnresolved()) {
            KLog.getLogger().warn(Messages.getString(Messages.TeiidInstance.parentNotStartedMessage, endPoint)); 
            return false;
        }

        try {
            socket = new Socket();
            socket.connect(endPoint, 1024);

            /*
             * This may not seem necessary since a socket connection
             * should be enough. However, TEIIDDES-1971 has shown
             * that without actually using the socket, 'Connection reset
             * by peer' messages with be reported in the server log.
             */
            InputStream socketReader = socket.getInputStream();

            final char[] buffer = new char[100];
            in = new InputStreamReader(socketReader);
            int rsz = in.read(buffer, 0, buffer.length);
            if (rsz == -1) {
                return false;
            }

            StringBuffer output = new StringBuffer();
            for (int i = 0; i < buffer.length; ++i) {
                if (Character.isLetterOrDigit(buffer[i])) {
                    output.append(buffer[i]);
                }
            }

            return true;
        } catch (Exception ex) {
            return false;
        } finally {
            try {
                if (in != null)
                    in.close();

                if (socket != null && socket.isConnected()) {
                    socket.close();
                    socket = null;
                }
            } catch (Exception ex2) {
                /*
                 * Unlikely event that socket did not close correctly.
                 * Do nothing
                 */
            }
        }
    }    

    private void determineVersion() throws Exception {
        if (! isParentConnected())
            teiidVersion = DefaultTeiidVersion.Version.DEFAULT_TEIID_VERSION.get();

        /* Attempt to connect using the Admin8Factory */
        teiidVersion = Admin8Factory.getInstance().getTeiidVersion(parent.getHost(), 
                                                                                        parent.getPort(),
                                                                                        parent.getUserName(),
                                                                                        parent.getPassword());
    }

    /**
     * @return version
     * @throws Exception
     */
    public TeiidVersion getVersion() throws Exception {
        if (teiidVersion == null)
            determineVersion();

        return teiidVersion;
    }

}
