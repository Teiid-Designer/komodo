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
package org.komodo.teiid;

import java.sql.Connection;
import java.sql.Driver;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.TeiidConnectionInfo;

public abstract class AbstractConnectionManager implements StringConstants {

    /**
     * @return the implementation of the TeiidDriver
     */
    protected abstract Driver getTeiidDriver();

    /**
     * @param vdb the target vdb
     * @param user the user
     * @param password the password
     * @param host the host
     * @param port the port
     * @param secure should connection be secure
     *
     * @return a new connection to the vdb
     * @throws Exception if error occurs
     */
    public abstract Connection getConnection(String vdb, String host, int port,
                                                                                     String user, String password,
                                                                                     boolean secure) throws Exception;

    /**
     * @param vdb
     * @param host
     * @param port
     * @param user
     * @param password
     * @param secure
     * @return an SQL connection using the TeiidDriver
     * @throws Exception
     */
    public Connection getTeiidDriverConnection(String vdb, String host, int port,
                                                                                       String user, String password, boolean secure) throws Exception {        
            String url = TeiidConnectionInfo.UrlConstructor.createTeiidConnectionUrl(vdb, secure, host, port);

            try {
                String urlAndCredentials = url + ";user=" + user + ";password=" + password + SEMI_COLON; //$NONNLS1$ //$NONNLS2$
                return getTeiidDriver().connect(urlAndCredentials, null);
        
            } catch (Exception ex) {
                throw new KException(ex);
            }
        }

    public AbstractConnectionManager() {
        super();
    }

}
