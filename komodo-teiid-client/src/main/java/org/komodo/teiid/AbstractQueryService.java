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
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import org.komodo.spi.query.QSColumn;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.query.QSRow;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.type.DataTypeManager;
import org.komodo.spi.type.DataTypeManager.DataTypeName;
import org.komodo.utils.KLog;

public abstract class AbstractQueryService implements QueryService {

    private final DataTypeManager dataTypeManager;

    private final String user;

    private final String password;

    private final String host;

    private final int port;

    private final boolean secure;

    public AbstractQueryService(DataTypeManager dataTypeManager,
                                                            String host, int port, String user, String password, boolean isSecure) {
        this.dataTypeManager = dataTypeManager;
        this.host = host;
        this.port = port;
        this.user = user;
        this.password = password;
        this.secure = isSecure;
    }

    @Override
    public QSResult query(String vdb, String query, int offset, int limit) throws Exception {
        QSResult result = new QSResult();

        KLog.getLogger().debug("Commencing query execution: {0}", query);

        Connection connection = null;
        Statement statement = null;
        ResultSet rs = null;

        try {
            KLog.getLogger().debug("Initialising SQL connection for vdb {0}", vdb);
            connection = getConnection(vdb, host, port, user, password, secure);
            if (connection == null)
                throw new Exception("Failed to make a connection to '" + vdb + "' as user '" + user + "'");

            statement = connection.createStatement();

            KLog.getLogger().debug("Executing SQL Statement for query {0} with offset of {1} and limit of {2}",
                                                       query, offset, limit);
            rs = statement.executeQuery(query);

            ResultSetMetaData rsmd = rs.getMetaData();
            int columns = rsmd.getColumnCount();

            //
            // Populate the columns
            //
            for (int i = 1; i <= columns; ++i) {
                String columnName = rsmd.getColumnName(i);
                String columnLabel = rsmd.getColumnLabel(i);
                String colTypeName = rsmd.getColumnTypeName(i);
                DataTypeName typeName = dataTypeManager.getDataTypeName(colTypeName);
                QSColumn column = new QSColumn(typeName, columnName, columnLabel);
                result.addColumn(column);
            }

            int rowNum = 0;
            while (rs.next()) {
                rowNum++;

                if (offset > NO_OFFSET && rowNum < offset) {
                    continue;
                }

                if (limit > NO_LIMIT && result.getRows().size() >= limit) {
                    break;
                }

                QSRow row = new QSRow();
                for (int i = 1; i <= columns; ++i) {
                    Object value = rs.getObject(i);
                    row.add(value);
                }

                result.addRow(row);
            }

            KLog.getLogger().debug("Query executed and returning {0} results", result.getRows().size());

            return result;
        } finally {
            try {
                if (rs != null)
                    rs.close();

                if (statement != null)
                    statement.close();

                if (connection != null)
                    connection.close();
            } catch (SQLException e1) {
                // ignore
            }
        }
    }

    protected abstract Connection getConnection(String vdb, String host, int port, String user,
                                                                                            String password, boolean secure) throws Exception;
}
