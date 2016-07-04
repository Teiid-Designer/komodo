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
package org.komodo.plugin.framework.teiid;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import org.komodo.spi.query.QSRow;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.type.DataTypeManager;
import org.komodo.spi.type.DataTypeManager.DataTypeName;
import org.komodo.utils.KLog;

public abstract class AbstractQueryService implements QueryService {

    private final DataTypeManager dataTypeManager;

    private final String user;

    private final String password;

    public AbstractQueryService(DataTypeManager dataTypeManager, String user, String password) {
        this.dataTypeManager = dataTypeManager;
        this.user = user;
        this.password = password;
    }

    @Override
    public List<QSRow> query(String vdb, String query, int offset, int limit) throws Exception {
        List<QSRow> results = new ArrayList<QSRow>();

        KLog.getLogger().debug("Commencing query execution: {0}", query);

        Connection connection = null;
        Statement statement = null;
        ResultSet rs = null;

        try {
            KLog.getLogger().debug("Initialising SQL connection for vdb {0}", vdb);
            connection = getConnection(vdb, user, password);

            statement = connection.createStatement();

            KLog.getLogger().debug("Executing SQL Statement for query {0} with offset of {1} and limit of {2}",
                                                       query, offset, limit);
            rs = statement.executeQuery(query);

            ResultSetMetaData rsmd = rs.getMetaData();
            int columns = rsmd.getColumnCount();

            int rowNum = 0;
            while (rs.next()) {
                rowNum++;

                System.out.println("Looping through rows with offset of " + offset + " and limit of " + limit);
                if (offset > NO_OFFSET && rowNum < offset) {
                    System.out.println("Not Adding row since rowNum is " + rowNum + " and offset is " + offset);
                    continue;
                }

                if (limit > NO_LIMIT && results.size() >= limit) {
                    System.out.println("Bombing out because limit is " + limit + " and rowNum + offset is " + results.size());
                    break;
                }

                QSRow row = new QSRow();

                for (int i = 1; i <= columns; ++i) {
                    Object value = rs.getObject(i);
                    String colTypeName = rsmd.getColumnTypeName(i);
                    DataTypeName typeName = dataTypeManager.getDataTypeName(colTypeName);
                    row.add(value, typeName);
                }

                results.add(row);
            }

            KLog.getLogger().debug("Query executed and returning {0} result rows", results.size());

            return results;
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

    protected abstract Connection getConnection(String vdb, String user, String password) throws Exception;
}
