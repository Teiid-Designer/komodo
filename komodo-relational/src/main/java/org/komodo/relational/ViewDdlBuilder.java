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
package org.komodo.relational;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.TeiidSqlConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;

/**
 * ViewDdlBuilder methods for generating view DDL for various scenarios
 */
public class ViewDdlBuilder {

    private static final char SQL_ESCAPE_CHAR = '\"';

    /**
     * Generated View DDL that supports the Teiid OData requirement - that views must have a Primary Key - to get auto-generated.
     *
     * @param uow
     *        the transaction
     * @param viewName
     *        the view name
     * @param table
     *        the table for generating the view
     * @return the View DDL
     * @throws KException
     *         if problem occurs
     */
    public static String getODataViewDdl(UnitOfWork uow,
                                         String viewName,
                                         Table table)
                                         throws KException {
        List<String> colNames = new ArrayList<String>();
        List<String> colTypes = new ArrayList<String>();
        if (table != null) {
            Column[] columns = table.getColumns(uow);
            for (int i = 0; i < columns.length; i++) {
                colNames.add(columns[i].getName(uow));
                colTypes.add(columns[i].getDatatypeName(uow));
            }
        }
        return getODataViewDdl(viewName, table.getName(uow), colNames, colTypes);
    }

    /**
     * Generated View DDL that supports the Teiid OData requirement - that views
     * must have a Primary Key - to get auto-generated.
     *
     * @param viewName
     *            the view name
     * @param sourceName
     *            the source name
     * @param columnNames
     *            the list of column names
     * @param typeNames
     *            the list of column data type names
     * @return the View DDL
     */
    public static String getODataViewDdl(String viewName, String sourceName, List<String> columnNames,
                                         List<String> typeNames) {
        TeiidVersion teiidVersion = TeiidVersionProvider.getInstance().getTeiidVersion();
        StringBuilder sb = new StringBuilder();

        sb.append("CREATE VIEW "); //$NON-NLS-1$
        sb.append(viewName);
        sb.append(" (RowId integer PRIMARY KEY,"); //$NON-NLS-1$
        sb.append(getColWithTypeString(teiidVersion, columnNames, typeNames));
        sb.append(") AS \nSELECT ROW_NUMBER() OVER (ORDER BY "); //$NON-NLS-1$
        sb.append(escapeSQLName(teiidVersion, columnNames.get(0)));
        sb.append(StringConstants.CLOSE_BRACKET + StringConstants.COMMA);
        sb.append(getColString(teiidVersion, columnNames));
        sb.append(" \nFROM "); //$NON-NLS-1$
        sb.append(escapeSQLName(teiidVersion, sourceName));
        sb.append(StringConstants.SEMI_COLON);

        return sb.toString();
    }

    private static String getColString(TeiidVersion teiidVersion, List<String> columnNames) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < columnNames.size(); i++) {
            if (i != 0) {
                sb.append(StringConstants.COMMA);
            }
            sb.append(StringConstants.SPACE + escapeSQLName(teiidVersion, columnNames.get(i)));
        }
        return sb.toString();
    }

    private static String getColWithTypeString(TeiidVersion teiidVersion, List<String> columnNames,
                                               List<String> typeNames) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < columnNames.size(); i++) {
            if (i != 0) {
                sb.append(StringConstants.COMMA);
            }
            sb.append(StringConstants.SPACE + escapeSQLName(teiidVersion, columnNames.get(i)));
            sb.append(StringConstants.SPACE);
            sb.append(typeNames.get(i));
        }
        return sb.toString();
    }

    private static String escapeSQLName(TeiidVersion teiidVersion, String part) {
        if (TeiidSqlConstants.isReservedWord(teiidVersion, part)) {
            return SQL_ESCAPE_CHAR + part + SQL_ESCAPE_CHAR;
        }
        boolean escape = true;
        char start = part.charAt(0);
        if (start == '#' || start == '@' || isLetter(start)) {
            escape = false;
            for (int i = 1; !escape && i < part.length(); i++) {
                char c = part.charAt(i);
                escape = !isLetterOrDigit(c) && c != '_';
            }
        }
        if (escape) {
            return SQL_ESCAPE_CHAR + part + SQL_ESCAPE_CHAR;
        }
        return part;
    }

    private static boolean isLetter(char c) {
        return isBasicLatinLetter(c) || Character.isLetter(c);
    }

    private static boolean isLetterOrDigit(char c) {
        return isBasicLatinLetter(c) || isBasicLatinDigit(c) || Character.isLetterOrDigit(c);
    }

    private static boolean isBasicLatinLetter(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    private static boolean isBasicLatinDigit(char c) {
        return c >= '0' && c <= '9';
    }

}
