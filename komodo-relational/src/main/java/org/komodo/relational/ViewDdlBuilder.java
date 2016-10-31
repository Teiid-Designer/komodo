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
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.TeiidSqlConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.StringUtils;

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
        
        StringBuilder sb = new StringBuilder();
        TeiidVersion teiidVersion = TeiidVersionProvider.getInstance().getTeiidVersion();
        
        // Determine constraints from table if available
        String constraintStr = getPkConstraint(uow, teiidVersion, table);
        if(StringUtils.isEmpty(constraintStr)) {
            constraintStr = getUcConstraint(uow, teiidVersion, table);
        }
        
        // Get table column names and types
        List<String> colNames = new ArrayList<String>();
        List<String> colTypes = new ArrayList<String>();
        Column[] columns = table.getColumns(uow);
        for (int i = 0; i < columns.length; i++) {
            colNames.add(columns[i].getName(uow));
            colTypes.add(columns[i].getDatatypeName(uow));
        }
        
        // Generate the View DDL
        sb.append("CREATE VIEW "); //$NON-NLS-1$
        sb.append(viewName);
        sb.append(" ("); //$NON-NLS-1$
        
        // Use generated table constraints if available
        if(constraintStr.length()>0) {
            sb.append(getColWithTypeString(teiidVersion, colNames, colTypes));
            sb.append(StringConstants.COMMA+StringConstants.SPACE);
            sb.append(constraintStr);
            sb.append(") AS \nSELECT "); //$NON-NLS-1$
            sb.append(getColString(teiidVersion, colNames));
        // No table constraint found - generate a primary key
        } else {
            sb.append("RowId integer PRIMARY KEY,"); //$NON-NLS-1$
            sb.append(getColWithTypeString(teiidVersion, colNames, colTypes));
            sb.append(") AS \nSELECT ROW_NUMBER() OVER (ORDER BY "); //$NON-NLS-1$
            sb.append(escapeSQLName(teiidVersion, colNames.get(0)));
            sb.append(StringConstants.CLOSE_BRACKET + StringConstants.COMMA);
            sb.append(getColString(teiidVersion, colNames));
        }
        sb.append(" \nFROM "); //$NON-NLS-1$
        sb.append(escapeSQLName(teiidVersion, table.getName(uow)));
        sb.append(StringConstants.SEMI_COLON);

        return sb.toString();
    }

    /*
     * Generates Primary Key constraint string if table has a PK
     * Will be of form: "CONSTRAINT pkName PRIMARY KEY (col1)"
     */
    private static String getPkConstraint(UnitOfWork uow, TeiidVersion teiidVersion, Table table) throws KException {
        StringBuilder sb = new StringBuilder();

        // Look for pk column
        PrimaryKey pk = table.getPrimaryKey(uow);
        if(pk!=null) {
            sb.append("CONSTRAINT "); //$NON-NLS-1$
            sb.append(escapeSQLName(teiidVersion,pk.getName(uow)));
            sb.append(" PRIMARY KEY ("); //$NON-NLS-1$
            Column[] pkCols = pk.getColumns(uow);
            for(int i=0; i<pkCols.length; i++) {
                if(i!=0) sb.append(StringConstants.COMMA+StringConstants.SPACE);
                sb.append(escapeSQLName(teiidVersion, pkCols[i].getName(uow)));
            }
            sb.append(StringConstants.CLOSE_BRACKET);
        }
        return sb.toString();
    }
    
    /*
     * Generates UniqueConstraint string if table has a PK
     * Will be of form: "CONSTRAINT ucName UNIQUE (col1, col2)"
     */
    private static String getUcConstraint(UnitOfWork uow, TeiidVersion teiidVersion, Table table) throws KException {
        StringBuilder sb = new StringBuilder();

        // Look for uc
        UniqueConstraint[] ucs = table.getUniqueConstraints(uow);
        for(int iuc=0; iuc<ucs.length; iuc++) {
            if(iuc!=0) sb.append(StringConstants.COMMA);
            sb.append("CONSTRAINT "); //$NON-NLS-1$
            sb.append(escapeSQLName(teiidVersion,ucs[iuc].getName(uow)));
            sb.append(" UNIQUE ("); //$NON-NLS-1$
            Column[] ucCols = ucs[iuc].getColumns(uow);
            for(int icol=0; icol<ucCols.length; icol++) {
                if(icol!=0) sb.append(StringConstants.COMMA+StringConstants.SPACE);
                sb.append(escapeSQLName(teiidVersion, ucCols[icol].getName(uow)));
            }
            sb.append(StringConstants.CLOSE_BRACKET);
        }
        return sb.toString();
    }
    
    /*
     * Generates comma separated string of the supplied column names
     * Will be of form: "column1, column2, column3"
     */
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

    /*
     * Generates comma separated string of the supplied column name with corresponding type
     * Will be of form: "column1 string, column2 string, column3 long"
     */
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
