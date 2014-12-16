/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.model.legacy;

/**
 * Relational DataTypes
 */
@SuppressWarnings("javadoc")
public interface DataTypes {

	String ARRAY = "ARRAY"; //$NON-NLS-1$
	String BIGDECIMAL = "BIGDECIMAL"; //$NON-NLS-1$
	String BIGINTEGER = "BIGINTEGER"; //$NON-NLS-1$
	String BINARY = "BINARY"; //$NON-NLS-1$
	String BIT = "BIT"; //$NON-NLS-1$
	String BLOB = "BLOB"; //$NON-NLS-1$
	String BYTE = "BYTE"; //$NON-NLS-1$
	String CHAR = "CHAR"; //$NON-NLS-1$
	String CLOB = "CLOB"; //$NON-NLS-1$
	String DATE = "DATE"; //$NON-NLS-1$
	String DATETIME = "DATETIME"; //$NON-NLS-1$
	String DECIMAL = "DECIMAL"; //$NON-NLS-1$
	String DOUBLE = "DOUBLE"; //$NON-NLS-1$
	String FLOAT = "FLOAT"; //$NON-NLS-1$
	String INT = "INT"; //$NON-NLS-1$
	String INTEGER = "INTEGER"; //$NON-NLS-1$
	String LONG = "LONG"; //$NON-NLS-1$
	String LONGVARBINARY = "LONGVARBINARY"; //$NON-NLS-1$
	String LONGVARCHAR = "LONGVARCHAR"; //$NON-NLS-1$
	String NCHAR = "NCHAR"; //$NON-NLS-1$
	String NUMERIC = "NUMERIC"; //$NON-NLS-1$
	String OBJECT = "OBJECT"; //$NON-NLS-1$
	String REAL = "REAL"; //$NON-NLS-1$
	String REF = "REF"; //$NON-NLS-1$
	String SHORT = "SHORT"; //$NON-NLS-1$
	String STRING = "STRING"; //$NON-NLS-1$
	String SMALLINT = "SMALLINT"; //$NON-NLS-1$
	String TIMES = "TIME"; //$NON-NLS-1$
	String TIMESTAMP = "TIMESTAMP"; //$NON-NLS-1$
	String TINYINT = "TINYINT"; //$NON-NLS-1$
	String VARBINARY = "VARBINARY"; //$NON-NLS-1$
	String VARCHAR = "VARCHAR"; //$NON-NLS-1$
	
	String[] AS_ARRAY = {
			ARRAY,
			BIGDECIMAL,
			BIGINTEGER ,
			BINARY,
			BIT,
			BLOB,
			BYTE,
			CHAR,
			CLOB,
			DATE,
			DATETIME,
			DECIMAL,
			DOUBLE,
			FLOAT,
			INT,
			INTEGER,
			LONG,
			LONGVARBINARY,
			LONGVARCHAR,
			NCHAR,
			NUMERIC,
			OBJECT,
			REAL,
			REF,
			SHORT,
			STRING,
			SMALLINT,
			TIMES,
			TIMESTAMP,
			TINYINT,
			VARBINARY,
			VARCHAR
	};
}
