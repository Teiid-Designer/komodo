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
package org.komodo.spi.query;

/**
 * Types of statement
 */
public enum StatementType {

    /** 
    * Represents an unknown type of statement 
    */
    TYPE_UNKNOWN,

    /**
     * Represents a IF statement
     */
    TYPE_IF,

    /**
     * Represents a SQL COMMAND statement
     */
    TYPE_COMMAND,

    /**
     * Represents a DECLARE statement
     */
    TYPE_DECLARE,

    /**
     * Represents a ERROR statement
     */
    TYPE_ERROR,

    /**
     * Represents a ASSIGNMENT statement
     */
    TYPE_ASSIGNMENT,

    /**
     * Represents a LOOP statement
     */
    TYPE_LOOP,

    /**
     * Represents a WHILE statement
     */
    TYPE_WHILE,

    /**
     * Represents a CONTINUE statement
     */
    TYPE_CONTINUE,

    /**
     * Represents a BREAK statement
     */
    TYPE_BREAK,

    /**
     * Represents a UPDATE statement
     */
    TYPE_UPDATE,

    /**
     * Represents a COMPOUND statement
     */
    TYPE_COMPOUND,

    /**
     * Represents a LEAVE statement
     */
    TYPE_LEAVE,

    /**
     * Represents a RETURN statement
     */
    TYPE_RETURN;

    /**
     * @param name
     * @return StatementType for given name
     */
    public static StatementType findStatementType(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (StatementType statementType : values()) {
            if (statementType.name().equals(name))
                return statementType;
        }

        return null;
    }

    /**
     * @param index
     * @return StatementType for given index
     */
    public static StatementType findStatementType(int index) {
        for (StatementType type : values()) {
            if (type.ordinal() == index)
                return type;
        }

        return null;
    }
}