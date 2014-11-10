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

package org.komodo.modeshape.teiid.sql.proc;

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.proc.IStatement;

/**
 *
 */
public abstract class Statement extends ASTNode implements IStatement<SQLanguageVisitorImpl> {

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
    }

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public Statement(TeiidSeqParser p, int id) {
        super(p, id);
    }

    /**
     * Return type of statement to make it easier to build switch statements by statement type.
     * @return Type from TYPE constants
     */
    public StatementType getType() {
        Object property = getProperty(TeiidSqlLexicon.Statement.TYPE_PROP_NAME);
        return property == null ? null : StatementType.findStatementType(property.toString());
    }

    protected void setType(StatementType statementType) {
        setProperty(TeiidSqlLexicon.Statement.TYPE_PROP_NAME, statementType.name());
    }

    /**
     * Deep clone statement to produce a new identical statement.
     * @return Deep clone 
     */
    @Override
    public abstract Statement clone();

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

}
