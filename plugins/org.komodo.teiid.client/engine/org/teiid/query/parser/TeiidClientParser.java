/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.teiid.query.parser;

import java.io.Reader;

import org.komodo.spi.annotation.Since;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.metadata.MetadataFactory;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.Command;
import org.teiid.query.sql.lang.Criteria;
import org.teiid.query.sql.lang.LanguageObject;
import org.teiid.query.sql.proc.Statement;
import org.teiid.query.sql.symbol.Expression;

/**
 *
 */
public interface TeiidClientParser {

    /**
     * @return teiid version of this parser
     */
    TeiidVersion getVersion();

    /**
     * @param teiidVersion
     */
    void setVersion(TeiidVersion teiidVersion);

    /**
     * @return dataTypeManagerService
     */
    DefaultDataTypeManager getDataTypeService();

    /**
     * Reinitialise the parser against the new sql reader
     *
     * @param sql
     */
    void ReInit(Reader sql);

    /**
     * @param nodeType
     * 
     * @return instance of commonly used node
     */
    <T extends LanguageObject> T createASTNode(ASTNodes nodeType);

    /**
     * @param parseInfo
     *
     * @return command for trigger action
     * @throws Exception 
     */
    Command forEachRowTriggerAction(ParseInfo parseInfo) throws Exception;

    /**
     * Parse an expression
     *
     * @param info
     * @return the expression
     * @throws Exception
     */
    Expression expression(ParseInfo info) throws Exception;

    /**
     * Parse a command
     *
     * @param parseInfo
     * @return the command
     * @throws Exception
     */
    Command command(ParseInfo parseInfo) throws Exception;

    /**
     * Parse a designer command
     *
     * @param parseInfo
     * @return the command
     * @throws Exception
     */
    Command designerCommand(ParseInfo parseInfo) throws Exception;

    /**
     * Parse a criteria
     *
     * @param parseInfo
     * @return the criteria
     * @throws Exception
     */
    Criteria criteria(ParseInfo parseInfo) throws Exception;

    /**
     * Parse a statement
     *
     * @param info
     * @return the statement
     * @throws Exception
     */
    Statement statement(ParseInfo info) throws Exception;

    /**
     * Parse a select expression
     *
     * @param info
     * @return the select expression
     * @throws Exception
     */
    Expression selectExpression(ParseInfo info) throws Exception;

    /**
     * Parse a procedure command
     *
     * @param parseInfo
     * @return command
     * @throws Exception 
     */
    @Since(Version.TEIID_8_0)
    Command procedureBodyCommand(ParseInfo parseInfo) throws Exception;

    /**
     * @param factory
     * @throws Exception 
     */
    @Since(Version.TEIID_8_0)
    void parseMetadata(MetadataFactory factory) throws Exception;

}
