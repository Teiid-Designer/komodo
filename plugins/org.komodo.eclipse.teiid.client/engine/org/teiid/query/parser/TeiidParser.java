/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.teiid.query.parser;

import java.io.Reader;

import org.komodo.spi.annotation.Since;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.core.types.DataTypeManagerService;
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
public interface TeiidParser {

    /**
     * @return teiid version of this parser
     */
    ITeiidVersion getVersion();

    /**
     * @param teiidVersion
     */
    void setVersion(ITeiidVersion teiidVersion);

    /**
     * @return dataTypeManagerService
     */
    DataTypeManagerService getDataTypeService();

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
