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

package org.komodo.modeshape.teiid.parser;

import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.komodo.modeshape.teiid.Messages;
import org.komodo.modeshape.teiid.TeiidClientException;
import org.komodo.modeshape.teiid.parser.AbstractTeiidSeqParser.ParsingError;
import org.komodo.modeshape.teiid.parser.completion.TeiidCompletionParser;
import org.komodo.modeshape.teiid.sql.lang.CommandImpl;
import org.komodo.modeshape.teiid.sql.lang.CriteriaImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.query.QueryParser;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 * <p>Converts a SQL-string to an object version of a query.  This
 * QueryParser can be reused but is NOT thread-safe as the parser uses an
 * input stream.  Putting multiple queries into the same stream will result
 * in unpredictable and most likely incorrect behavior.</p>
 */
public class SQQueryParser implements QueryParser, StringConstants {

	private TeiidSeqParser teiidParser;

    private final TeiidVersion teiidVersion;
    
	/**
	 * Construct a QueryParser - this may be reused.
	 *
	 * @param teiidVersion version of teiid
	 */
	public SQQueryParser(TeiidVersion teiidVersion) {
	    this.teiidVersion = teiidVersion;
	    this.teiidParser = createTeiidParser(new StringReader("")); //$NON-NLS-1$
	}

	private TeiidSeqParser createTeiidParser(Reader sql) {
	    if (sql == null)
	        throw new IllegalArgumentException(Messages.gs(Messages.TEIID.TEIID30377));

	    int major = Integer.parseInt(teiidVersion.getMajor());

	    TeiidSeqParser teiidParser = null;
	    switch (major) {
	        case 8:
	            teiidParser = new TeiidSequencingParser(sql);
	            teiidParser.setVersion(teiidVersion);
	            break;
	        default:
	            throw new IllegalStateException(Messages.getString(Messages.TeiidParser.noParserForVersion, major));
	    }

	    return teiidParser;
	}

    /**
     * @return the teiidParser
     */
    public TeiidSeqParser getTeiidParser() {
        return this.teiidParser;
    }

    /**
     * @param sql text to pass to the parser
     * @return the {@link TeiidSeqParser} initialised with the given sql
     */
    public TeiidSeqParser getTeiidParser(String sql) {
        return getSqlParser(new StringReader(sql));
    }

	private TeiidSeqParser getSqlParser(Reader sql) {
		if(teiidParser == null) {
		    teiidParser = createTeiidParser(sql);
		} else
		    teiidParser.reset(sql);

		return teiidParser;
	}

	/**
	 * Parses the given procedure sql string, returning the object
	 * representation
	 *
	 * @param sql text to pass to the parser
	 * @param update if true then assumes an update procedure
	 * @return command of sql
	 * @throws Exception if failure to parse correctly
	 */
	@Since(Version.TEIID_8_0)
	public CommandImpl parseProcedure(String sql, boolean update) throws Exception {
        try{
            if (update) {
                return getTeiidParser(sql).forEachRowTriggerAction(new ParseInfo());
            }
            CommandImpl result = getTeiidParser(sql).procedureBodyCommand(new ParseInfo());
            return result;
        } catch(Exception pe) {
            throw convertParserException(pe);
        }
    }

	/**
	 * Takes a SQL string representing a Command and returns the object
	 * representation.
	 *
	 * @param sql SQL string 
	 * instead of string litral
	 * @return SQL object representation
	 * @throws Exception if parsing fails
	 * @throws IllegalArgumentException if sql is null
	 */	
	@Override
    public CommandImpl parseCommand(String sql) throws Exception {
	    return parseCommand(sql, new ParseInfo(), false);
	}

	/**
     * Takes a SQL string representing a Command and returns the object
     * representation.
     *
     * @param sql SQL string instead of string litral
	 * @param parseInfo parsing attribute model
     * @return SQL object representation
     * @throws Exception if parsing fails
     * @throws IllegalArgumentException if sql is null
     */ 
    public CommandImpl parseCommand(String sql, ParseInfo parseInfo) throws Exception {
        return parseCommand(sql, parseInfo, false);
    }

    /**
     * Takes a SQL string representing a Command and returns the object
     * representation.
     *
     * @param sql SQL string
     * @return SQL object representation
     * @throws Exception if parsing fails
     * @throws IllegalArgumentException if sql is null
     */
    @Override
    public CommandImpl parseDesignerCommand(String sql) throws Exception {
        return parseCommand(sql, new ParseInfo(), true);
    }

	private CommandImpl parseCommand(String sql, ParseInfo parseInfo, boolean designerCommands) throws Exception {
        if(sql == null || sql.length() == 0) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }
        
    	CommandImpl result = null;
        try{
            TeiidSeqParser teiidParser = getTeiidParser(sql);
            if (designerCommands) {
                result = teiidParser.designerCommand(parseInfo);
            } else {
                result = teiidParser.command(parseInfo);
            }

            analyzeErrors(teiidParser);
        } catch(Exception e) {
           throw convertParserException(e);
        }

		return result;
	}

    private void analyzeErrors(TeiidSeqParser teiidParser) throws Exception {
        List<ParsingError> errors = teiidParser.getErrors();
        if (! errors.isEmpty()) {
            StringBuffer buf = new StringBuffer();
            Iterator<ParsingError> iterator = errors.iterator();
            while (iterator.hasNext()) {
                ParsingError error = iterator.next();
                buf.append(error.toString());
                if (iterator.hasNext())
                    buf.append(NEW_LINE);
            }

            throw new Exception(buf.toString());
        }
    }

    /**
     * @param e
     * @return
     */
	private TeiidClientException convertParserException(Exception e) {
        TeiidClientException qpe = new TeiidClientException(e, e.getMessage());
        return qpe;
    }

    /**
     * Takes a SQL string representing an SQL criteria (i.e. just the WHERE
     * clause) and returns the object representation.
     * @param sql SQL criteria (WHERE clause) string
     * @return Criteria SQL object representation
     * @throws Exception if parsing fails
     * @throws TeiidClientException if sql is null
     */
    @Override
    public CriteriaImpl parseCriteria(String sql) throws Exception {
        if(sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        CriteriaImpl result = null;
        try{
            TeiidSeqParser teiidParser = getTeiidParser(sql);
            result = teiidParser.criteria(dummyInfo);
            analyzeErrors(teiidParser);
        } catch(Exception e) {
            throw convertParserException(e);
        }

        return result;
    }
        
    /**
     * Takes a SQL string representing an SQL expression
     * and returns the object representation.
     * @param sql SQL expression string
     * @return SQL expression object
     * @throws Exception if parsing fails
     * @throws IllegalArgumentException if sql is null
     */
    @Override
    public BaseExpression parseExpression(String sql) throws Exception {
        if(sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        BaseExpression result = null;
        try{
            TeiidSeqParser teiidParser = getTeiidParser(sql);
            result = teiidParser.expression(dummyInfo);
            analyzeErrors(teiidParser);
        } catch (Exception e) {
            throw convertParserException(e);
        }

        return result;
    }

    /**
     * @param sql SQL string
     * @return Expression representing sql
     * @throws Exception if parsing fails
     */
    public BaseExpression parseSelectExpression(String sql) throws Exception {
        if (sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        BaseExpression result = null;
        try {
            TeiidSeqParser teiidParser = getTeiidParser(sql);
            result = teiidParser.selectExpression(dummyInfo);
            analyzeErrors(teiidParser);
        } catch (Exception e) {
            throw convertParserException(e);
        }

        return result;
    }

    /**
     * Retrieves the next expected tokens to come after the given SQL.
     * Uses the completion parser to get all the textual possibilities for the
     * next SQL in the sequence.
     *
     * @param sql current SQL string (does not have to be complete) 
     * @return list of tokens expected to follow the given sql 
     */
    public Collection<String> getExpectedTokens(String sql) {
        StringReader reader = new StringReader(sql);
        TeiidCompletionParser completionParser = new TeiidCompletionParser(reader);

        try {            
            completionParser.command(new ParseInfo());
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        List<String> list = new ArrayList<String>(completionParser.getExpected());
        Collections.sort(list);

        return list;
    }
}
