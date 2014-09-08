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
import org.komodo.modeshape.teiid.Messages;
import org.komodo.modeshape.teiid.TeiidClientException;
import org.komodo.modeshape.teiid.parser.v8.Teiid8Parser;
import org.komodo.modeshape.teiid.sql.lang.Command;
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.IQueryParser;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;

/**
 * <p>Converts a SQL-string to an object version of a query.  This
 * QueryParser can be reused but is NOT thread-safe as the parser uses an
 * input stream.  Putting multiple queries into the same stream will result
 * in unpredictable and most likely incorrect behavior.</p>
 */
public class QueryParser implements IQueryParser {

	private TeiidParser teiidParser;

    private final ITeiidVersion teiidVersion;
    
	/**
	 * Construct a QueryParser - this may be reused.
	 *
	 * @param teiidVersion
	 */
	public QueryParser(ITeiidVersion teiidVersion) {
	    this.teiidVersion = teiidVersion;
	    this.teiidParser = createTeiidParser(new StringReader("")); //$NON-NLS-1$
	}

	private TeiidParser createTeiidParser(Reader sql) {
	    if (sql == null)
	        throw new IllegalArgumentException(Messages.gs(Messages.TEIID.TEIID30377));

	    int major = Integer.parseInt(teiidVersion.getMajor());

	    TeiidParser teiidParser = null;
	    switch (major) {
	        case 8:
	            teiidParser = new Teiid8Parser(sql);
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
    public TeiidParser getTeiidParser() {
        return this.teiidParser;
    }

    /**
     * @param sql
     * @return the {@link TeiidParser} initialised with the given sql
     */
    public TeiidParser getTeiidParser(String sql) {
        return getSqlParser(new StringReader(sql));
    }

	private TeiidParser getSqlParser(Reader sql) {
		if(teiidParser == null) {
		    teiidParser = createTeiidParser(sql);
		} else
		    teiidParser.ReInit(sql);

		return teiidParser;
	}

	/**
	 * Parses the given procedure sql string, returning the object
	 * representation
	 *
	 * @param sql
	 * @param update
	 * @return command of sql
	 * @throws Exception
	 */
	@Since(Version.TEIID_8_0)
	public Command parseProcedure(String sql, boolean update) throws Exception {
        try{
            if (update) {
                return getTeiidParser(sql).forEachRowTriggerAction(new ParseInfo());
            }
            Command result = getTeiidParser(sql).procedureBodyCommand(new ParseInfo());
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
    public Command parseCommand(String sql) throws Exception {
	    return parseCommand(sql, new ParseInfo(), false);
	}

	/**
     * Takes a SQL string representing a Command and returns the object
     * representation.
     *
     * @param sql SQL string instead of string litral
	 * @param parseInfo
     * @return SQL object representation
     * @throws Exception if parsing fails
     * @throws IllegalArgumentException if sql is null
     */ 
    public Command parseCommand(String sql, ParseInfo parseInfo) throws Exception {
        return parseCommand(sql, parseInfo, false);
    }

    /**
     * Takes a SQL string representing a Command and returns the object
     * representation.
     *
     * @param sql
     * @return SQL object representation
     * @throws Exception if parsing fails
     * @throws IllegalArgumentException if sql is null
     */
    @Override
    public Command parseDesignerCommand(String sql) throws Exception {
        return parseCommand(sql, new ParseInfo(), true);
    }

	private Command parseCommand(String sql, ParseInfo parseInfo, boolean designerCommands) throws Exception {
        if(sql == null || sql.length() == 0) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }
        
    	Command result = null;
        try{
            if (designerCommands) {
                result = getTeiidParser(sql).designerCommand(parseInfo);
            } else {
                result = getTeiidParser(sql).command(parseInfo);
            }
        } catch(Exception e) {
           throw convertParserException(e);
        }

		return result;
	}

    /**
     * @param e
     * @return
     */
	private TeiidClientException convertParserException(Exception e) {
        TeiidClientException qpe = new TeiidClientException(e, Messages.getString(Messages.TeiidParser.lexicalError, e.getMessage()));
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
    public Criteria parseCriteria(String sql) throws Exception {
        if(sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        Criteria result = null;
        try{
            result = getTeiidParser(sql).criteria(dummyInfo);

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
    public Expression parseExpression(String sql) throws Exception {
        if(sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        Expression result = null;
        try{
            result = getTeiidParser(sql).expression(dummyInfo);
        } catch (Exception e) {
            throw convertParserException(e);
        }

        return result;
    }

    /**
     * @param sql
     * @return Expression representing sql
     * @throws Exception
     */
    public Expression parseSelectExpression(String sql) throws Exception {
        if (sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        Expression result = null;
        try {
            result = getTeiidParser(sql).selectExpression(dummyInfo);

        } catch (Exception e) {
            throw convertParserException(e);
        }

        return result;
    }
}
