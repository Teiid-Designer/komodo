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

package org.teiid.query.parser;

import java.io.Reader;
import java.io.StringReader;
import java.util.HashSet;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.QueryParser;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.metadata.FunctionMethod;
import org.teiid.metadata.MetadataFactory;
import org.teiid.query.parser.v7.Teiid7ClientParser;
import org.teiid.query.parser.v8.Teiid8ClientParser;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.TeiidClientException;

/**
 * <p>Converts a SQL-string to an object version of a query.  This
 * QueryParser can be reused but is NOT thread-safe as the parser uses an
 * input stream.  Putting multiple queries into the same stream will result
 * in unpredictable and most likely incorrect behavior.</p>
 */
public class TCQueryParser implements QueryParser {

	private TeiidClientParser teiidParser;

    private final TeiidVersion teiidVersion;
    
	/**
	 * Construct a QueryParser - this may be reused.
	 *
	 * @param teiidVersion
	 */
	public TCQueryParser(TeiidVersion teiidVersion) {
	    this.teiidVersion = teiidVersion;
	    this.teiidParser = createTeiidParser(new StringReader("")); //$NON-NLS-1$
	}

	private TeiidClientParser createTeiidParser(Reader sql) {
	    if (sql == null)
	        throw new IllegalArgumentException(Messages.gs(Messages.TEIID.TEIID30377));

	    int major = Integer.parseInt(teiidVersion.getMajor());

	    TeiidClientParser teiidParser = null;
	    switch (major) {
	        case 7:
	            teiidParser = new Teiid7ClientParser(sql);
	            teiidParser.setVersion(teiidVersion);
	            break;
	        case 8:
	            teiidParser = new Teiid8ClientParser(sql);
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
    public TeiidClientParser getTeiidParser() {
        return this.teiidParser;
    }

    /**
     * @param sql
     * @return the {@link TeiidClientParser} initialised with the given sql
     */
    public TeiidClientParser getTeiidParser(String sql) {
        return getSqlParser(new StringReader(sql));
    }

	private TeiidClientParser getSqlParser(Reader sql) {
		if(teiidParser == null) {
		    teiidParser = createTeiidParser(sql);
		} else
		    teiidParser.ReInit(sql);

		return teiidParser;
	}

	@Deprecated
	@Removed(Version.TEIID_8_0)
	private CommandImpl parseUpdateProcedure(String sql) throws Exception {
        try{
            TeiidClientParser teiidParser = getTeiidParser(sql);
            if (!(teiidParser instanceof Teiid7ClientParser))
                throw new IllegalStateException();

            CommandImpl result = ((Teiid7ClientParser) teiidParser).updateProcedure(new ParseInfo());
            return result;
        } catch(Exception pe) {
            throw convertParserException(pe);
        }
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
	public CommandImpl parseProcedure(String sql, boolean update) throws Exception {
        try{
            if (teiidVersion.isLessThan(Version.TEIID_8_0.get()))
                return parseUpdateProcedure(sql);

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
	 * @param parseInfo
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
     * @param sql
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
    public CriteriaImpl parseCriteria(String sql) throws Exception {
        if(sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        CriteriaImpl result = null;
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
    public BaseExpression parseExpression(String sql) throws Exception {
        if(sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        BaseExpression result = null;
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
    public BaseExpression parseSelectExpression(String sql) throws Exception {
        if (sql == null) {
            throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30377));
        }

        ParseInfo dummyInfo = new ParseInfo();

        BaseExpression result = null;
        try {
            result = getTeiidParser(sql).selectExpression(dummyInfo);

        } catch (Exception e) {
            throw convertParserException(e);
        }

        return result;
    }

//    TODO consider whether we need to explicitly parse update procedure / procedure
//    public Command parseProcedure(String sql, boolean update) throws Exception {
//        try{
//            if (update) {
//                return getTeiidParser(sql).forEachRowTriggerAction(new ParseInfo());
//            }
//            Command result = getSqlParser(sql).procedureBodyCommand(new ParseInfo());
//            result.setCacheHint(SQLParserUtil.getQueryCacheOption(sql));
//            return result;
//        } catch(ParseException pe) {
//            throw convertParserException(pe);
//        } finally {
//            tm.reinit();
//        }
//    }
//
//    public Command parseUpdateProcedure(String sql) throws Exception {
//        try{
//            Command result = getTeiidParser(sql).updateProcedure(new ParseInfo());
//            result.setCacheHint(SQLParserUtil.getQueryCacheOption(sql));
//            return result;
//        } catch(ParseException pe) {
//            throw convertParserException(pe);
//        } catch(TokenMgrError tme) {
//            throw handleTokenMgrError(tme);
//        }
//    }

    public void parseDDL(MetadataFactory factory, String ddl) {
        parseDDL(factory, new StringReader(ddl));
    }
    
    public void parseDDL(MetadataFactory factory, Reader ddl) {
        try {
            getSqlParser(ddl).parseMetadata(factory);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        HashSet<FunctionMethod> functions = new HashSet<FunctionMethod>();
        for (FunctionMethod functionMethod : factory.getSchema().getFunctions().values()) {
            if (!functions.add(functionMethod)) {
                throw new RuntimeException(Messages.gs(Messages.TEIID.TEIID60015, functionMethod.getName()));
            }
        }
    }
}
