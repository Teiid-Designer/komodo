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
package org.komodo.modeshape.teiid.sequencer;

import static org.junit.Assert.assertNotNull;
import javax.jcr.Node;
import org.junit.Test;
import org.komodo.modeshape.AbstractTSqlSequencerTest;
import org.komodo.repository.KSequencerController.SequencerType;
import org.komodo.spi.lexicon.TeiidSqlLexicon.AbstractCompareCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.CompareCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.ElementSymbol;
import org.komodo.spi.lexicon.TeiidSqlLexicon.From;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Function;
import org.komodo.spi.lexicon.TeiidSqlLexicon.JoinPredicate;
import org.komodo.spi.lexicon.TeiidSqlLexicon.MultipleElementSymbol;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Query;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Select;
import org.komodo.spi.query.JoinTypeTypes;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractTestSequencers extends AbstractTSqlSequencerTest {

    /**
     * @param teiidVersion
     */
    public AbstractTestSequencers(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    @Test(timeout = 5000000)
    public void testBasicDDLStatement() throws Exception {
        String ddl =  "CREATE VIEW Tweet AS select * FROM twitterview.getTweets;";
        Node fileNode = prepareSequence(ddl, SequencerType.DDL);

        //
        // Sequencing completed, now verify
        //

        // DDL Sequencer creates the 'Tweet' node
        Node tweetNode = fileNode.getNode("Tweet");
        assertNotNull(tweetNode);

        // TSQL Sequencer is triggered from the Tweet node's queryExpression property
        // and sequences its string value into TSQL nodes starting with a query node
        Node queryNode = verify(tweetNode, Query.ID, Query.ID);

        // Query should have a SELECT
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        // Select should have a symbols collection
        // Select has a * so symbolsNode should be a MultipleElementSymbol
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        // Should have a FROM
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        // UnaryFromClause should have a group
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "twitterview.getTweets");
    }

    @Test(timeout=5000000)
    public void testComplexDDLStatement() throws Exception {
        StringBuffer ddl = new StringBuffer();

        /*
         * Commented out unsupported SCHEMA syntax text.
         * May be supported in the future but at the moment, generate
         * unknown statements which are invalid for KSequencers.
         */

//        ddl.append("CREATE FOREIGN SCHEMA TWITTER (connection-jndi-name=\"java:/twitterDS\":translator-name=\"rest\") ")
        ddl.append("CREATE VIRTUAL PROCEDURE getTweets(query varchar) RETURNS (created_on varchar(25), from_user varchar(25), to_user varchar(25), ")
             .append("profile_image_url varchar(25), source varchar(25), text varchar(140)) AS ")
                     .append("select tweet.* from ")
                     .append("(call twitter.invokeHTTP(action => 'GET', endpoint =>querystring(\'',query as \"q\"))) w,  ")
                             .append("XMLTABLE('results' passing JSONTOXML('myxml', w.result) columns ")
                             .append("created_on string PATH 'created_at', ")
                             .append("from_user string PATH 'from_user', ")
                             .append("to_user string PATH 'to_user', ")
                             .append("profile_image_url string PATH 'profile_image_url', ")
                             .append("source string PATH 'source', ")
                             .append("text string PATH 'text') tweet; ")
             .append("CREATE VIEW Tweet AS select * FROM twitterview.getTweets; ")
//             .append("CREATE FOREIGN SCHEMA PARTSSUPPLIER (connection-jndi-name=\"parts-oracle\":translator-name=\"jdbc\") ")
             .append("CREATE FOREIGN TABLE PARTSSUPPLIER.PART (id integer PRIMARY KEY,  name   varchar(25), color varchar(25),  weight integer); ")
//             .append("CREATE VIRTUAL SCHEMA PARTS_VIEWS ")
             .append("CREATE VIEW PARTS_VIEWS.PARTS ( ")
                 .append("PART_ID integer PRIMARY KEY, ")
                 .append("PART_NAME varchar(255), ")
                 .append("PART_COLOR varchar(30), ")
                 .append("PART_WEIGHT varchar(255) ")
                 .append(") AS ")
                     .append("SELECT ")
                         .append("a.id as PART_ID, ")
                         .append("a.name as PART_NAME, ")
                         .append("b.color as PART_COLOR, ")
                         .append("b.weight as PART_WEIGHT ")
                     .append("FROM PARTSSUPPLIER.part a, PARTSSUPPLIER.part b WHERE a.id = b.id; ")
//             .append("CREATE FOREIGN SCHEMA PRODUCT (connection-jndi-name=\"product-oracle\":translator-name=\"jdbc\") ")
             .append("CREATE FOREIGN TABLE PRODUCT.Customer ( ")
                 .append("id integer PRIMARY KEY, ")
                 .append("firstname  varchar(25), ")
                 .append("lastname varchar(25), ")
                 .append("dob timestamp); ")
             .append("CREATE FOREIGN TABLE PRODUCT.Order ( ")
                 .append("id integer PRIMARY KEY, ")
                 .append("customerid  integer, ")
                 .append("saledate date, ")
                 .append("amount decimal(25,4), ")
                 .append("CONSTRAINT CUSTOMER_FK FOREIGN KEY(customerid) REFERENCES PRODUCT.Customer(id)); ")
//             .append("CREATE VIRTUAL SCHEMA PRODUCT_VIEWS ")
             .append("CREATE VIEW PRODUCT_VIEWS.CustomerOrders ( ")
                 .append("name varchar(50), ")
                 .append("saledate date, ")
                 .append("amount decimal) OPTIONS (CARDINALITY 100, ANNOTATION 'Example') ")
                 .append("AS ")
                 .append("SELECT ")
                     .append("concat(c.firstname, c.lastname) as name, ")
                     .append("o.saledate as saledate, ")
                     .append("o.amount as amount ")
                     .append("FROM Customer C INNER JOIN o ON c.id = o.customerid; ");

        Node fileNode = prepareSequence(ddl.toString(), SequencerType.DDL);

        //
        // Sequencing completed, now verify
        //

        // DDL Sequencer creates the 'Tweet' node
        Node tweetNode = fileNode.getNode("Tweet");
        assertNotNull(tweetNode);
        Node queryNode = verify(tweetNode, Query.ID, Query.ID);
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "twitterview.getTweets");

        // DDL Sequencer create the 'PARTS_VIEWS.PARTS' node
        Node partsNode = fileNode.getNode("PARTS_VIEWS.PARTS");
        assertNotNull(partsNode);
        queryNode = verify(partsNode, Query.ID, Query.ID);
        selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "PART_ID", ElementSymbol.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "PART_NAME", ElementSymbol.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 3, "PART_COLOR", ElementSymbol.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 4, "PART_WEIGHT", ElementSymbol.ID);
        fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a");
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "b");
        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyElementSymbol(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "b.id");
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "a.id");

        // DDL Sequencer create the 'PRODUCT_VIEWS.CustomerOrders' node
        Node productNode = fileNode.getNode("PRODUCT_VIEWS.CustomerOrders");
        assertNotNull(productNode);
        queryNode = verify(productNode, Query.ID, Query.ID);
        selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "name", Function.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "saledate", ElementSymbol.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 3, "amount", ElementSymbol.ID);
        fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        Node fromClause = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(fromClause, JoinTypeTypes.JOIN_INNER);
        criteriaNode = verify(fromClause, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyElementSymbol(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "o.customerid");
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "c.id");
        verifyUnaryFromClauseGroup(fromClause, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "o");
        verifyUnaryFromClauseGroup(fromClause, JoinPredicate.LEFT_CLAUSE_REF_NAME, "C");
    }

    @SuppressWarnings( "unused" )
    private interface MaterializedOptions {

        /**
         * Whether table is materialized
         */
        String MATERIALIZED = "MATERIALIZED";

        /**
         * Materialized table
         */
        String MATERIALIZED_TABLE = "MATERIALIZED_TABLE";

        /**
         * Whether table is updateable
         */
        String UPDATABLE = "UPDATABLE";

        /**
         * Allow Teiid based management
         */
        String ALLOW_MATVIEW_MANAGEMENT = "teiid_rel:ALLOW_MATVIEW_MANAGEMENT";

        /**
         * Fully qualified Status Table Name defined above
         */
        String MATVIEW_STATUS_TABLE = "teiid_rel:MATVIEW_STATUS_TABLE";

        /**
         * Semi-colon(;) separated DDL/DML commands to run
         * before the actual load of the cache, typically used to
         * truncate staging table
         */
        String MATVIEW_BEFORE_LOAD_SCRIPT = "teiid_rel:MATVIEW_BEFORE_LOAD_SCRIPT";

        /**
         * semi-colon(;) separated DDL/DML commands to run for
         * loading of the cache
         */
        String MATVIEW_LOAD_SCRIPT = "teiid_rel:MATVIEW_LOAD_SCRIPT";

        /**
         * semi-colon(;) separated DDL/DML commands to run after
         * the actual load of the cache. Typically used to rename
         * staging table to actual cache table. Required when
         * MATVIEW_LOAD_SCRIPT not defined to copy data from
         * teiid_rel:MATVIEW_STAGE_TABLE to MATVIEW table
         */
        String MATVIEW_AFTER_LOAD_SCRIPT = "teiid_rel:MATVIEW_AFTER_LOAD_SCRIPT";

        /**
         * Allowed values are {NONE, VDB, SCHEMA}, which define
         * if the cached contents are shared among different VDB
         * versions and different VDBs as long as schema names match
         */
        String MATVIEW_SHARE_SCOPE = "teiid_rel:MATVIEW_SHARE_SCOPE";

        /**
         * When MATVIEW_LOAD_SCRIPT property not defined, Teiid
         * loads the cache contents into this table.
         * Required when MATVIEW_LOAD_SCRIPT not defined
         */
        String MATERIALIZED_STAGE_TABLE = "teiid_rel:MATERIALIZED_STAGE_TABLE";

        /**
         * DML commands to run start of vdb
         */
        String ON_VDB_START_SCRIPT = "teiid_rel:ON_VDB_START_SCRIPT";

        /**
         * DML commands to run at VDB un-deploy; typically
         * used for cleaning the cache/status tables
         */
        String ON_VDB_DROP_SCRIPT = "teiid_rel:ON_VDB_DROP_SCRIPT";

        /**
         * Action to be taken when mat view contents are
         * requested but cache is invalid. Allowed values
         * are
         *
         * THROW_EXCEPTION = throws an exception,
         * IGNORE = ignores the warning and supplied invalidated data,
         * WAIT = waits until the data is refreshed and valid
         *              then provides the updated data)
         */
        String MATVIEW_ONERROR_ACTION = "teiid_rel:MATVIEW_ONERROR_ACTION";

        /**
         * Time to live in milliseconds. Provide property or
         * cache hint on view transformation - property takes
         * precedence.
         */
        String MATVIEW_TTL = "teiid_rel:MATVIEW_TTL";
    }

    @Test(timeout=300000)
    public void testMaterializedStatement() throws Exception {
        StringBuffer ddl = new StringBuffer();

        ddl.append("CREATE view stockPricesMatView (")
            .append("product_id integer, ")
            .append("symbol string, ")
            .append("price bigdecimal, ")
            .append("company_name   varchar(256) ")
        .append(") OPTIONS (MATERIALIZED 'TRUE', UPDATABLE 'TRUE', ")
               .append("MATERIALIZED_TABLE 'Accounts.h2_stock_mat', ")
               .append("\"teiid_rel:MATVIEW_TTL\" 120000, ")
               .append("\"teiid_rel:MATVIEW_BEFORE_LOAD_SCRIPT\" 'execute accounts.native(''truncate table h2_stock_mat'');', ")
               .append("\"teiid_rel:MATVIEW_AFTER_LOAD_SCRIPT\"  'execute accounts.native('''')', ")
               .append("\"teiid_rel:ON_VDB_DROP_SCRIPT\" 'DELETE FROM Accounts.status WHERE Name=''stock'' AND schemaname = ''Stocks''', ")
               .append("\"teiid_rel:MATERIALIZED_STAGE_TABLE\" 'Accounts.h2_stock_mat', ")
               .append("\"teiid_rel:ALLOW_MATVIEW_MANAGEMENT\" 'true', ")
               .append("\"teiid_rel:MATVIEW_STATUS_TABLE\" 'status', ")
               .append("\"teiid_rel:MATVIEW_SHARE_SCOPE\" 'NONE', ")
               .append("\"teiid_rel:MATVIEW_ONERROR_ACTION\" 'THROW_EXCEPTION') ")
        .append("AS SELECT  A.ID, S.symbol, S.price, A.COMPANY_NAME ")
        .append("FROM Stocks.StockPrices AS S, Accounts.PRODUCT AS A ")
        .append("WHERE S.symbol = A.SYMBOL;");

        Node fileNode = prepareSequence(ddl.toString(), SequencerType.DDL);

        //
        // Sequencing completed, now verify
        //

        // DDL Sequencer creates the 'stock view' node
        Node stockViewNode = verify(fileNode, "stockPricesMatView", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);

        Node option = verify(stockViewNode, enc(MaterializedOptions.ALLOW_MATVIEW_MANAGEMENT), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, Boolean.TRUE.toString().toLowerCase());

        option = verify(stockViewNode, enc(MaterializedOptions.MATVIEW_SHARE_SCOPE), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "NONE");

        option = verify(stockViewNode, enc(MaterializedOptions.MATVIEW_AFTER_LOAD_SCRIPT), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "execute accounts.native('''')");

        option = verify(stockViewNode, enc(MaterializedOptions.MATVIEW_TTL), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, 120000);

        option = verify(stockViewNode, enc(MaterializedOptions.ON_VDB_DROP_SCRIPT), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "DELETE FROM Accounts.status WHERE Name=''stock'' AND schemaname = ''Stocks''");

        option = verify(stockViewNode, enc(MaterializedOptions.MATERIALIZED_STAGE_TABLE), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "Accounts.h2_stock_mat");

        option = verify(stockViewNode, enc(MaterializedOptions.MATERIALIZED_TABLE), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "Accounts.h2_stock_mat");

        option = verify(stockViewNode, enc(MaterializedOptions.UPDATABLE), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, Boolean.TRUE.toString().toUpperCase());

        option = verify(stockViewNode, enc(MaterializedOptions.MATVIEW_BEFORE_LOAD_SCRIPT), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "execute accounts.native(''truncate table h2_stock_mat'');");

        option = verify(stockViewNode, enc(MaterializedOptions.MATVIEW_STATUS_TABLE), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "status");

        option = verify(stockViewNode, enc(MaterializedOptions.MATVIEW_ONERROR_ACTION), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, "THROW_EXCEPTION");

        option = verify(stockViewNode, enc(MaterializedOptions.MATERIALIZED), StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(option, StandardDdlLexicon.VALUE, Boolean.TRUE.toString().toUpperCase());

        Node queryNode = verify(stockViewNode, Query.ID, Query.ID);

        //
        // Verify TSQL Sequencer
        //
        verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
    }
}
