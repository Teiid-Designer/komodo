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
import static org.junit.Assert.assertTrue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.jcr.Node;
import org.junit.Test;
import org.komodo.modeshape.AbstractTSqlSequencerTest;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AbstractCompareCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CompareCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ElementSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.From;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Function;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.JoinPredicate;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.MultipleElementSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Query;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Select;
import org.komodo.spi.query.sql.lang.JoinType;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

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
        CountDownLatch updateLatch = addPathLatchListener(1, ".*\\/ddl:statements\\/Tweet\\/tsql:query");

        String ddl =  "CREATE VIEW Tweet AS select * FROM twitterview.getTweets;";        
        Node fileNode = prepareSequence(ddl, SequenceType.DDL);

        // Wait for the starting of the repository or timeout of 3 minutes
        assertTrue(updateLatch.await(3, TimeUnit.MINUTES));

        //
        // Sequencing completed, now verify
        //

        // DDL sequencer create the statements node
        Node statementsNode = fileNode.getNode(StandardDdlLexicon.STATEMENTS_CONTAINER);
        assertNotNull(statementsNode);

        // DDL Sequencer creates the 'Tweet' node 
        Node tweetNode = statementsNode.getNode("Tweet");
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

        ddl.append("CREATE FOREIGN SCHEMA TWITTER (connection-jndi-name=\"java:/twitterDS\":translator-name=\"rest\") ")
             .append("CREATE VIRTUAL PROCEDURE getTweets(query varchar) RETURNS (created_on varchar(25), from_user varchar(25), to_user varchar(25), ")
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
             .append("CREATE FOREIGN SCHEMA PARTSSUPPLIER (connection-jndi-name=\"parts-oracle\":translator-name=\"jdbc\"); ")
             .append("CREATE FOREIGN TABLE PARTSSUPPLIER.PART (id integer PRIMARY KEY,  name   varchar(25), color varchar(25),  weight integer); ") 
             .append("CREATE VIRTUAL SCHEMA PARTS_VIEWS; ")
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
             .append("CREATE FOREIGN SCHEMA PRODUCT (connection-jndi-name=\"product-oracle\":translator-name=\"jdbc\"); ")
             .append("CREATE FOREIGN TABLE PRODUCT.Customer ( ")
                 .append("id integer PRIMARY KEY, ")
                 .append("firstname  varchar(25), ")
                 .append("lastname varchar(25), ")
                 .append("dob timestamp); ")
             .append("CREATE FOREIGN TABLE PRODUCT.Order ( ")
                 .append("id integer PRIMARY KEY, ")
                 .append("customerid  integer, ")
                 .append("saledate date, ")
                 .append("amount decimal(25,4)) (CONSTRAINT FOREIGN  KEY(customerid)  REFERENCES Customer(id)); ")
             .append("CREATE VIRTUAL SCHEMA PRODUCT_VIEWS; ")
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

        CountDownLatch updateLatch = addPathLatchListener(4, ".*\\/ddl:statements\\/.*\\/tsql:query");

        Node fileNode = prepareSequence(ddl.toString(), SequenceType.DDL);

        // Wait for the starting of the repository or timeout of 5 minutes
        assertTrue(updateLatch.await(5, TimeUnit.MINUTES));

        //
        // Sequencing completed, now verify
        //

        // DDL sequencer create the statements node
        Node statementsNode = fileNode.getNode(StandardDdlLexicon.STATEMENTS_CONTAINER);
        assertNotNull(statementsNode);

        // DDL Sequencer creates the 'Tweet' node
        Node tweetNode = statementsNode.getNode("Tweet");
        assertNotNull(tweetNode);
        Node queryNode = verify(tweetNode, Query.ID, Query.ID);
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "twitterview.getTweets");

        // DDL Sequencer create the 'PARTS_VIEWS.PARTS' node
        Node partsNode = statementsNode.getNode("PARTS_VIEWS.PARTS");
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
        Node productNode = statementsNode.getNode("PRODUCT_VIEWS.CustomerOrders");
        assertNotNull(productNode);
        queryNode = verify(productNode, Query.ID, Query.ID);
        selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "name", Function.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "saledate", ElementSymbol.ID);
        verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, 3, "amount", ElementSymbol.ID);
        fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        Node fromClause = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(fromClause, JoinType.Types.JOIN_INNER);
        criteriaNode = verify(fromClause, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyElementSymbol(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "o.customerid");
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "c.id");
        verifyUnaryFromClauseGroup(fromClause, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "o");
        verifyUnaryFromClauseGroup(fromClause, JoinPredicate.LEFT_CLAUSE_REF_NAME, "C");
    }
}
