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
package org.komodo.osgi;

import static org.junit.Assert.assertTrue;
import java.net.URL;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import org.junit.After;
import org.junit.Before;
import org.komodo.spi.constants.StringConstants;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

public abstract class AbstractTestPluginService implements StringConstants {

    /**
     * The prefix of all teiid service bundles
     */
    protected static final String TEIID_BUNDLE_PREFIX = "org.komodo.plugins.teiid" + DOT;

    protected PluginService service;

    protected int bundleCount() throws Exception {
        URL idxUrl = getClass().getClassLoader().getResource(PluginService.INDEX);

        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();
        Document doc = docBuilder.parse(idxUrl.openStream());

        // Use XPath to locate the teiid jar filenames
        XPathFactory xpathFactory = XPathFactory.newInstance();
        XPath xpath = xpathFactory.newXPath();

        // XPath expression to find all the teiid jar filenames
        XPathExpression expr = xpath.compile(PluginService.INDEX_BUNDLE_PATH);

        //evaluate expression result on XML document
        NodeList nodes = (NodeList)expr.evaluate(doc, XPathConstants.NODESET);
        assertTrue(nodes.getLength() > 0);

        return nodes.getLength();
    }

    @Before
    public void setup() throws Exception {
        bundleCount();
    
        if (service == null)
            service = PluginService.getInstance();
    
        service.start();
    }

    @After
    public void teardown() throws Exception {
        if (service == null)
            return;
    
        service.shutdown();
    }

}
