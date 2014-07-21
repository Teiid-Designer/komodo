/*
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
*/
package org.teiid.query.validator;

import org.teiid.core.types.DataTypeManagerService;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.parser.TeiidParser;
import org.teiid.query.sql.AbstractTestFactory;
import org.teiid.query.unittest.RealMetadataFactory;

/**
 *
 */
public abstract class AbstractTest {

    private final ITeiidVersion teiidVersion;

    private final DataTypeManagerService dataTypeManager;

    private final QueryParser queryParser;

    private final RealMetadataFactory metadataFactory;

    /**
     * @param teiidVersion
     */
    public AbstractTest(ITeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
        this.dataTypeManager = DataTypeManagerService.getInstance(teiidVersion);
        this.queryParser = new QueryParser(teiidVersion);
        this.metadataFactory = new RealMetadataFactory(teiidVersion);
    }

    /**
     * @return the teiidVersion
     */
    public ITeiidVersion getTeiidVersion() {
        return this.teiidVersion;
    }

    /**
     * @return the queryParser
     */
    public QueryParser getQueryParser() {
        return this.queryParser;
    }

    /**
     * @return the teiid parser
     */
    public TeiidParser getTeiidParser() {
        return getQueryParser().getTeiidParser();
    }

    /**
     * @return the metadataFactory
     */
    public RealMetadataFactory getMetadataFactory() {
        return this.metadataFactory;
    }

    /**
     * @return the dataTypeManager
     */
    public DataTypeManagerService getDataTypeManager() {
        return this.dataTypeManager;
    }

    protected abstract AbstractTestFactory getFactory();
}
