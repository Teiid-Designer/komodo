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
package org.komodo.teiid;

import javax.jcr.Node;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;
import org.teiid.core.util.ApplicationInfo;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.sql.lang.Command;

public class TeiidServiceImpl implements TeiidService {

    private final String parentBundle;

    private final TeiidVersion version = new DefaultTeiidVersion(ApplicationInfo.getInstance().getReleaseNumber());

    private DataTypeManager dataTypeManager;

    public TeiidServiceImpl(String parentBundle) {
        this.parentBundle = parentBundle;
    }

    @Override
    public TeiidVersion getVersion() {
        return version;
    }

    @Override
    public DataTypeManager getDataTypeManager() {
        if (dataTypeManager == null)
            dataTypeManager = new DataTypeManagerImpl(getVersion());

        return dataTypeManager;
    }

    @Override
    public String getParentBundle() {
        return parentBundle;
    }

    @Override
    public void convert(String sql, Object parent) throws Exception {
        if (! (parent instanceof Node))
            throw new Exception("Parent not an instance of javax.jcr.Node");

        if (sql == null)
            return;

        QueryParser parser = new QueryParser();
        Command command = parser.parseDesignerCommand(sql);
            
        NodeGenerator generator = new NodeGenerator((Node) parent, getDataTypeManager(), version);
        command.acceptVisitor(generator);
        if (generator.errorOccurred())
            throw generator.getError();
    }
    
}
