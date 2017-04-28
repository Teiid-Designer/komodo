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
package org.komodo.modeshape.teiid;

import java.io.IOException;
import java.io.InputStream;
import javax.jcr.Binary;
import javax.jcr.NamespaceRegistry;
import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.teiid.TeiidServiceProvider;
import org.komodo.utils.KLog;
import org.modeshape.common.annotation.NotThreadSafe;
import org.modeshape.common.text.ParsingException;
import org.modeshape.common.util.CheckArg;
import org.modeshape.common.util.IoUtil;
import org.modeshape.jcr.api.nodetype.NodeTypeManager;
import org.modeshape.jcr.api.sequencer.Sequencer;


/**
 * A sequencer for Teiid SQL files.
 */
@NotThreadSafe
public class TeiidSqlSequencer extends Sequencer {

    private static final KLog LOGGER = KLog.getLogger();

    /**
     * @return the teiidVersion
     */
    public TeiidVersion getTeiidVersion() {
        return TeiidVersionProvider.getInstance().getTeiidVersion();
    }

    @Override
    public void initialize(NamespaceRegistry registry, NodeTypeManager nodeTypeManager) throws RepositoryException, IOException {
        registerNodeTypes("cnd/TeiidSql.cnd", nodeTypeManager, true); //$NON-NLS-1$
    }

    @Override
    public boolean execute(Property inputProperty, Node outputNode, Context context) throws Exception {
        Binary sqlContent = inputProperty.getBinary();
        CheckArg.isNotNull(sqlContent, "teiid sql content binary value"); //$NON-NLS-1$

        InputStream stream = sqlContent.getStream();
        try {
            String sql = IoUtil.read(stream);
            TeiidService teiidService = TeiidServiceProvider.getInstance().getTeiidService(getTeiidVersion());
            teiidService.nodeConvert(sql, outputNode);
        } catch (ParsingException e) {
            LOGGER.error(Messages.getString(Messages.TeiidSqlSequencer.ErrorParsingContent), e, e.getLocalizedMessage());
            throw e;
        } catch (Throwable e) {
            LOGGER.error(Messages.getString(Messages.TeiidSqlSequencer.ErrorSequencingContent), e, e.getLocalizedMessage());
            throw new Exception(e);
        } finally {
            stream.close();
        }

        return true;
    }
}
