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

package org.teiid.query.resolver.command;

import org.komodo.spi.query.sql.lang.Command;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.metadata.Table.TriggerEvent;
import org.teiid.query.metadata.TempMetadataAdapter;
import org.teiid.query.resolver.CommandResolver;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.resolver.util.ResolverUtil;
import org.teiid.query.sql.lang.AlterImpl;
import org.teiid.query.sql.lang.AlterProcedureImpl;
import org.teiid.query.sql.lang.AlterTriggerImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.runtime.client.Messages;

/**
 *
 */
public class AlterResolver extends CommandResolver {

	/**
     * @param queryResolver
     */
    public AlterResolver(TCQueryResolver queryResolver) {
        super(queryResolver);
    }

    @Override
	public void resolveCommand(CommandImpl command, TempMetadataAdapter metadata,
			boolean resolveNullLiterals) throws Exception {
		AlterImpl<? extends CommandImpl> alter = (AlterImpl<? extends CommandImpl>)command;
		ResolverUtil.resolveGroup(alter.getTarget(), metadata);
		int type = Command.TYPE_QUERY;
		boolean viewTarget = true;
		if (alter instanceof AlterTriggerImpl) {
			TriggerEvent event = ((AlterTriggerImpl)alter).getEvent();
			switch (event) {
			case DELETE:
				type = Command.TYPE_DELETE;
				break;
			case INSERT:
				type = Command.TYPE_INSERT;
				break;
			case UPDATE:
				type = Command.TYPE_UPDATE;
				break;
			}
		} else if (alter instanceof AlterProcedureImpl) {
			type = Command.TYPE_STORED_PROCEDURE;
			viewTarget = false;
		}
		if (viewTarget && !getQueryResolver().isView(alter.getTarget(), metadata)) {
			 throw new QueryResolverException(Messages.gs(Messages.TEIID.TEIID30116, alter.getTarget()));
		}
		if (alter.getDefinition() != null) {
			getQueryResolver().resolveCommand(alter.getDefinition(), alter.getTarget(), type, metadata.getDesignTimeMetadata(), false);
		}
	}

}
