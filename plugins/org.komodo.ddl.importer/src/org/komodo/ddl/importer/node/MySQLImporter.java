/*************************************************************************************
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
 ************************************************************************************/
package org.komodo.ddl.importer.node;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.mysql.MySqlDdlLexicon;
import org.modeshape.sequencer.ddl.node.AstNode;

/**
 * MySQL DDL Importer
 */
public class MySQLImporter extends StandardImporter {

    /**
     * Create RelationalObject objects
     * @param node the provided AstNode
     * @param model the Model being created
     * @param schema the schema
     * @return the map of AstNodes which need to be deferred
     * @throws Exception 
     */
    @Override
    protected Map<AstNode,RelationalObject> createObject(AstNode node, Model model, Schema schema) throws Exception {
      	Map<AstNode,RelationalObject> deferredMap = new HashMap<AstNode,RelationalObject>();

        if (is(node, MySqlDdlLexicon.TYPE_CREATE_INDEX_STATEMENT)) {
      		deferredMap.put(node, null);
        } else if (is(node, MySqlDdlLexicon.TYPE_CREATE_PROCEDURE_STATEMENT)) {
            createProcedure(node, model);
        } else if (is(node, MySqlDdlLexicon.TYPE_CREATE_FUNCTION_STATEMENT)) {
            createProcedure(node, model).setFunction(true);
        } else
            return super.createObject(node, model, schema);
      	return deferredMap;
    }
    
    /**
     * Create deferred objects using the supplied map
     * @param deferredNodes the map of deferred AstNodes
     * @param model the Model being created
     * @throws Exception 
     */
    @Override
	protected void createDeferredObjects(Map<AstNode,RelationalObject> deferredNodes, Model model) throws Exception {
		Collection<RelationalObject> allRefs = model.getAllObjects();

		// Make first pass to create the PKs
		Set<AstNode> astNodes = deferredNodes.keySet();
		for(AstNode node:astNodes) {
			if (is(node, StandardDdlLexicon.TYPE_TABLE_CONSTRAINT)) {
				Table table = (Table)deferredNodes.get(node);
				createPrimaryKey(node, table, allRefs);
			} else if (is(node, StandardDdlLexicon.TYPE_ALTER_TABLE_STATEMENT)) {
				Table table = find(Table.class, node, null, allRefs);
				for (AstNode node1 : node) {
					if (is(node1, StandardDdlLexicon.TYPE_ADD_TABLE_CONSTRAINT_DEFINITION)) 
						createPrimaryKey(node1, table, allRefs);
				}
			}
		}

		// Second pass create other constraints
		for(AstNode node:astNodes) {
            if (is(node, MySqlDdlLexicon.TYPE_CREATE_INDEX_STATEMENT)) {
                Index index = getFactory().createIndex();
                Info info = createInfo(node, model);
                if (info.getSchema() == null)
                    model.addChild(index);
                else
                    info.getSchema().getIndexes().add(index);

                initialize(index, node, info.getName());
            } else if (is(node, StandardDdlLexicon.TYPE_TABLE_CONSTRAINT)) {
				Table table = (Table)deferredNodes.get(node);
				createConstraint(node, table, allRefs);
			} else if (is(node, StandardDdlLexicon.TYPE_ALTER_TABLE_STATEMENT)) {
				Table table = find(Table.class, node, null, allRefs);
				for (AstNode node1 : node) {
					if (is(node1, StandardDdlLexicon.TYPE_ADD_TABLE_CONSTRAINT_DEFINITION)) 
						createConstraint(node1, table, allRefs);
					else if (is(node1, StandardDdlLexicon.TYPE_ADD_COLUMN_DEFINITION))
						createColumn(node1, table);
				}
			}
		}
    }
    
}
