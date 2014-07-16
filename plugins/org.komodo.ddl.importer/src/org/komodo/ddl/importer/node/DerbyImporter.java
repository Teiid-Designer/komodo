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

import org.komodo.relational.model.Column;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.derby.DerbyDdlLexicon;
import org.modeshape.sequencer.ddl.node.AstNode;

/**
 * Derby DDL Importer
 */
public class DerbyImporter extends StandardImporter {


    @Override
    protected Procedure createProcedure(AstNode procedureNode, Model model) throws Exception {
        Procedure procedure = super.createProcedure(procedureNode, model);

        for (AstNode child : procedureNode) {
            if (! is(child, DerbyDdlLexicon.TYPE_FUNCTION_PARAMETER))
                continue;

            Parameter prm = procedure.createParameter();
            initialize(prm, child);
            String datatype = child.getProperty(StandardDdlLexicon.DATATYPE_NAME).toString();
            prm.setNativeType(datatype);

            String teiidType = getTeiidDataTypeName(datatype);
            prm.setDatatypeName(teiidType);

            Object prop = child.getProperty(StandardDdlLexicon.DATATYPE_LENGTH);
            if (prop != null)
                prm.setLength(Integer.parseInt(prop.toString()));

            prop = child.getProperty(StandardDdlLexicon.DATATYPE_PRECISION);
            if (prop != null)
                prm.setPrecision(Integer.parseInt(prop.toString()));

            prop = child.getProperty(StandardDdlLexicon.DATATYPE_SCALE);
            if (prop != null)
                prm.setScale(Integer.parseInt(prop.toString()));

            prop = child.getProperty(StandardDdlLexicon.NULLABLE);
            if (prop != null)
                //prm.setNullable(prop.toString().equals("NULL") ? NullableType.NULLABLE_LITERAL : NullableType.NO_NULLS_LITERAL); //$NON-NLS-1$
            	prm.setNullable(prop.toString()); 

            prop = child.getProperty(StandardDdlLexicon.DEFAULT_VALUE);
            if (prop != null)
                prm.setDefaultValue(prop.toString());
        }

        return procedure;
    }
    
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

      	if (is(node, DerbyDdlLexicon.TYPE_CREATE_INDEX_STATEMENT)) {
      		deferredMap.put(node, null);
        } else if (is(node, DerbyDdlLexicon.TYPE_CREATE_PROCEDURE_STATEMENT))  {
            createProcedure(node, model);
        } else if (is(node, DerbyDdlLexicon.TYPE_CREATE_FUNCTION_STATEMENT)) {
            createProcedure(node, model).setFunction(true);
        } else {
            return super.createObject(node, model, schema);
        }
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
          	if (is(node, DerbyDdlLexicon.TYPE_CREATE_INDEX_STATEMENT)) {
                Index index = getFactory().createIndex();
                Info info = createInfo(node, model);
                if (info.getSchema() == null)
                    model.addChild(index);
                else
                    info.getSchema().getIndexes().add(index);

                initialize(index, node, info.getName());
                Object prop = node.getProperty(DerbyDdlLexicon.UNIQUE_INDEX);
                if (prop != null)
                    index.setUnique((Boolean) prop);
     
                prop = node.getProperty(DerbyDdlLexicon.TABLE_NAME);
                if (prop == null)
                    return;

                try {
                    Table table = find(Table.class, prop.toString(), node, null, allRefs);
                    for (AstNode node1 : node) {
                        // Probably need to check for a simple column reference for Oracle
                        if (node1.hasMixin(DerbyDdlLexicon.TYPE_INDEX_COLUMN_REFERENCE)) {
                            try {
                                index.getColumns().add(find(Column.class, node1, table, allRefs));
                            } catch (EntityNotFoundException error) {
                                addProgressMessage(error.getMessage());
                            }
                        }
                    }
                } catch (EntityNotFoundException error) {
                    addProgressMessage(error.getMessage());
                }
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
