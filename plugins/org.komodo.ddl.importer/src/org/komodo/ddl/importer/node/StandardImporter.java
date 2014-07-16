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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.komodo.ddl.importer.ImportMessages;
import org.komodo.ddl.importer.ImportOptions;
import org.komodo.ddl.importer.Messages.DDL_IMPORTER;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.utils.KomodoCoreRuntimeException;
import org.komodo.utils.Messages;
import org.komodo.utils.ModelType;
import org.komodo.utils.StringNameValidator;
import org.modeshape.sequencer.ddl.DdlConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.node.AstNode;

/**
 * Node importer for standard DDL
 */
public class StandardImporter extends AbstractImporter {

	private int DEFAULT_STRING_LENGTH = 10;
	
	class Info {

		protected Schema schema;
		protected String name;

		public Info(AstNode node, Model model) throws Exception {
			init(node, model);
		}

		protected void init(AstNode node, Model model) throws Exception {
			String name = node.getName();
			int ndx = name.lastIndexOf('.');
			if (ndx >= 0) {
				schema = null;
				this.name = removeLeadingTrailingTicks(name.substring(ndx+1));
			} else {
				schema = null;
				this.name = removeLeadingTrailingTicks(name);
			}
		}

		protected String removeLeadingTrailingTicks(String name) {
			String resultName = name;
			if(name!=null && name.length()>2 & name.startsWith("`") && name.endsWith("`")) { //$NON-NLS-1$ //$NON-NLS-2$
				resultName = name.substring(1, name.length()-1);
			}
			return resultName;
		}
		
		/**
		 * @return the name
		 */   
		public String getName() {
			return this.name;
		}

		/**
		 * @return the schema
		 */
		public Schema getSchema() {
			return schema;
		}
	}

	private static final String STRING_TYPENAME = "string"; //$NON-NLS-1$

	/**
	 * Create new info object
	 *
	 * @param node the AstNode
	 * @param model the Model
	 *
	 * @return new info object
	 *
	 * @throws Exception
	 */
	protected Info createInfo(AstNode node, Model model) throws Exception {
		return new Info(node, model);
	}

	/**
	 * @param type type of RelationalObject to find
	 * @param name the node name
	 * @param node the AstNode
	 * @param parent the parent reference
	 * @param allModelRefs the collection of all model RelationalObjects
	 *
	 * @return RelationalObject which is a match
	 *
	 * @throws EntityNotFoundException
	 * @throws KomodoCoreRuntimeException
	 */
	protected <T extends RelationalObject> T find(Class<T> type, String name, AstNode node,
			RelationalObject parent, Collection<RelationalObject> allModelRefs) throws EntityNotFoundException, KomodoCoreRuntimeException {
		
		// Look through all refs list for a matching object
		for ( RelationalObject obj : allModelRefs) {
			if (type.isInstance(obj)) {
				T relEntity = (T)obj;
				if (relEntity.getName().equalsIgnoreCase(name)) {
					RelationalObject relParent = relEntity.getParent();
					if(parent!=null) {
						if(relParent.getName().equalsIgnoreCase(parent.getName())) {
							return relEntity;
						}
					} else {
						return relEntity;
					}
				}
			}
		}

		while (node.getProperty(StandardDdlLexicon.DDL_EXPRESSION) == null) {
			node = node.getParent();
		}

		throw new EntityNotFoundException(Messages.getString(DDL_IMPORTER.entityNotFoundMsg,
				type.getSimpleName(),
				name,
				Messages.getString(DDL_IMPORTER.MODEL),
				parent == null ? getImportOptions().getModelName() : parent.getName(),
						node.getProperty(StandardDdlLexicon.DDL_START_LINE_NUMBER).toString(),
						node.getProperty(StandardDdlLexicon.DDL_START_COLUMN_NUMBER).toString()));
	}

	/**
	 * @param type type of RelationalObject to find
	 * @param node the AstNode
	 * @param parent the parent reference
	 * @param allModelRefs the collection of all model RelationalObjects
	 * @return RelationalObject which is a match
	 *
	 * @throws EntityNotFoundException
	 * @throws KomodoCoreRuntimeException
	 */
	protected <T extends RelationalObject> T find(Class<T> type, AstNode node, RelationalObject parent, Collection<RelationalObject> allModelRefs) throws EntityNotFoundException, KomodoCoreRuntimeException {
		String nodeName = node.getName();
		int indx = nodeName.lastIndexOf('.');
		if(indx>=0) {
			nodeName = nodeName.substring(indx+1);
		}
		return find(type, nodeName, node, parent, allModelRefs);
	}

	/**
	 * Initialize the RelationalObject
	 * @param entity the object
	 * @param node the corresponding AstNode
	 * @param name the name for the object
	 */
	protected void initialize(RelationalObject entity, AstNode node, String name) {
		entity.setName(name);
		entity.setNameInSource(name);

		// descriptions must wait to be set until container and model type has been set
		if (getImportOptions().isSetModelEntityDescription()) {
			Object prop = node.getProperty(StandardDdlLexicon.DDL_EXPRESSION);
			if (prop != null) {
				entity.setDescription(prop.toString());
			} else {
				entity.setDescription(""); //$NON-NLS-1$
			}
		}
	}


	/**
	 * Initialize the RelationalObject
	 * @param entity the object
	 * @param node the corresponding AstNode
	 */
	protected void initialize(RelationalObject entity, AstNode node) {
		initialize(entity, node, node.getName());
	}

	/**
	 * Helper method for creating unique FK names
	 * @param currentFKs the List of ForeignKeys currently on the table
	 * @param newFKName the proposed name for the new FK
	 * @return the unique name - generated from the proposed name
	 */
	protected String getUniqueFKName(Collection<ForeignKey> currentFKs, String newFKName) {
		// If current list is empty, no need to check names
		if (currentFKs == null || currentFKs.isEmpty()) return newFKName;

		// Use name validator for unique name generation
		StringNameValidator nameValidator = new StringNameValidator();

		// Add the current FK names to the validator
		for (ForeignKey fk : currentFKs) {
			nameValidator.addExistingName(fk.getName());
		}

		// Make the proposed name unique
		return nameValidator.createValidUniqueName(newFKName);
	}

	/**
	 * Initialize a ForeignKey
	 * @param currentFKs collection of current FKs
	 * @param key a ForeignKey
	 * @param node corresponding AstNode
	 */
	protected void initializeFK(Collection<ForeignKey> currentFKs, ForeignKey key, AstNode node) {
		// Get Name from DDL node
		String fkName = node.getName();
		// Make sure not to add duplicate FK names
		String uniqueName = getUniqueFKName(currentFKs, fkName);

		initialize(key, node, uniqueName);
	}

	/**
	 * Initialize a Table
	 * @param table the Table to init
	 * @param node corresponding AstNode
	 * @param model the Model
	 * @return the initialized Table
	 * @throws Exception
	 */
	protected <T extends Table> T initializeTable(T table, AstNode node, Model model) throws Exception {
		Info info = createInfo(node, model);
		if (info.getSchema() == null)
			model.addChild(table);
		else
			info.getSchema().getTables().add(table);

		initialize(table, node, info.getName());
		return table;
	}

	/**
	 * Handle a statement OPTION key for Column for DDL
	 *
	 * @param column the Column
	 * @param columnOptionNode a statementOption node for a column
	 */
	protected void handleColumnOption(Column column, AstNode columnOptionNode) {
		// Do nothing
	}

	/**
	 * Handle the OPTION keys that may be set on Column for DDL
	 *
	 * @param column the Column
	 * @param columnNode the column AstNode
	 */
	protected void handleColumnOptions(Column column, AstNode columnNode) {
		List<AstNode> children = columnNode.getChildren();
		for(AstNode child: children) {
			if(is(child, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
				handleColumnOption(column,child);
			}
		}
	}

	/**
	 * @param datatype
	 *
	 * @return {@link EObject} represented by the given data type id
	 * @throws Exception
	 */
	protected String getTeiidDataTypeName(String datatype) throws Exception {
		String typeName = "string"; //$NON-NLS-1$
//		EObject dataType = RelationalTypeMappingImpl.getInstance().getDatatype(datatype);
//		if(dataType!=null) {
//			typeName = ModelerCore.getWorkspaceDatatypeManager().getName(dataType);
//		}
		return typeName;
	}

	/**
	 * Create Column from the provided AstNode within ColumnSet
	 * @param node the provided AstNode
	 * @param table the ColumnSet in which to create the column
	 * @return the column
	 *
	 * @throws Exception 
	 */
	protected Column createColumn(AstNode node, Table table) throws Exception {
		Column col = table.createColumn();
		initialize(col, node);

		String datatype = node.getProperty(StandardDdlLexicon.DATATYPE_NAME).toString();
		col.setNativeType(datatype);

		String teiidType = getTeiidDataTypeName(datatype);
		col.setDatatypeName(teiidType);

		// Datatype length
		Object prop = node.getProperty(StandardDdlLexicon.DATATYPE_LENGTH);
		if (prop != null) {
			col.setLength(Integer.parseInt(prop.toString()));
		} else {
			// Length is not provided for type 'string', use the default length specified in preferences...
			//String dtName = ModelerCore.getWorkspaceDatatypeManager().getName(type);
			if(teiidType != null && teiidType.equalsIgnoreCase(STRING_TYPENAME)) {
//				col.setLength(ModelerCore.getTransformationPreferences().getDefaultStringLength());
				col.setLength(DEFAULT_STRING_LENGTH);
			}
		}

		prop = node.getProperty(StandardDdlLexicon.DATATYPE_PRECISION);
		if (prop != null)
			col.setPrecision(Integer.parseInt(prop.toString()));

		prop = node.getProperty(StandardDdlLexicon.DATATYPE_SCALE);
		if (prop != null)
			col.setScale(Integer.parseInt(prop.toString()));

		prop = node.getProperty(StandardDdlLexicon.NULLABLE);
		if (prop != null)
			col.setNullable(getRelRefNullable(prop.toString())); 

		prop = node.getProperty(StandardDdlLexicon.DEFAULT_VALUE);
		if (prop != null)
			col.setDefaultValue(prop.toString());

		return col;
	}

	/**
	 * Create ProcedureParameter from the provided AstNode within procedure
	 * @param node the provided AstNode
	 * @param procedure the Procedure in which to create the procedure parameter
	 * @return the procedure parameter
	 *
	 * @throws Exception 
	 */
	protected Parameter createProcedureParameter(AstNode node, Procedure procedure) throws Exception {
		Parameter prm = procedure.createParameter();
		initialize(prm, node);

		String datatype = node.getProperty(StandardDdlLexicon.DATATYPE_NAME).toString();
		prm.setNativeType(datatype);

		String teiidType = getTeiidDataTypeName(datatype);
		prm.setDatatypeName(teiidType);

		// Datatype length
		Object prop = node.getProperty(StandardDdlLexicon.DATATYPE_LENGTH);
		if (prop != null) {
			prm.setLength(Integer.parseInt(prop.toString()));
		} else {
			// Length is not provided for type 'string', use the default length specified in preferences...
			//String dtName = ModelerCore.getWorkspaceDatatypeManager().getName(type);
			if(teiidType != null && teiidType.equalsIgnoreCase(STRING_TYPENAME)) {
//				prm.setLength(ModelerCore.getTransformationPreferences().getDefaultStringLength());
				prm.setLength(DEFAULT_STRING_LENGTH);
			}
		}

		prop = node.getProperty(StandardDdlLexicon.DATATYPE_PRECISION);
		if (prop != null)
			prm.setPrecision(Integer.parseInt(prop.toString()));

		prop = node.getProperty(StandardDdlLexicon.DATATYPE_SCALE);
		if (prop != null)
			prm.setScale(Integer.parseInt(prop.toString()));

		prop = node.getProperty(StandardDdlLexicon.NULLABLE);
		if (prop != null)
			prm.setNullable(getRelRefNullable(prop.toString())); 

		prop = node.getProperty(StandardDdlLexicon.DEFAULT_VALUE);
		if (prop != null)
			prm.setDefaultValue(prop.toString());

		return prm;
	}

	/**
	 * Create a PrimaryKey
	 * @param node the AstNode representing the primary key
	 * @param table the parent Table
	 * @param allRefs the Collection of all RelationalObject objects in the model
	 *
	 * @throws KomodoCoreRuntimeException
	 */
	protected void createPrimaryKey(AstNode node, Table table, Collection<RelationalObject> allRefs) throws KomodoCoreRuntimeException {
		String type = node.getProperty(StandardDdlLexicon.CONSTRAINT_TYPE).toString();
		if (DdlConstants.PRIMARY_KEY.equals(type)) {
			PrimaryKey key = getFactory().createPrimaryKey();
			table.setPrimaryKey(key);
			initialize(key, node);

			for (AstNode node1 : node) {
				if (is(node1, StandardDdlLexicon.TYPE_COLUMN_REFERENCE)) {
					try {
						Column column = find(Column.class, node1, table, allRefs);
						//                        if (column.getNullable() == NullableType.NULLABLE_UNKNOWN_LITERAL
						//                            || column.getNullable() == NullableType.NULLABLE_LITERAL) {
						//                            column.setNullable(NullableType.NO_NULLS_LITERAL);
						//                        }
						key.getColumns().add(column);
					} catch (EntityNotFoundException error) {
						addProgressMessage(error.getMessage());
					}
				}
			}
		}
	}
	
	/**
	 * Create a Constraint
	 * @param node the AstNode representing the constraint
	 * @param table the parent Table
	 * @param allRefs the Collection of all RelationalObject objects in the model
	 *
	 * @throws KomodoCoreRuntimeException
	 */
	protected void createConstraint(AstNode node, Table table, Collection<RelationalObject> allRefs) throws KomodoCoreRuntimeException {
		String type = node.getProperty(StandardDdlLexicon.CONSTRAINT_TYPE).toString();
		if (DdlConstants.FOREIGN_KEY.equals(type)) {
			ForeignKey key = getFactory().createForeignKey();
			initializeFK(table.getForeignKeys(), key, node);
			table.getForeignKeys().add(key);
			Table foreignTable = null;
			Set<Column> foreignColumns = new HashSet<Column>();

			for (AstNode node1 : node) {
				try {
					if (is(node1, StandardDdlLexicon.TYPE_COLUMN_REFERENCE))
						key.getColumns().add(find(Column.class, node1, table, allRefs));
					else if (is(node1, StandardDdlLexicon.TYPE_TABLE_REFERENCE))
						foreignTable = find(Table.class, node1, null, allRefs);
					else if (is(node1, StandardDdlLexicon.TYPE_FK_COLUMN_REFERENCE) && foreignTable != null) {
						foreignColumns.add(find(Column.class, node1, foreignTable, allRefs));
					}
				} catch (Exception error) {
					addProgressMessage(error.getMessage());
				}
			}

			if (foreignTable == null)
				return;

			PrimaryKey primaryKey = foreignTable.getPrimaryKey();
			Collection<Column> primaryKeyColumns = primaryKey.getColumns();
			if (foreignColumns.isEmpty()) {
				key.setUniqueKeyName(primaryKey.getName());
				key.setUniqueKeyTableName(foreignTable.getName());
			}
			if (primaryKeyColumns.containsAll(foreignColumns) && primaryKeyColumns.size() == foreignColumns.size()) {
				key.setUniqueKeyName(primaryKey.getName());
				key.setUniqueKeyTableName(foreignTable.getName());
			} else {
				for (Object obj : foreignTable.getUniqueConstraints()) {
					UniqueConstraint uniqueKey = (UniqueConstraint)obj;
					Collection<Column> uniqueKeyColumns = uniqueKey.getColumns();
					if (uniqueKeyColumns.containsAll(foreignColumns) && uniqueKeyColumns.size() == foreignColumns.size()) {
						key.setUniqueKeyName(uniqueKey.getName());
						key.setUniqueKeyTableName(foreignTable.getName());
						break;
					}
				}
			}

		} else if (DdlConstants.UNIQUE.equals(type)) {
			UniqueConstraint key = getFactory().createUniqueConstraint();
			table.getUniqueConstraints().add(key);
			initialize(key, node);

			for (AstNode node1 : node) {
				if (! is(node1, StandardDdlLexicon.TYPE_COLUMN_REFERENCE))
					continue;

				try {
					Column column = find(Column.class, node1, table, allRefs);

					//                    if (column.getNullable() == NullableType.NULLABLE_UNKNOWN_LITERAL || column.getNullable() == NullableType.NULLABLE_LITERAL) {
						//                        column.setNullable(NullableType.NO_NULLS_LITERAL);
						//                    }
					key.getColumns().add(column);
				} catch (Exception error) {
					addProgressMessage(error.getMessage());
				}
			}
		}
	}

	/**
	 * Create a Procedure
	 * @param procedureNode the AstNode for the procedure
	 * @param model the Model
	 * @return the Procedure
	 *
	 * @throws Exception
	 */
	protected Procedure createProcedure( AstNode procedureNode, Model model) throws Exception {
		Procedure procedure = getFactory().createProcedure();
		Info info = createInfo(procedureNode, model);
		if (info.getSchema() == null)
			model.addChild(procedure);
		else {
			info.getSchema().getProcedures().add(procedure);
			procedure.setParent(info.getSchema());
		}

		initialize(procedure, procedureNode, info.getName());
		// TODO: determine how to handle Procedure StatementOption
		// TODO: determine how to handle Procedure Statement

		if (procedureNode.getProperty(StandardDdlLexicon.DATATYPE_NAME) != null) {
			ProcedureResultSet result = getFactory().createProcedureResultSet();
			procedure.setResultSet(result);
			initialize(result, procedureNode);
		}

		return procedure;
	}

	/**
	 * Perform the import
	 * @param rootNode the rootNode of the DDL
	 * @param importOptions the import Options for this import
	 * @param importMessages the import Messages for this import
	 * @return the Model created
	 * @throws Exception the exception
	 */
	@Override
	public Model importNode(AstNode rootNode, ImportOptions importOptions, ImportMessages importMessages) throws Exception {

		this.importOptions = importOptions;
		this.importMessages = importMessages;

		// Create a Model for the imported DDL
		Model model = getFactory().createModel(importOptions.getModelName()); 

		// Map for holding deferred nodes, which much be created later
		Map<AstNode,RelationalObject> deferredCreateMap = new HashMap<AstNode,RelationalObject>();

		// Create objects from the DDL.  (populated map of deferred nodes)
		for (AstNode node : rootNode) {
			if (is(node, StandardDdlLexicon.TYPE_CREATE_SCHEMA_STATEMENT)) {
				Schema schema = getFactory().createSchema();
				model.addChild(schema);
				initialize(schema, node);
				for (AstNode node1 : node) {
					Map<AstNode,RelationalObject> deferredMap = createObject(node1, model, schema);
					if(!deferredMap.isEmpty()) {
						deferredCreateMap.putAll(deferredMap);
					}
				}
			} else {
				Map<AstNode,RelationalObject> deferredMap = createObject(node, model, null);
				if(!deferredMap.isEmpty()) {
					deferredCreateMap.putAll(deferredMap);
				}
			}
		}

		// Now process all the 'deferred' nodes.  These are nodes which reference other nodes (which are required to exist first)
		createDeferredObjects(deferredCreateMap,model);

		return model;
	}

	/**
	 * Create RelationalObject objects
	 * @param node the provided AstNode
	 * @param model the Model being created
	 * @param schema the schema
	 * @return the map of AstNodes which need to be deferred
	 * @throws Exception 
	 */
	protected Map<AstNode,RelationalObject> createObject(AstNode node, Model model, Schema schema) throws Exception {
		Map<AstNode,RelationalObject> deferredMap = new HashMap<AstNode,RelationalObject>();

		// -----------------------------------------------------------------------
		// Standard DDL 
		// -----------------------------------------------------------------------
		if (is(node, StandardDdlLexicon.TYPE_CREATE_TABLE_STATEMENT)) {
			Table table = initializeTable(getFactory().createTable(), node, model);
			for (AstNode child : node) {
				if (is(child, StandardDdlLexicon.TYPE_COLUMN_DEFINITION))
					createColumn(child, table);
				else if (is(child, StandardDdlLexicon.TYPE_TABLE_CONSTRAINT)) {
					deferredMap.put(child, table);
				}
			}
		} else if (is(node, StandardDdlLexicon.TYPE_CREATE_VIEW_STATEMENT)) {
			if (getImportOptions().getModelType().toString() != ModelType.Type.VIRTUAL.toString() 
					&& getImportOptions().isCreateModelEntitiesForUnsupportedDdl())

				initializeTable(getFactory().createView(), node, model);

		} else if (is(node, StandardDdlLexicon.TYPE_ALTER_TABLE_STATEMENT)) {
			deferredMap.put(node, null);
		// Unhandled node - get the mixin type and increment the count
		} else {
			StringBuffer sb = new StringBuffer();
			List<String> mixins = node.getMixins();
			Iterator<String> iter = mixins.iterator();
			while(iter.hasNext()) {
				String mixin = iter.next();
				sb.append(mixin);
				if(iter.hasNext()) sb.append(","); //$NON-NLS-1$
			}
			getImportMessages().incrementUnhandledNodeType(sb.toString());
		}
		return deferredMap;
	}

	/**
	 * Create deferred objects using the supplied map
	 * @param deferredNodes the map of deferred AstNodes
	 * @param model the Model being created
	 * @throws Exception 
	 */
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
			if (is(node, StandardDdlLexicon.TYPE_TABLE_CONSTRAINT)) {
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
	
	/**
	 * Get the RelationalObject nullable string for the provided ast nullable property value
	 * @param astNullableStr
	 * @return RelationalObject nullable string
	 */
	protected String getRelRefNullable(String astNullableStr) {
		String nullableStr = "NULLABLE_UNKNOWN"; //$NON-NLS-1$
		if(astNullableStr!=null) {
			if(astNullableStr.equalsIgnoreCase("null")) { //$NON-NLS-1$
				nullableStr = "NULLABLE"; //$NON-NLS-1$
			} else if(astNullableStr.equalsIgnoreCase("not null")) { //$NON-NLS-1$
				nullableStr = "NO_NULLS"; //$NON-NLS-1$
			}
		}
		return nullableStr;
	}
    
}
