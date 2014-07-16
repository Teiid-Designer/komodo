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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.komodo.ddl.importer.ImportMessages;
import org.komodo.ddl.importer.ImportOptions;
import org.komodo.ddl.importer.Messages;
import org.komodo.ddl.importer.Messages.DDL_IMPORTER;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.core.DataTypes;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.ddl.TeiidDDLConstants;
import org.komodo.utils.KomodoCoreRuntimeException;
import org.komodo.utils.StringUtil;
import org.modeshape.sequencer.ddl.DdlConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.node.AstNode;

/**
 * Teiid DDL node importer
 */
public class TeiidDdlImporter extends StandardImporter {

	private static final String NS_TEIID_ODATA = "teiid_odata"; //$NON-NLS-1$
	private static final String NS_TEIID_WEBSERVICE= "teiid_ws"; //$NON-NLS-1$
	private static final String NS_TEIID_MONGO = "teiid_mongo"; //$NON-NLS-1$
	private static final String NS_TEIID_SALESFORCE = "teiid_sf"; //$NON-NLS-1$
	private static final String NS_TEIID_RELATIONAL = "teiid_rel"; //$NON-NLS-1$
	private static final String NS_TEIID_ACCUMULO = "teiid_accumulo"; //$NON-NLS-1$
    private static final String NS_TEIID_EXCEL = "teiid_excel"; //$NON-NLS-1$
    private static final String NS_TEIID_JPA = "teiid_jpa"; //$NON-NLS-1$
	private static final String NS_DESIGNER_ODATA = "odata"; //$NON-NLS-1$
	private static final String NS_DESIGNER_WEBSERVICE= "ws"; //$NON-NLS-1$
	private static final String NS_DESIGNER_MONGO = "mongo"; //$NON-NLS-1$
	private static final String NS_DESIGNER_SALESFORCE = "salesforce"; //$NON-NLS-1$
	private static final String NS_DESIGNER_RELATIONAL = "relational"; //$NON-NLS-1$
	private static final String NS_DESIGNER_ACCUMULO = "accumulo"; //$NON-NLS-1$
    private static final String NS_DESIGNER_EXCEL = "excel"; //$NON-NLS-1$
    private static final String NS_DESIGNER_JPA = "jpa2"; //$NON-NLS-1$
	
	static int DEFAULT_NULL_VALUE_COUNT = -1;
	
	/*
	 *         return !(type == Types.LONGVARBINARY || type == Types.LONGVARCHAR || type == Types.VARBINARY || type == Types.VARCHAR
                 || type == Types.ARRAY || type == Types.BLOB || type == Types.CLOB);
	 */
	
	private class TeiidInfo extends Info {

		/**
		 * @param node the AstNode
		 * @param model the Model
		 *
		 * @throws Exception the exception
		 */
		public TeiidInfo(AstNode node, Model model) throws Exception {
			super(node, model);
		}

		@Override
		protected void init(AstNode node, Model model) throws Exception {
			String name = node.getName();
			int ndx = name.lastIndexOf('.');
			if (ndx >= 0) {
				this.schema = null;
				this.name = removeLeadingTrailingTicks(name.substring(ndx + 1));
			} else
				super.init(node, model);
		}
	}
	
	@Override
	protected TeiidInfo createInfo(AstNode node, Model model) throws Exception {
		return new TeiidInfo(node, model);
	}

	@Override
	protected String getTeiidDataTypeName(String datatype) throws Exception {
		String resultTypeName = "string";  //$NON-NLS-1$

//		/*
//		 * Get the Datatype for Teiid DDL.
//		 * First tries to match the datatype string with a teiid built-in type.
//		 * If a built-in type is not found, then attempt to use the relational mapping to find a match.
//		 */
//
//		// Look up matching Built-In type
//		EObject[] builtInTypes = ModelerCore.getWorkspaceDatatypeManager().getAllDatatypes();
//		String dtName = null;
//		for (int i = 0; i < builtInTypes.length; i++) {
//			dtName = ModelerCore.getWorkspaceDatatypeManager().getName(builtInTypes[i]);
//			if (dtName != null && dtName.equalsIgnoreCase(datatype)) {
//				resultTypeName = dtName;
//				break;
//			}
//		}
//
//		// Built In type not found, try mapping from native to built-in
//		if(resultTypeName == null) {
//			resultTypeName = super.getTeiidDataTypeName(datatype);
//		}

		return resultTypeName;
	}

	/**
	 * Creates constraints for Table for Teiid DDL
	 * @param constraintNode the AstNode for the constraint
	 * @param table the Table object
	 * @param model the Model
	 * @param allRefs the collection of all RelationalObject objects in the model
	 *
	 * @throws KomodoCoreRuntimeException
	 */
	private void createConstraint(AstNode constraintNode, Table table, Model model, Collection<RelationalObject> allRefs) throws KomodoCoreRuntimeException {

		String type = constraintNode.getProperty(TeiidDdlLexicon.Constraint.TYPE).toString();
		boolean primaryKeyConstraint = false;
		boolean uniqueConstraint = false;
		boolean accessPatternConstraint = false;
		boolean foreignKeyConstraint = false;
		boolean indexConstraint = false;
		RelationalObject key = null;

		if (DdlConstants.PRIMARY_KEY.equals(type)) {
			key = getFactory().createPrimaryKey();
			initialize(key, constraintNode);
			table.setPrimaryKey((PrimaryKey)key);
			primaryKeyConstraint = true;
		} else if (DdlConstants.INDEX.equals(type)) {
			// TODO need to process teiidddl:expression property
			key = getFactory().createIndex();
			initialize(key, constraintNode);
			model.addChild(key);
			indexConstraint = true;
		} else if (DdlConstants.UNIQUE.equals(type)) {
			key = getFactory().createUniqueConstraint();
			initialize(key, constraintNode);
			table.getUniqueConstraints().add((UniqueConstraint)key);
			uniqueConstraint = true;
		} else if (TeiidDdlConstants.TeiidNonReservedWord.ACCESSPATTERN.toDdl().equals(type)) {
			key = getFactory().createAccessPattern();
			initialize(key, constraintNode);
			table.getAccessPatterns().add((AccessPattern)key);
			accessPatternConstraint = true;
		} else if (DdlConstants.FOREIGN_KEY.equals(type)) {
			key = getFactory().createForeignKey();
			initializeFK(table.getForeignKeys(), (ForeignKey)key, constraintNode);
			table.getForeignKeys().add((ForeignKey)key);
			foreignKeyConstraint = true;
		} else {
			assert false : "Unexpected constraint type of '" + type + "'"; //$NON-NLS-1$ //$NON-NLS-2$
		}

		// process referenced columns multi-valued property
		Object temp = constraintNode.getProperty(TeiidDdlLexicon.Constraint.REFERENCES);
		List<AstNode> references = (List<AstNode>)temp;

		for (AstNode ref : references) {
			try {
				Column col = find(Column.class, ref, table, allRefs);
				if(col!=null) {
					if (primaryKeyConstraint) {
						((PrimaryKey)key).getColumns().add(col);
					} else if (uniqueConstraint) {
						((UniqueConstraint)key).getColumns().add(col);
					} else if (accessPatternConstraint) {
						((AccessPattern)key).getColumns().add(col);
					} else if (foreignKeyConstraint) {
						((ForeignKey)key).getColumns().add(col);
					} else if (indexConstraint) {
						((Index)key).getColumns().add(col);
					}else {
						assert false : "Unexpected constraint type of '" + type + "'"; //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			} catch (EntityNotFoundException error) {
				addProgressMessage(error.getMessage());
			}
		}

		// special processing for foreign key
		if (foreignKeyConstraint) {
			ForeignKey foreignKey = (ForeignKey)key;

			AstNode tableRefNode = (AstNode)constraintNode.getProperty(TeiidDdlLexicon.Constraint.TABLE_REFERENCE);
			if(tableRefNode==null) {
				addProgressMessage(Messages.getString(DDL_IMPORTER.fkTableRefNotFoundMsg)+" '"+foreignKey.getName()+"'"); //$NON-NLS-1$ //$NON-NLS-2$
				return;
			}

			try {
				Table tableRef = find(Table.class, tableRefNode, null, allRefs);
				PrimaryKey tableRefPrimaryKey = tableRef.getPrimaryKey();
				Collection<Column> primaryKeyColumns = tableRef.getColumns();
				// check to see if foreign table columns are referenced
				Object tempRefColumns = constraintNode.getProperty(TeiidDdlLexicon.Constraint.TABLE_REFERENCE_REFERENCES);

				List<AstNode> foreignTableColumnNodes = (tempRefColumns==null) ? Collections.<AstNode>emptyList() : (List<AstNode>)tempRefColumns;
				int numPKColumns = primaryKeyColumns.size();
				int numFKColumns = foreignTableColumnNodes.size();

				//TODO: Can we just set PK object on the FK, instead of the Names???   
				if( foreignTableColumnNodes.isEmpty() ) {
					if(tableRefPrimaryKey!=null) {
						foreignKey.setUniqueKeyName(tableRefPrimaryKey.getName());
						foreignKey.setUniqueKeyTableName(tableRef.getName());
					}
				} else if( numPKColumns == numFKColumns ) {
					//TODO: not sure what this is doing
					for(AstNode fTableColumn : foreignTableColumnNodes) {
						find(Column.class, fTableColumn, tableRef, allRefs);
					}
					foreignKey.setUniqueKeyName(tableRefPrimaryKey.getName());
					foreignKey.setUniqueKeyTableName(tableRef.getName());
				} else {
					foreignKey.setUniqueKeyName(tableRefPrimaryKey.getName());
					foreignKey.setUniqueKeyTableName(tableRef.getName());
				}
			} catch (EntityNotFoundException error) {
				addProgressMessage(error.getMessage());
			}
		}
	}

	@Override
	protected Column createColumn(AstNode node, Table table) throws Exception {
		Column column = super.createColumn(node, table);

		// Handle Teiid-specific properties and options
		Object prop = node.getProperty(TeiidDdlLexicon.CreateTable.AUTO_INCREMENT);
		if(prop != null)
			column.setAutoIncremented(((Boolean)prop).booleanValue());

		// Find all the Option properties
		List<AstNode> optionNodes = new ArrayList<AstNode>();
		List<AstNode> children = node.getChildren();
		for(AstNode child: children) {
			if(is(child, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
				optionNodes.add(child);
			}
		}

		// process the Column Options
		processOptions(optionNodes,column);

		return column;
	}

	@Override
	protected Procedure createProcedure(AstNode procedureNode, Model model) throws Exception {
		Procedure procedure = super.createProcedure(procedureNode, model);

		List<AstNode> procOptionNodes = new ArrayList<AstNode>();

		for (AstNode child : procedureNode) {
			if (is(child, TeiidDdlLexicon.CreateProcedure.PARAMETER)) {
				createProcedureParameter(child, procedure);
			} else if(is(child, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMNS)) {
				ProcedureResultSet result = getFactory().createProcedureResultSet();
				procedure.setResultSet(result);
				initialize(result, procedureNode);

				for(AstNode resultCol: child) {
					if(resultCol.hasMixin(TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN)) {
						createColumn(resultCol,result);
					}
				}
			} else if(is(child, TeiidDdlLexicon.CreateProcedure.RESULT_DATA_TYPE)) {
				ProcedureResultSet result = getFactory().createProcedureResultSet();
				procedure.setResultSet(result);
				initialize(result, procedureNode);
				createColumn(child,result);
			} else if(is(child, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
				procOptionNodes.add(child);
			}
		}

		// process the Procedure Options
		processOptions(procOptionNodes,procedure);

		return procedure;
	}

	@Override
	protected Parameter createProcedureParameter(AstNode node, Procedure procedure) throws Exception {
		Parameter prm = super.createProcedureParameter(node, procedure);

		// Handle Teiid-specific properties and options
		Object prop = node.getProperty(TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE);
		if(prop != null) {
			String direction = prop.toString();
			prm.setDirection(direction);
		}

		// Find all the Option properties
		List<AstNode> optionNodes = new ArrayList<AstNode>();
		List<AstNode> children = node.getChildren();
		for(AstNode child: children) {
			if(is(child, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
				optionNodes.add(child);
			}
		}

		processOptions(optionNodes,prm);

		return prm;
	}
	
	/**
	 * Perform the import
	 * @param rootNode the rootNode of the DDL
	 * @param importOptions the import options for this import
	 * @param importMessages the import messages for this import
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
			} else if (is(node, TeiidDdlLexicon.OptionNamespace.STATEMENT)) { 
				// No Objects created for Teiid Option namespace
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
	@Override
	protected Map<AstNode,RelationalObject> createObject(AstNode node, Model model, Schema schema) throws Exception {
		Map<AstNode,RelationalObject> deferredMap = new HashMap<AstNode,RelationalObject>();

		// -----------------------------------------------------------------------
		// Handle Creation of Teiid Entities
		// -----------------------------------------------------------------------
		if (is(node, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT)
				|| is(node, TeiidDdlLexicon.CreateTable.VIEW_STATEMENT)) {

			Table baseTable = getFactory().createTable();
			initializeTable(baseTable, node, model);

			List<AstNode> optionNodes = new ArrayList<AstNode>();

			for (AstNode child : node) {
				// Table Elements
				if (is(child, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT)) {
					createColumn(child, baseTable);
					// Statement Options
				} else if (is(child, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
					optionNodes.add(child);
					// Contraints
				} else if (is(child, TeiidDdlLexicon.Constraint.TABLE_ELEMENT)
						|| is(child, TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT)
						|| is(child, TeiidDdlLexicon.Constraint.INDEX_CONSTRAINT)) {
					deferredMap.put(child, baseTable);
				}
			}
			// processes all options for this table
			if(!optionNodes.isEmpty()) {
				processOptions(optionNodes,baseTable);
			}

		} else if (is(node, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT)
				|| is(node, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT)) {
			createProcedure(node, model);

			// Handle Alter Table
		} else if (is(node, TeiidDdlLexicon.AlterOptions.TABLE_STATEMENT)) {
			deferredMap.put(node, null);
		} else if (is(node, TeiidDdlLexicon.AlterOptions.VIEW_STATEMENT)
				|| is(node, TeiidDdlLexicon.AlterOptions.PROCEDURE_STATEMENT)) {
		} else {
			// -----------------------------------------------------------------------
			// All other Non-Teiid DDL 
			// -----------------------------------------------------------------------
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
			if (is(node, TeiidDdlLexicon.Constraint.TABLE_ELEMENT)) {
				Table table = (Table)deferredNodes.get(node);
				createConstraint(node, table, model, allRefs);
			} 
		}

		// Second pass create FKs, options, others
		for(AstNode node:astNodes) {
			if (is(node, TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT)
					|| is(node, TeiidDdlLexicon.Constraint.INDEX_CONSTRAINT)) {
				Table table = (Table)deferredNodes.get(node);
				createConstraint(node, table, model, allRefs);
			} else if (is(node, TeiidDdlLexicon.AlterOptions.TABLE_STATEMENT)) {
				//FIXME: find different way
				Table table = find(Table.class, node, null, allRefs);
				List<AstNode> optionNodes = new ArrayList<AstNode>();
				if (table != null) {
					for (AstNode child : node) {
						if (is(child, TeiidDdlLexicon.AlterOptions.OPTIONS_LIST)) {
							List<AstNode> nodeList = child.getChildren();
							for (AstNode listItem : nodeList) {
								if (listItem.hasMixin(StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
									optionNodes.add(listItem);
								}
							}
						}
					}
				}
				// processes all options for this table
				if(!optionNodes.isEmpty()) {
					processOptions(optionNodes,table);
				}
				// Handle Alter View and Procedure
				// TODO: could potentially be combined with alter table block above
			}
		}
	}

	/**
	 * Process the Statement Option AstNodes for the supplied relational entity.
	 * @param optionNodes the list of AstNodes
	 * @param RelationalObject the RelationalObject
	 */
	private void processOptions(List<AstNode> optionNodes, RelationalObject relationalReference) {
		// process the standard teiid options.  Recognized Options are removed from the list as they are processed.
		processTeiidStandardOptions(optionNodes,relationalReference);

		// Add the remaining Options as extension properties.
		processTeiidExtensionOptions(optionNodes,relationalReference);
	}

	/**
	 * Process the options that are specific to the provided entity type
	 * @param optionNodes the list of AstNode
	 * @param relationalReference the RelationalObject
	 */
	private void processTeiidStandardOptions(List<AstNode> optionNodes, RelationalObject relationalReference) {
		// process Options common to all Entities
		processTeiidCommonOptions(optionNodes,relationalReference);

		// process Options specific to entity type
		if(relationalReference instanceof Table) {
			processTeiidTableOptions(optionNodes,(Table)relationalReference);
		} else if(relationalReference instanceof Column) {
			processTeiidColumnOptions(optionNodes,(Column)relationalReference);
		} else if(relationalReference instanceof Procedure) {
			processTeiidProcedureOptions(optionNodes,(Procedure)relationalReference);
		} 
	}

	/**
	 * Handles statementOption common to all relational entities for Teiid DDL
	 * @param optionNodes the list of statementOption AstNodes
	 * @param entity the RelationalEntity
	 */
	private void processTeiidCommonOptions(List<AstNode> optionNodes, RelationalObject entity) {
		Iterator<AstNode> nodeIter = optionNodes.iterator();
		while(nodeIter.hasNext()) {
			AstNode optionNode = nodeIter.next();
			String optionName = optionNode.getName();
			Object optionValue = optionNode.getProperty(StandardDdlLexicon.VALUE);
			if(!StringUtil.isEmpty(optionName)) {
				String optionValueStr = (String)optionValue;
				if(!StringUtil.isEmpty(optionValueStr)) {
					if(optionName.equalsIgnoreCase(TeiidDDLConstants.ANNOTATION)) {
						entity.setDescription(optionValueStr);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.UUID)) {
						// entity.setUUID();
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.NAMEINSOURCE)) {
						entity.setNameInSource(optionValueStr);
						nodeIter.remove();
					} 
				}
			}
		}
		return;
	}

	/**
	 * Handle the OPTION keys that may be set on Tables for Teiid DDL
	 * @param optionNodes 
	 * @param table 
	 */
	private void processTeiidTableOptions(List<AstNode> optionNodes, Table table) {
		Iterator<AstNode> nodeIter = optionNodes.iterator();
		while(nodeIter.hasNext()) {
			AstNode optionNode = nodeIter.next();
			String optionName = optionNode.getName();
			Object optionValue = optionNode.getProperty(StandardDdlLexicon.VALUE);
			if(!StringUtil.isEmpty(optionName)) {
				String optionValueStr = (String)optionValue;
				if(!StringUtil.isEmpty(optionValueStr)) {
					if(optionName.equalsIgnoreCase(TeiidDDLConstants.CARDINALITY)) {
						table.setCardinality(Integer.parseInt(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.MATERIALIZED)) {
						table.setMaterialized(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.MATERIALIZED_TABLE)) {
						//Table mattable = new Table();
						//mattable.setName(value);
						//table.setMaterializedTable(mattable);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.UPDATABLE)) {
						table.setSupportsUpdate(isTrue(optionValueStr));
						nodeIter.remove();
					}
				}
			}
		}
	}

	/**
	 * Handle the OPTION keys that may be set on Procedures for Teiid DDL
	 * @param optionNodes the list of optionNodes for a Procedure
	 * @param procedure the procedure
	 */
	private void processTeiidProcedureOptions(List<AstNode> optionNodes, Procedure procedure) {
		Iterator<AstNode> nodeIter = optionNodes.iterator();
		while(nodeIter.hasNext()) {
			AstNode optionNode = nodeIter.next();
			String optionName = optionNode.getName();
			Object optionValue = optionNode.getProperty(StandardDdlLexicon.VALUE);
			if(!StringUtil.isEmpty(optionName)) {
				String optionValueStr = (String)optionValue;
				// If any function properties are present, the setFuntion boolean is also set
				if(!StringUtil.isEmpty(optionValueStr)) {
					if(optionName.equalsIgnoreCase(TeiidDDLConstants.UPDATECOUNT)) {
						procedure.setUpdateCount(optionValueStr);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.CATEGORY)) {
						procedure.setFunctionCategory(optionValueStr);
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.AGGREGATE_PROP)) {
						procedure.setAggregate(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.ALLOWS_DISTINCT_PROP)) {
						procedure.setAllowsDistinct(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.ALLOWS_ORDER_BY_PROP)) {
						procedure.setAllowsOrderBy(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.ANALYTIC_PROP)) {
						procedure.setAnalytic(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.DECOMPOSABLE_PROP)) {
						procedure.setDecomposable(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.NON_PREPARED_PROP)) {
						procedure.setNonPrepared(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.NULL_ON_NULL_PROP)) {
						procedure.setReturnsNullOnNull(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.USES_DISTINCT_ROWS_PROP)) {
						procedure.setUseDistinctRows(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.VARARGS_PROP)) {
						procedure.setVariableArguments(isTrue(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.DETERMINISM_PROP)) {
						procedure.setDeterministic(isDeterministic(optionValueStr));
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.NATIVE_QUERY_PROP)) {
						procedure.setNativeQuery(optionValueStr);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.FUNCTION_CATEGORY_PROP)) {
						procedure.setFunctionCategory(optionValueStr);
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.JAVA_CLASS)) {
						procedure.setJavaClassName(optionValueStr);
						procedure.setFunction(true);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.JAVA_METHOD)) {
						procedure.setJavaMethodName(optionValueStr);
						procedure.setFunction(true);
						nodeIter.remove();
					}
				}
			}
		}
	}

	/**
	 * Handle the OPTION keys that may be set on Columns for Teiid DDL
	 * @param optionNodes 
	 * @param column 
	 */
	private void processTeiidColumnOptions(List<AstNode> optionNodes, Column column) {
		// Need to pre-process a couple things
		
//		if( column.getNullValueCount() == 0 ) {
//			// DDL Parser uses a default value of 0 so we need to check and convert to Teiid's expected null value of -1
//			column.setNullValueCount(DEFAULT_NULL_VALUE_COUNT);
//		}
		
		Iterator<AstNode> nodeIter = optionNodes.iterator();
		while(nodeIter.hasNext()) {
			AstNode optionNode = nodeIter.next();
			String optionName = optionNode.getName();
			Object optionValue = optionNode.getProperty(StandardDdlLexicon.VALUE);
			if(!StringUtil.isEmpty(optionName)) {
				String optionValueStr = (String)optionValue;
				if(!StringUtil.isEmpty(optionValueStr)) {
					if(optionName.equalsIgnoreCase(TeiidDDLConstants.SELECTABLE)) {
						column.setSelectable(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.UPDATABLE)) {
						column.setUpdateable(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.CURRENCY)) {
						column.setCurrency(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.CASE_SENSITIVE)) {
						column.setCaseSensitive(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.SIGNED)) {
						column.setSigned(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.FIXED_LENGTH)) {
						column.setLengthFixed(isTrue(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.SEARCHABLE)) {
						column.setSearchability(optionValueStr);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.MIN_VALUE)) {
						column.setMinimumValue(optionValueStr);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.MAX_VALUE)) {
						column.setMaximumValue(optionValueStr);
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.NATIVE_TYPE)) {
						column.setNativeType(optionValueStr);
						nodeIter.remove();
						resolveColumnDatatype(column, optionValueStr);
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.NULL_VALUE_COUNT)) {
						column.setNullValueCount(Integer.parseInt(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.RADIX)) {
						column.setRadix(Integer.parseInt(optionValueStr));
						nodeIter.remove();
					} else if(optionName.equalsIgnoreCase(TeiidDDLConstants.CHAR_OCTET_LENGTH)) {
						column.setCharacterOctetLength(Integer.parseInt(optionValueStr));
						nodeIter.remove();
					}
				}
			}
		}
	}
	
	private void resolveColumnDatatype(Column column, String nativeType) {
		if( column.getDatatypeName().equalsIgnoreCase(DataTypes.INTEGER) && nativeType.equalsIgnoreCase(DataTypes.INT) ) {
			column.setDatatypeName(nativeType);
		} else if( column.getDatatypeName().equalsIgnoreCase(DataTypes.STRING) && nativeType.equalsIgnoreCase(DataTypes.CHAR ) ) {
			column.setDatatypeName(nativeType);
		} else if( column.getDatatypeName().equalsIgnoreCase(DataTypes.VARBINARY) && nativeType.equalsIgnoreCase(DataTypes.BINARY ) ) {
			column.setDatatypeName(DataTypes.OBJECT.toLowerCase());
		} else if( column.getDatatypeName().equalsIgnoreCase(DataTypes.TIMESTAMP) && nativeType.equalsIgnoreCase(DataTypes.DATETIME ) ) {
		} else if( column.getDatatypeName().equalsIgnoreCase(DataTypes.DOUBLE) && nativeType.equalsIgnoreCase(DataTypes.FLOAT ) ) {
			column.setDatatypeName(nativeType);
			if( column.getPrecision() == 0 ) {
				column.setPrecision(53);
			}
		} else if( column.getDatatypeName().equalsIgnoreCase(DataTypes.BIGDECIMAL) && nativeType.equalsIgnoreCase(DataTypes.DECIMAL ) ) {
			column.setDatatypeName(nativeType);
		} else if( column.getDatatypeName().equalsIgnoreCase(DataTypes.SHORT) && nativeType.equalsIgnoreCase(DataTypes.TINYINT ) ) {
			column.setDatatypeName(DataTypes.BYTE.toLowerCase());
		} else if( column.getDatatypeName().equalsIgnoreCase(DataTypes.FLOAT) && nativeType.equalsIgnoreCase(DataTypes.REAL ) ) {
			if( column.getPrecision() == 0 ) {
				column.setPrecision(24);
			}
		}
		
		
		// Not enough info in DDL to determine if fixed length data type so calling it here
		column.setLengthFixed(isFixedLength(column.getDatatypeName()));
		
		// From ModelProcessor....
        if (column.getSearchability().equalsIgnoreCase(RelationalConstants.SEARCHABILITY.ALL_EXCEPT_LIKE) ) {
            column.setCaseSensitive(false);
        } else if (column.getSearchability().equalsIgnoreCase(RelationalConstants.SEARCHABILITY.SEARCHABLE) ) {
            column.setCaseSensitive(true);
        } else {
            column.setCaseSensitive(false);
        }
	}
	
    /**
     * Method that can identify if a data type is fixed length or not
     * (See <code>org.teiid.designer.jdbc.relational.impl.RelationalModelProcessorImpl</code> used for JDBC import)
     * @param typeName 
     * @return True if the specified type should be considered fixed-length.
     * @since 4.2
     */
    protected boolean isFixedLength( final String typeName ) {
        return !(DataTypes.LONGVARBINARY.equalsIgnoreCase(typeName) || 
        		DataTypes.LONGVARCHAR.equalsIgnoreCase(typeName) || 
        		DataTypes.VARBINARY.equalsIgnoreCase(typeName) || 
        		DataTypes.VARCHAR.equalsIgnoreCase(typeName) || 
        		DataTypes.ARRAY.equalsIgnoreCase(typeName) || 
        		DataTypes.BLOB.equalsIgnoreCase(typeName) || 
        		DataTypes.CLOB.equalsIgnoreCase(typeName));
    }

	/**
	 * Get the deterministic boolean from the DETERMINISM option string
	 * @param determinismStr
	 * @return 'true' if deterministic, 'false' if not.
	 */
	private boolean isDeterministic(String determinismStr) {
		if(TeiidDDLConstants.DETERMINISM_OPT_NONDETERMINISTIC.equalsIgnoreCase(determinismStr)) {
			return false;
		} else if(TeiidDDLConstants.DETERMINISM_OPT_COMMAND_DETERMINISTIC.equalsIgnoreCase(determinismStr)) {
			return false;
		} else if(TeiidDDLConstants.DETERMINISM_OPT_SESSION_DETERMINISTIC.equalsIgnoreCase(determinismStr)) {
			return false;
		} else if(TeiidDDLConstants.DETERMINISM_OPT_USER_DETERMINISTIC.equalsIgnoreCase(determinismStr)) {
			return false;
		} else if(TeiidDDLConstants.DETERMINISM_OPT_VDB_DETERMINISTIC.equalsIgnoreCase(determinismStr)) {
			return false;
		} else if(TeiidDDLConstants.DETERMINISM_OPT_DETERMINISTIC.equalsIgnoreCase(determinismStr)) {
			return true;
		}
		return false;
	}
	
	/**
	 * Save the Extension Option name-value info to the importerModel
	 * @param optionNodes the list of statement option AstNodes
	 * @param relationalEntity the relational entity
	 */
	private void processTeiidExtensionOptions(List<AstNode> optionNodes, RelationalObject relationalEntity) {
		Iterator<AstNode> nodeIter = optionNodes.iterator();
		while(nodeIter.hasNext()) {
			AstNode optionNode = nodeIter.next();

			String optionName = optionNode.getName();
			Object optionValue = optionNode.getProperty(StandardDdlLexicon.VALUE);
			if(!StringUtil.isEmpty(optionName)) {
				// Translate incoming Teiid-namespaced ExtProps into the equivalent Designer MED NS
				if(isUriNamespaced(optionName) || isPrefixNamespaced(optionName)) {
					optionName = translateNamespacedOptionName(optionName);
				}
				String optionValueStr = (String)optionValue;
				if(!StringUtil.isEmpty(optionValueStr)) {
					relationalEntity.addExtensionProperty(optionName, optionValueStr);
				}
			}
			nodeIter.remove();
		}
	}
	
    /**
	 * Translate a namespaced extension property name, translating teiid namespaces to designer MED namespaces
	 * @param namespacedPropName the extension property name, including namespace
	 * @return the equivalent designer-namespaced PropName.
	 */
	private String translateNamespacedOptionName(String namespacedPropName) {
		// ===============================================================================================================
		// Handling for propNames which are namespaced with URI, eg http://www.teiid.org/translator/excel/2014}CELL_NUMBER
		// ===============================================================================================================
		if(isUriNamespaced(namespacedPropName)) {
			// Get the Namespace URI
			String propNsUri = getExtensionPropertyNsUri(namespacedPropName);
			
			// Translate the uri to corresponding designer med prefix
			String designerNs = translateTeiidNsUriToDesignerNSPrefix(propNsUri);
			
			// Get name portion of incoming name
			String propName = getExtensionPropertyName(namespacedPropName);
			
			// return reassembled namespaced name
			return designerNs+':'+propName;
		// =====================================================================================
		// Handling for propNames which are namespaced with a prefix, eg teiid_excel:CELL_NUMBER
		// =====================================================================================
		} else if(isPrefixNamespaced(namespacedPropName)) {
			// Get the Namespace prefix
			String propNsPrefix = getExtensionPropertyNsPrefix(namespacedPropName);
			
			String designerNs = translateTeiidNSPrefixToDesignerNSPrefix(propNsPrefix);
			
			// Get name portion of incoming name
			String propName = getExtensionPropertyName(namespacedPropName);
			
			// return reassembled namespaced name
			return designerNs+':'+propName;
		}
		return namespacedPropName;
	}

    /**
	 * Get the Namespace prefix from the extension property name.  The propertyName may or may not be namespaced.
	 * If it's not a null is returned
	 * @param propName the extension property name, including namespace
	 * @return the namespace, if present.  'null' if not namespaced
	 */
	private String getExtensionPropertyNsPrefix(String propName) {
		String namespace = null;
		if(!StringUtil.isEmpty(propName) && isPrefixNamespaced(propName)) {
			int index = propName.indexOf(':');
			if(index!=-1) {
				namespace = propName.substring(0,index);
			}
		}
		return namespace;
	}
	
    /**
	 * Get the Name from the extension property name.  If its not namespaced, just return the name.  Otherwise strip off the namespace
	 * @param propName the extension property name, with or without namespace
	 * @return the name without namespace, if present.
	 */
	private String getExtensionPropertyNsUri(String propName) {
		String name = null;
		if(propName!=null) {
			propName.trim();
			if(isUriNamespaced(propName)) {
				int index1 = propName.indexOf('{');
				int index2 = propName.indexOf('}');
				name = propName.substring(index1+1, index2);
			}
		}
		return name;
	}
	
    /**
	 * Get the Name from the extension property name.  If its not namespaced, just return the name.  Otherwise strip off the namespace
	 * @param namespacedPropName the extension property name, with or without namespace
	 * @return the name without namespace, if present.
	 */
	private String getExtensionPropertyName(String namespacedPropName) {
		String name = namespacedPropName;
		
		if(isPrefixNamespaced(namespacedPropName)) {
			int index = namespacedPropName.indexOf(':');
			name = namespacedPropName.substring(index+1);
		} else if(isUriNamespaced(namespacedPropName)) {
			int index = namespacedPropName.indexOf('}');
			name = namespacedPropName.substring(index+1);
		}
		
		return name;
	}
	
    /**
	 * Determine if the property name has a leading namespace prefix
	 * @param propName the extension property name, including namespace
	 * @return 'true' if a namespace is present, 'false' if not.
	 */
	private boolean isPrefixNamespaced(String propName) {
		boolean isPrefixNamespaced = false;
		if(!StringUtil.isEmpty(propName) && !hasOpenCloseBraces(propName) && propName.indexOf(':') != -1) {
			isPrefixNamespaced = true;
		}
		return isPrefixNamespaced;
	}
	
    /**
	 * Determine if the property name has a leading namespace uri
	 * @param propName the extension property name, including namespace uri
	 * @return 'true' if a namespace uri is present, 'false' if not.
	 */
	private boolean isUriNamespaced(String propName) {
		boolean isUriNamespaced = false;
		if(!StringUtil.isEmpty(propName) && hasOpenCloseBraces(propName)) {
			isUriNamespaced = true;
		}
		return isUriNamespaced;
	}
	
	/**
	 * Determine if the supplied property name has open and closed braces
	 * @param propName the extension property name
	 * @return 'true' if both open and closed braces are found
	 */
	private boolean hasOpenCloseBraces(String propName) {
		boolean hasBoth = false;
		if( !StringUtil.isEmpty(propName) && propName.indexOf('{')!=-1 && propName.indexOf('}')!=-1 ) {
			hasBoth = true;
		}
		return hasBoth;
	}
	
	/**
	 * Translate a Teiid ExtensionProperty namespace into the Designer equivalent.
	 * @param teiidNamespace
	 * @return the designer MED namespace equivalent
	 */
	private String translateTeiidNSPrefixToDesignerNSPrefix(String teiidNamespace) {
		String designerNS = teiidNamespace;
		if(NS_TEIID_ODATA.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_ODATA; 
		} else if(NS_TEIID_RELATIONAL.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_RELATIONAL;
		} else if(NS_TEIID_WEBSERVICE.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_WEBSERVICE;
		} else if(NS_TEIID_SALESFORCE.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_SALESFORCE;
		} else if(NS_TEIID_MONGO.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_MONGO;
		} else if(NS_TEIID_ACCUMULO.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_ACCUMULO;
		} else if(NS_TEIID_EXCEL.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_EXCEL;
		} else if(NS_TEIID_JPA.equals(teiidNamespace)) {
			designerNS = NS_DESIGNER_JPA;
		} 
		return designerNS;
	}  
	
	/**
	 * Translate a Teiid ExtensionProperty namespace into the Designer equivalent.
	 * @param teiidNsUri the teiid namespace uri
	 * @return the designer MED namespace equivalent
	 */
	private String translateTeiidNsUriToDesignerNSPrefix(String teiidNsUri) {
		String designerNsPrefix = null;
		
//		ModelExtensionRegistry registry = ExtensionPlugin.getInstance().getRegistry();
//		
//		Collection<ModelExtensionDefinition> meds = registry.getAllDefinitions();
//		for(ModelExtensionDefinition med : meds) {
//			String designerMedNsUri = med.getNamespaceUri();
//			String designerMedNsPrefix = med.getNamespacePrefix();
//			
//			if(!StringUtil.isEmpty(designerMedNsUri) && designerMedNsUri.equals(teiidNsUri)) {
//				designerNsPrefix = designerMedNsPrefix;
//				break;
//			}
//		}
		
		return designerNsPrefix;
	}        
	
}
