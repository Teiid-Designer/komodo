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
package org.komodo.relational.validation;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public class RelationalValidationTest extends AbstractLocalRepositoryTest {

    protected static final String VDB_PATH = "/vdb/path/vdb.vdb";
    protected static final String ENTRY_PATH = "/vdb/entryPath";

    protected List<String> getRuleNames( final Rule[] rules ) throws Exception {
        List<String> ruleNames = new ArrayList<String>();
        for(Rule rule : rules) {
            ruleNames.add( rule.getName( getTransaction() ) );
        }
        return ruleNames;
    }

    protected Vdb createVdb( final String vdbName ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb vdb = mgr.createVdb( getTransaction(), null, vdbName, VDB_PATH );

        assertThat( vdb.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.VIRTUAL_DATABASE ) );
        assertThat( vdb.getName( getTransaction() ), is( vdbName ) );
        assertThat( vdb.getOriginalFilePath( getTransaction() ), is( VDB_PATH ) );
        return vdb;
    }

    protected Model addModel( final Vdb vdb,
                              final String modelName ) throws Exception {
        final Model model = vdb.addModel( getTransaction(), modelName );

        assertThat( model.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.DECLARATIVE_MODEL ) );
        assertThat( model.getName( getTransaction() ), is( modelName ) );
        return model;
    }

    protected DataRole addDataRole( final Vdb vdb,
                                    final String dataRoleName ) throws Exception {
        final DataRole dataRole = vdb.addDataRole( getTransaction(), dataRoleName );

        assertThat( dataRole.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.DATA_ROLE ) );
        assertThat( dataRole.getName( getTransaction() ), is( dataRoleName ) );
        return dataRole;
    }

    protected Entry addEntry( final Vdb vdb,
                              final String entryName ) throws Exception {
        final Entry entry = vdb.addEntry( getTransaction(), entryName, ENTRY_PATH );

        assertThat( entry.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Entry.ENTRY ) );
        assertThat( entry.getName( getTransaction() ), is( entryName ) );
        return entry;
    }

    protected Translator addTranslator( final Vdb vdb,
                                        final String translatorName,
                                        final String translatorType ) throws Exception {
        final Translator translator = vdb.addTranslator( getTransaction(), translatorName, translatorType );

        assertThat( translator.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Translator.TRANSLATOR ) );
        assertThat( translator.getName( getTransaction() ), is( translatorName ) );
        return translator;
    }

    protected VdbImport addVdbImport( final Vdb vdb,
                                      final String importName ) throws Exception {
        final VdbImport vdbImport = vdb.addImport( getTransaction(), importName );

        assertThat( vdbImport.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.ImportVdb.IMPORT_VDB ) );
        assertThat( vdbImport.getName( getTransaction() ), is( importName ) );
        return vdbImport;
    }

    protected Table addTable( final Model model,
                              final String tableName ) throws Exception {

        final Table table = model.addTable( getTransaction(), tableName );

        assertThat( table.getName( getTransaction() ), is( tableName ) );
        return table;
    }

    protected ModelSource addSource( final Model model,
                                     final String sourceName ) throws Exception {
        final ModelSource modelSource = model.addSource( getTransaction(), sourceName );

        assertThat( modelSource.getName( getTransaction() ), is( sourceName ) );
        return modelSource;
    }

    protected PushdownFunction addPushdownFunction( final Model model,
                                                    final String funcName ) throws Exception {
        final PushdownFunction pushdownFunction = model.addPushdownFunction( getTransaction(), funcName );

        assertThat( pushdownFunction.getName( getTransaction() ), is( funcName ) );
        return pushdownFunction;
    }

    protected UserDefinedFunction addUserDefinedFunction( final Model model,
                                                          final String funcName ) throws Exception {
        final UserDefinedFunction userDefinedFunction = model.addUserDefinedFunction( getTransaction(), funcName );

        assertThat( userDefinedFunction.getName( getTransaction() ), is( funcName ) );
        return userDefinedFunction;
    }

    protected VirtualProcedure addVirtualProcedure( final Model model,
                                                    final String procName ) throws Exception {
        final VirtualProcedure virtualProcedure = model.addVirtualProcedure( getTransaction(), procName );

        assertThat( virtualProcedure.getName( getTransaction() ), is( procName ) );
        return virtualProcedure;
    }

    protected StoredProcedure addStoredProcedure( final Model model,
                                                  final String procName ) throws Exception {
        final StoredProcedure storedProcedure = model.addStoredProcedure( getTransaction(), procName );

        assertThat( storedProcedure.getName( getTransaction() ), is( procName ) );
        return storedProcedure;
    }

    protected View addView( final Model model,
                            final String viewName ) throws Exception {
        final View view = model.addView( getTransaction(), viewName );

        assertThat( view.getName( getTransaction() ), is( viewName ) );
        return view;
    }

    protected Permission addPermission( final DataRole dataRole,
                                        final String permissionName ) throws Exception {
        final Permission permission = dataRole.addPermission( getTransaction(), permissionName );

        assertThat( permission.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.Permission.PERMISSION ) );
        assertThat( permission.getName( getTransaction() ), is( permissionName ) );
        return permission;
    }

    protected Mask addMask( final Permission permission,
                            final String maskName ) throws Exception {
        final Mask mask = permission.addMask( getTransaction(), maskName );

        assertThat( mask.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.Permission.Mask.MASK ) );
        assertThat( mask.getName( getTransaction() ), is( maskName ) );
        return mask;
    }

    protected Condition addCondition( final Permission permission,
                                      final String conditionName ) throws Exception {
        final Condition condition = permission.addCondition( getTransaction(), conditionName );

        assertThat( condition.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.Permission.Condition.CONDITION ) );
        assertThat( condition.getName( getTransaction() ), is( conditionName ) );
        return condition;
    }

    protected AccessPattern addAccessPattern( final Table table,
                                              final String apName ) throws Exception {
        final AccessPattern ap = table.addAccessPattern( getTransaction(), apName );

        assertThat( ap.getName( getTransaction() ), is( apName ) );
        return ap;
    }

    protected Index addIndex( final Table table,
                              final String indexName ) throws Exception {
        final Index index = table.addIndex(getTransaction(), indexName );

        assertThat( index.getName( getTransaction() ), is( indexName ) );
        return index;
    }

    protected Column addColumn( final Table table,
                                final String columnName ) throws Exception {
        final Column column = table.addColumn( getTransaction(), columnName );

        assertThat( column.getName( getTransaction() ), is( columnName ) );
        return column;
    }

    protected Column addColumn( final View view,
                                final String columnName ) throws Exception {
        final Column column = view.addColumn( getTransaction(), columnName );

        assertThat( column.getName( getTransaction() ), is( columnName ) );
        return column;
    }

    protected PrimaryKey addPrimaryKey( final Table table,
                                        final String pkName ) throws Exception {
        final PrimaryKey pk = table.setPrimaryKey( getTransaction(), pkName );

        assertThat( pk.getName( getTransaction() ), is( pkName ) );
        return pk;
    }

    protected ForeignKey addForeignKey( final Table table,
                                        final String fkName,
                                        final Table referencedTable ) throws Exception {
        final ForeignKey fk = table.addForeignKey( getTransaction(), fkName, referencedTable );

        assertThat( fk.getName( getTransaction() ), is( fkName ) );
        return fk;
    }

    protected UniqueConstraint addUniqueConstraint( final Table table,
                                                    final String ucName ) throws Exception {
        final UniqueConstraint uc = table.addUniqueConstraint( getTransaction(), ucName );

        assertThat( uc.getName( getTransaction() ), is( ucName ) );
        return uc;
    }

    protected Parameter addParameter( final Procedure proc,
                                      final String paramName ) throws Exception {
        final Parameter param = proc.addParameter( getTransaction(), paramName );

        assertThat( param.getName( getTransaction() ), is( paramName ) );
        return param;
    }

    protected Parameter addParameter( final Function func,
                                      final String paramName ) throws Exception {
        final Parameter param = func.addParameter( getTransaction(), paramName );

        assertThat( param.getName( getTransaction() ), is( paramName ) );
        return param;
    }

    protected DataTypeResultSet addDataTypeResultSet( final StoredProcedure proc ) throws Exception {

        final DataTypeResultSet resultSet = proc.setResultSet( getTransaction(), DataTypeResultSet.class );

        assertThat( resultSet.getName(getTransaction()), is( "resultSet" ) );
        return resultSet;
    }

    protected TabularResultSet addTabularResultSet( final StoredProcedure proc ) throws Exception {

        final TabularResultSet resultSet = proc.setResultSet( getTransaction(), TabularResultSet.class );

        assertThat( resultSet.getName(getTransaction()), is( "resultSet" ) );
        return resultSet;
    }

    protected ResultSetColumn addResultSetColumn( final TabularResultSet rs,
                                                  final String rsColName ) throws Exception {

        final ResultSetColumn col = rs.addColumn( getTransaction(), rsColName );

        assertThat( col.getName( getTransaction() ), is( rsColName ) );
        return col;
    }

}
