/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ModelImplTest extends RelationalModelTest {

    private static final String NAME = "model";

    private Model model;

    @Before
    public void init() throws Exception {
        final WorkspaceManager wsMgr = WorkspaceManager.getInstance(_repo);
        this.model = wsMgr.createModel(null, null, NAME);
    }

    @Test
    public void shouldAddFunction() throws Exception {
        final String name = "function";
        final Function function = this.model.addFunction(null, name);
        assertThat(function, is(notNullValue()));
        assertThat(function.getName(null), is(name));
    }

    @Test
    public void shouldAddProcedure() throws Exception {
        final String name = "procedure";
        final Procedure procedure = this.model.addProcedure(null, name);
        assertThat(procedure, is(notNullValue()));
        assertThat(procedure.getName(null), is(name));
    }

    @Test
    public void shouldAddTable() throws Exception {
        final String name = "table";
        final Table table = this.model.addTable(null, name);
        assertThat(table, is(notNullValue()));
        assertThat(table.getName(null), is(name));
    }

    @Test
    public void shouldAddView() throws Exception {
        final String name = "view";
        final View view = this.model.addView(null, name);
        assertThat(view, is(notNullValue()));
        assertThat(view.getName(null), is(name));
    }

    @Test
    public void shouldAllowEmptyDescriptionWhenRemoving() throws Exception {
        this.model.setDescription(null, "blah");
        this.model.setDescription(null, StringConstants.EMPTY_STRING);
        assertThat(this.model.getDescription(null), is(nullValue()));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyFunctionName() throws Exception {
        this.model.addFunction(null, "");
    }

    @Test
    public void shouldAllowNullDescriptionWhenRemoving() throws Exception {
        this.model.setDescription(null, "blah");
        this.model.setDescription(null, null);
        assertThat(this.model.getDescription(null), is(nullValue()));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyProcedureName() throws Exception {
        this.model.addProcedure(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyTableName() throws Exception {
        this.model.addTable(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyViewName() throws Exception {
        this.model.addView(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullFunctionName() throws Exception {
        this.model.addFunction(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullProcedureName() throws Exception {
        this.model.addProcedure(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullTableName() throws Exception {
        this.model.addTable(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullViewName() throws Exception {
        this.model.addView(null, null);
    }

    @Test
    public void shouldFailConstructionIfNotModel() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ModelImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyFunctionName() throws Exception {
        this.model.removeFunction(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyProcedureName() throws Exception {
        this.model.removeProcedure(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyTableName() throws Exception {
        this.model.removeTable(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyViewName() throws Exception {
        this.model.removeView(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullFunctionName() throws Exception {
        this.model.removeFunction(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullProcedureName() throws Exception {
        this.model.removeProcedure(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullTableName() throws Exception {
        this.model.removeTable(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullViewName() throws Exception {
        this.model.removeView(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownFunction() throws Exception {
        this.model.removeFunction(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownProcedure() throws Exception {
        this.model.removeProcedure(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownTable() throws Exception {
        this.model.removeTable(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownView() throws Exception {
        this.model.removeView(null, "unknown");
    }

    @Test
    public void shouldGetFunctions() throws Exception {
        final int numFunctions = 5;

        for (int i = 0; i < numFunctions; ++i) {
            this.model.addFunction(null, "function" + i);
        }

        assertThat(this.model.getFunctions(null).length, is(numFunctions));
    }

    @Test
    public void shouldGetProcedures() throws Exception {
        final int numProcedures = 5;

        for (int i = 0; i < numProcedures; ++i) {
            this.model.addProcedure(null, "procedure" + i);
        }

        assertThat(this.model.getProcedures(null).length, is(numProcedures));
    }

    @Test
    public void shouldGetTables() throws Exception {
        final int numTables = 5;

        for (int i = 0; i < numTables; ++i) {
            this.model.addTable(null, "table" + i);
        }

        assertThat(this.model.getTables(null).length, is(numTables));
    }

    @Test
    public void shouldGetViews() throws Exception {
        final int numViews = 5;

        for (int i = 0; i < numViews; ++i) {
            this.model.addView(null, "view" + i);
        }

        assertThat(this.model.getViews(null).length, is(numViews));
    }

    @Test
    public void shouldHaveStrongTypeChildren() throws Exception {
        this.model.addFunction(null, "function");
        this.model.addProcedure(null, "procedure");
        this.model.addTable(null, "table");
        this.model.addView(null, "view");

        assertThat(this.model.getChildren(null).length, is(4));
        assertThat(this.model.getChildren(null)[0], is(instanceOf(Function.class)));
        assertThat(this.model.getChildren(null)[1], is(instanceOf(Procedure.class)));
        assertThat(this.model.getChildren(null)[2], is(instanceOf(Table.class)));
        assertThat(this.model.getChildren(null)[3], is(instanceOf(View.class)));
    }

    @Test
    public void shouldRemoveFunction() throws Exception {
        final String name = "function";
        this.model.addFunction(null, name);
        this.model.removeFunction(null, name);
        assertThat(this.model.getFunctions(null).length, is(0));
    }

    @Test
    public void shouldRemoveProcedure() throws Exception {
        final String name = "procedure";
        this.model.addProcedure(null, name);
        this.model.removeProcedure(null, name);
        assertThat(this.model.getProcedures(null).length, is(0));
    }

    @Test
    public void shouldRemoveTable() throws Exception {
        final String name = "table";
        this.model.addTable(null, name);
        this.model.removeTable(null, name);
        assertThat(this.model.getTables(null).length, is(0));
    }

    @Test
    public void shouldRemoveView() throws Exception {
        final String name = "view";
        this.model.addView(null, name);
        this.model.removeView(null, name);
        assertThat(this.model.getViews(null).length, is(0));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.model.setDescription(null, value);
        assertThat(this.model.getDescription(null), is(value));
    }

}
