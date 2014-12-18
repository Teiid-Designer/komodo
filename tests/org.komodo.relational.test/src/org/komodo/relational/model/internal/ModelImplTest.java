/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ModelImplTest extends RelationalModelTest {

    private static final String NAME = "model";

    private Model modelObject;

    @Before
    public void init() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction("init", false, null);
        final KomodoObject kobject = _repo.add(transaction, null, NAME, null);
        transaction.commit();

        this.modelObject = new ModelImpl(_repo, kobject.getAbsolutePath());
    }

    @Test
    public void shouldAddFunction() throws Exception {
        final String name = "function";
        final Procedure function = this.modelObject.addFunction(null, name);
        assertThat(function, is(notNullValue()));
        assertThat(function.getName(null), is(name));
        assertThat(function.isFunction(null), is(true));
    }

    @Test
    public void shouldAddProcedure() throws Exception {
        final String name = "procedure";
        final Procedure procedure = this.modelObject.addProcedure(null, name);
        assertThat(procedure, is(notNullValue()));
        assertThat(procedure.getName(null), is(name));
        assertThat(procedure.isFunction(null), is(false));
    }

    @Test
    public void shouldAddTable() throws Exception {
        final String name = "table";
        final Table table = this.modelObject.addTable(null, name);
        assertThat(table, is(notNullValue()));
        assertThat(table.getName(null), is(name));
    }

    @Test
    public void shouldAddView() throws Exception {
        final String name = "view";
        final View view = this.modelObject.addView(null, name);
        assertThat(view, is(notNullValue()));
        assertThat(view.getName(null), is(name));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyFunctionName() throws Exception {
        this.modelObject.addFunction(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyProcedureName() throws Exception {
        this.modelObject.addProcedure(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyTableName() throws Exception {
        this.modelObject.addTable(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyViewName() throws Exception {
        this.modelObject.addView(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullFunctionName() throws Exception {
        this.modelObject.addFunction(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullProcedureName() throws Exception {
        this.modelObject.addProcedure(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullTableName() throws Exception {
        this.modelObject.addTable(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullViewName() throws Exception {
        this.modelObject.addView(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyFunctionName() throws Exception {
        this.modelObject.removeFunction(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyProcedureName() throws Exception {
        this.modelObject.removeProcedure(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyTableName() throws Exception {
        this.modelObject.removeTable(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyViewName() throws Exception {
        this.modelObject.removeView(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullFunctionName() throws Exception {
        this.modelObject.removeFunction(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullProcedureName() throws Exception {
        this.modelObject.removeProcedure(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullTableName() throws Exception {
        this.modelObject.removeTable(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullViewName() throws Exception {
        this.modelObject.removeView(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownFunction() throws Exception {
        this.modelObject.removeFunction(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownProcedure() throws Exception {
        this.modelObject.removeProcedure(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownTable() throws Exception {
        this.modelObject.removeTable(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownView() throws Exception {
        this.modelObject.removeView(null, "unknown");
    }

    @Test
    public void shouldGetFunctions() throws Exception {
        final int numFunctions = 5;

        for (int i = 0; i < numFunctions; ++i) {
            this.modelObject.addFunction(null, "function" + i);
        }

        assertThat(this.modelObject.getFunctions(null).length, is(numFunctions));
    }

    @Test
    public void shouldGetProcedures() throws Exception {
        final int numProcedures = 5;

        for (int i = 0; i < numProcedures; ++i) {
            this.modelObject.addProcedure(null, "procedure" + i);
        }

        assertThat(this.modelObject.getProcedures(null).length, is(numProcedures));
    }

    @Test
    public void shouldGetTables() throws Exception {
        final int numTables = 5;

        for (int i = 0; i < numTables; ++i) {
            this.modelObject.addTable(null, "table" + i);
        }

        assertThat(this.modelObject.getTables(null).length, is(numTables));
    }

    @Test
    public void shouldGetViews() throws Exception {
        final int numViews = 5;

        for (int i = 0; i < numViews; ++i) {
            this.modelObject.addView(null, "view" + i);
        }

        assertThat(this.modelObject.getViews(null).length, is(numViews));
    }

    @Test
    public void shouldRemoveFunction() throws Exception {
        final String name = "function";
        this.modelObject.addFunction(null, name);
        this.modelObject.removeFunction(null, name);
        assertThat(this.modelObject.getFunctions(null).length, is(0));
    }

    @Test
    public void shouldRemoveProcedure() throws Exception {
        final String name = "procedure";
        this.modelObject.addProcedure(null, name);
        this.modelObject.removeProcedure(null, name);
        assertThat(this.modelObject.getProcedures(null).length, is(0));
    }

    @Test
    public void shouldRemoveTable() throws Exception {
        final String name = "table";
        this.modelObject.addTable(null, name);
        this.modelObject.removeTable(null, name);
        assertThat(this.modelObject.getTables(null).length, is(0));
    }

    @Test
    public void shouldRemoveView() throws Exception {
        final String name = "view";
        this.modelObject.addView(null, name);
        this.modelObject.removeView(null, name);
        assertThat(this.modelObject.getViews(null).length, is(0));
    }

}
