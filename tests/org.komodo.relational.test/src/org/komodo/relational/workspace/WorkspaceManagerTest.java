/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.workspace;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class WorkspaceManagerTest extends RelationalModelTest {

    private static WorkspaceManager _mgr;

    @BeforeClass
    public static void obtainWorkspaceManager() {
        _mgr = WorkspaceManager.getInstance(_repo);
    }

    private Model createModel( final UnitOfWork uow,
                               final KomodoObject parent,
                               final String modelName ) throws Exception {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = _repo.createTransaction(this.name.getMethodName(), false, null);
        }

        final Model model = _mgr.createModel(transaction, parent, modelName);

        if (uow == null) {
            transaction.commit();
        }

        assertThat(model.getPrimaryType(null).getName(), is(KomodoLexicon.VdbModel.NODE_TYPE));
        assertThat(model.getName(null), is(modelName));

        return model;
    }

    private Vdb createVdb( final UnitOfWork uow,
                           final KomodoObject parent,
                           final String vdbName ) throws Exception {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = _repo.createTransaction(this.name.getMethodName(), false, null);
        }

        final String externalFilePath = "extPath";
        final Vdb vdb = _mgr.createVdb(transaction, parent, vdbName, externalFilePath);

        if (uow == null) {
            transaction.commit();
        }

        assertThat(vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(vdb.getName(null), is(vdbName));
        assertThat(vdb.getOriginalFilePath(null), is(externalFilePath));

        return vdb;
    }

    @Test
    public void shouldCreateVdb() throws Exception {
        final KomodoObject parent = _repo.add(null, _repo.komodoWorkspace(null).getAbsolutePath(), "parent", null);
        final Vdb vdb = createVdb(null, parent, this.name.getMethodName());
        assertThat(vdb, is(notNullValue()));
        assertThat(_repo.getFromWorkspace(null, vdb.getAbsolutePath()), is((KomodoObject)vdb));
    }

    @Test
    public void shouldCreateVdbWithNullParent() throws Exception {
        final Vdb vdb = createVdb(null, null, this.name.getMethodName());
        assertThat(vdb, is(notNullValue()));
        assertThat(vdb.getParent(null), is(_repo.komodoWorkspace(null)));
    }

    @Test
    public void shouldDeleteModel() throws Exception {
        final Vdb parent = createVdb(null, null, this.name.getMethodName() + "Vdb");
        final Model model = createModel(null, parent, this.name.getMethodName() + "Model");
        _mgr.delete(null, model);
        assertThat(_mgr.findModels(null).length, is(0));
    }

    @Test
    public void shouldDeleteVdb() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        _mgr.delete(null, vdb);
        assertThat(_mgr.findVdbs(null).length, is(0));
    }

    @Test
    public void shouldFindModels() throws Exception {
        final Vdb parent = createVdb(null, null, "vdb");
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            createModel(null, parent, (prefix + ++suffix));
        }

        // create under a folder
        final KomodoObject folder = _repo.add(null, parent.getAbsolutePath(), "folder", null);

        for (int i = 0; i < 7; ++i) {
            createModel(null, folder, (prefix + ++suffix));
        }

        assertThat(_mgr.findModels(null).length, is(suffix));
    }

    @Test
    public void shouldFindVdbs() throws Exception {
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            createVdb(null, null, (prefix + ++suffix));
        }

        // create under a folder
        final KomodoObject parent = _repo.add(null, null, "blah", null);

        for (int i = 0; i < 7; ++i) {
            createVdb(null, parent, (prefix + ++suffix));
        }

        assertThat(_mgr.findVdbs(null).length, is(suffix));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyExternalFilePath() throws Exception {
        _mgr.createVdb(null, null, "vdbName", StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyVdbName() throws Exception {
        _mgr.createVdb(null, null, StringConstants.EMPTY_STRING, "externalFilePath");
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullExternalFilePath() throws Exception {
        _mgr.createVdb(null, null, "vdbName", null);
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullVdbName() throws Exception {
        _mgr.createVdb(null, null, null, "externalFilePath");
    }

    @Test
    public void shouldNotFindModelsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(_mgr.findModels(null).length, is(0));
    }

    @Test
    public void shouldNotFindSchemasWhenWorkspaceIsEmpty() throws Exception {
        assertThat(_mgr.findSchemas(null).length, is(0));
    }

    @Test
    public void shouldNotFindTeiidsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(_mgr.findTeiids(null).isEmpty(), is(true));
    }

    @Test
    public void shouldNotFindVdbsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(_mgr.findVdbs(null).length, is(0));
    }

}
