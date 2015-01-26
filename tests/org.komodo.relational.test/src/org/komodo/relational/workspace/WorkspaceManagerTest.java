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
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class WorkspaceManagerTest extends RelationalModelTest {

    private static int _counter = 1;
    private static WorkspaceManager _mgr;
    private static int _numVdbs = 0;

    @BeforeClass
    public static void obtainWorkspaceManager() {
        _mgr = WorkspaceManager.getInstance(_repo);
    }

    private Vdb createVdb( final UnitOfWork uow,
                           final KomodoObject parent ) throws Exception {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = _repo.createTransaction(this.name.getMethodName(), false, null);
        }

        final int suffix = _counter++;
        final String vdbName = ("vdb" + suffix);
        final String externalFilePath = ("extPath" + suffix);
        final Vdb vdb = _mgr.createVdb(transaction, parent, vdbName, externalFilePath);
        ++_numVdbs;

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
        final Vdb vdb = createVdb(null, parent);
        assertThat(vdb, is(notNullValue()));
        assertThat(_repo.getFromWorkspace(null, vdb.getAbsolutePath()), is((KomodoObject)vdb));
    }

    @Test
    public void shouldCreateVdbWithNullParent() throws Exception {
        final Vdb vdb = createVdb(null, null);
        assertThat(vdb, is(notNullValue()));
        assertThat(vdb.getParent(null), is(_repo.komodoWorkspace(null)));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyExternalFilePath() throws Exception {
        _mgr.createVdb(null, null, "vdbName", null);
        assertThat(_mgr.findVdbs(null).length, is(_numVdbs));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyVdbName() throws Exception {
        _mgr.createVdb(null, null, StringConstants.EMPTY_STRING, "externalFilePath");
        assertThat(_mgr.findVdbs(null).length, is(_numVdbs));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullExternalFilePath() throws Exception {
        _mgr.createVdb(null, null, "vdbName", null);
        assertThat(_mgr.findVdbs(null).length, is(_numVdbs));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullVdbName() throws Exception {
        _mgr.createVdb(null, null, null, "externalFilePath");
        assertThat(_mgr.findVdbs(null).length, is(_numVdbs));
    }

}
