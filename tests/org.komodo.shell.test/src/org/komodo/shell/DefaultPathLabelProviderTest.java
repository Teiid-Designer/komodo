/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.repository.RepositoryImpl;
import org.komodo.shell.api.PathLabelProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository;
import org.mockito.Mockito;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DefaultPathLabelProviderTest {

    private PathLabelProvider labelProvider;
    private WorkspaceStatus status;
    private Repository.UnitOfWork uow;

    @Before
    public void initializeState() {
        this.uow = Mockito.mock( Repository.UnitOfWork.class );
        final Repository repository = Mockito.mock( Repository.class );

        this.labelProvider = new DefaultPathLabelProvider();
        this.labelProvider.setRepository( repository );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailDisplayNameWhenPathIsEmpty() {
        this.labelProvider.getDisplayName( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailDisplayNameWhenPathIsNull() {
        this.labelProvider.getDisplayName( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailDisplayTextWhenPathIsEmpty() {
        this.labelProvider.getDisplayText( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailDisplayTextWhenPathIsNull() {
        this.labelProvider.getDisplayText( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenDisplayTextIsEmpty() {
        this.labelProvider.getPath( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenDisplayTextIsNull() {
        this.labelProvider.getPath( null );
    }

    @Test
    public void shouldHaveId() {
        assertThat( this.labelProvider.getId(), is( notNullValue() ) );
    }

    @Test
    public void shouldNotProvideDisplayTextWhenPathDoesNotStartWithKomodoRoot() {
        assertThat( this.labelProvider.getDisplayText( "blah" ), is( nullValue() ) );
    }

    @Test
    public void shouldNotProvidePathWhenDisplayTextDoesNotStartWithKomodoRootDisplayPath() {
        assertThat( this.labelProvider.getPath( "blah" ), is( nullValue() ) );
    }

    @Test
    public void shouldProvideDisplayNameWhenNameHasNoPrefix() {
        final String actual = "/tko:komodo/tko:workspace/abc";
        final String expected = "abc";

        assertThat( this.labelProvider.getDisplayName( actual ), is( expected ) );
    }

    @Test
    public void shouldProvideDisplayNameWithoutPrefix() {
        final String actual = "/tko:komodo/tko:workspace/abc:def";
        final String expected = "def";

        assertThat( this.labelProvider.getDisplayName( actual ), is( expected ) );
    }

    @Test
    public void shouldProvideDisplayTextForMultipleSegmentsAfterRoot() {
        final String actual = "/tko:komodo/tko:workspace/abc:foo/def:bar/";
        final String expected = "/workspace/foo/bar/";
        assertThat( this.labelProvider.getDisplayText( actual ), is( expected ) );
    }

    @Test
    public void shouldProvideDisplayTextForOneSegmentAfterRoot() {
        assertThat( this.labelProvider.getDisplayText( RepositoryImpl.WORKSPACE_ROOT ), is( "/workspace" ) );
    }

    @Test
    public void shouldProvideDisplayTextWithEndingSlash() {
        final String actual = "/tko:komodo/tko:workspace/foo/bar/";
        final String expected = "/workspace/foo/bar/";
        assertThat( this.labelProvider.getDisplayText( actual ), is( expected ) );
    }

    @Test
    public void shouldProvideDisplayTextWithoutEndingSlash() {
        final String actual = "/tko:komodo/tko:workspace/foo/blah:bar";
        final String expected = "/workspace/foo/bar";
        assertThat( this.labelProvider.getDisplayText( actual ), is( expected ) );
    }

    @Test
    public void shouldProvideRootDisplayText() {
        assertThat( this.labelProvider.getDisplayText( DefaultPathLabelProvider.ROOT_PATH ),
                    is( DefaultPathLabelProvider.ROOT_DISPLAY_PATH ) );
    }

    @Test
    public void shouldProvideRootPath() {
        assertThat( this.labelProvider.getPath( DefaultPathLabelProvider.ROOT_DISPLAY_PATH ),
                    is( DefaultPathLabelProvider.ROOT_PATH ) );
    }

}
