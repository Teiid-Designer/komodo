package org.komodo.shell;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.shell.commands.core.UnsetPropertyCommand;
import org.komodo.spi.repository.KomodoObject;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class UnsetCommandTest extends AbstractCommandTest {

    @Test
    public void testUnsetMultiValuedProperty() throws Exception {
        setup( "unsetCommand2.txt", UnsetPropertyCommand.class );
        execute();
        assertThat( this.wsStatus.getCurrentContext().getFullName(), is( "/workspace/MyVdb/MyDataRole" ) );

        final KomodoObject kobject = this.wsStatus.getCurrentContext().getKomodoObj();
        assertThat( kobject.getProperty( this.wsStatus.getTransaction(), VdbLexicon.DataRole.MAPPED_ROLE_NAMES ),
                    is( nullValue() ) );
    }

    @Test
    public void testUnsetSingleValueProperty() throws Exception {
        setup( "unsetCommand1.txt", UnsetPropertyCommand.class );
        execute();
        assertThat( this.wsStatus.getCurrentContext().getFullName(), is( "/workspace/MyModel" ) );

        final KomodoObject kobject = this.wsStatus.getCurrentContext().getKomodoObj();
        assertThat( kobject.getProperty( this.wsStatus.getTransaction(), VdbLexicon.Vdb.DESCRIPTION ), is( nullValue() ) );
    }

}
