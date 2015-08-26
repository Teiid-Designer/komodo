/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertThat;
import static org.komodo.rest.json.RestLink.LinkType.NO_LINK_TYPES;
import java.net.URI;
import java.util.Arrays;
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.spi.constants.StringConstants;
import com.google.gson.Gson;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbDescriptorTest {

    private static final Gson BUILDER = new Gson();
    private static final String DESCRIPTION = "my description";
    private static final String JSON = "{\"id\":\"MyVdb\",\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/komodo/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/komodo/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/komodo/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
    private static final LinkType LINK_TYPE = LinkType.SELF;
    private static final LinkType[] LINK_TYPES = new LinkType[] { LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST };
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();
    private static final String VDB_NAME = "MyVdb";

    @Test
    public void shouldBeEqual() {
        final RestVdbDescriptor descriptor1 = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( descriptor1.getName(), URI, LINK_TYPES );
        descriptor2.setDescription( descriptor1.getDescription() );
        assertThat( descriptor1, is( descriptor2 ) );
    }

    @Test
    public void shouldExportJson() {
        final RestVdbDescriptor descriptor = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        descriptor.setDescription( DESCRIPTION );
        assertThat( BUILDER.toJson( descriptor ), is( JSON ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenBaseUriIsNull() {
        new RestVdbDescriptor( "blah", null, LINK_TYPE );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypesAreEmpty() {
        new RestVdbDescriptor( "blah", URI, NO_LINK_TYPES );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypesAreNull() {
        new RestVdbDescriptor( "blah", URI );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenVdbNameIsEmpty() {
        new RestVdbDescriptor( StringConstants.EMPTY_STRING, URI, LINK_TYPE );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenVdbNameIsNull() {
        new RestVdbDescriptor( null, URI, LINK_TYPE );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbDescriptor descriptor1 = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( descriptor1.getName(), URI, LINK_TYPES );
        descriptor2.setDescription( descriptor1.getDescription() );
        assertThat( descriptor1.hashCode(), is( descriptor2.hashCode() ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDescriptor descriptor = BUILDER.fromJson( JSON, RestVdbDescriptor.class );
        assertThat( descriptor.getName(), is( VDB_NAME ) );
        assertThat( descriptor.getDescription(), is( DESCRIPTION ) );
        assertThat( descriptor.getLinks().length, is( LINK_TYPES.length ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptionIsDifferent() {
        final RestVdbDescriptor descriptor1 = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( descriptor1.getName(), URI, LINK_TYPES );
        descriptor2.setDescription( descriptor1.getDescription() + "blah" );
        assertThat( descriptor1.getDescription(), is( not( descriptor2.getDescription() ) ) );
        assertThat( descriptor1, is( not( descriptor2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenLinksAreDifferent() {
        final RestVdbDescriptor descriptor1 = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( descriptor1.getName(), URI, LINK_TYPE );
        descriptor2.setDescription( descriptor1.getDescription() );
        assertThat( Arrays.deepEquals( descriptor1.getLinks(), descriptor2.getLinks() ), is( false ) );
        assertThat( descriptor1, is( not( descriptor2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenVdbNameIsDifferent() {
        final RestVdbDescriptor descriptor1 = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( descriptor1.getName() + "blah", URI, LINK_TYPES );
        descriptor2.setDescription( descriptor1.getDescription() );
        assertThat( descriptor1.getName(), is( not( descriptor2.getName() ) ) );
        assertThat( descriptor1, is( not( descriptor2 ) ) );
    }

    @Test
    public void shouldNotCreateDuplicateLinks() {
        final RestVdbDescriptor descriptor = new RestVdbDescriptor( VDB_NAME,
                                                                    URI,
                                                                    LinkType.MANIFEST,
                                                                    LinkType.MANIFEST,
                                                                    LinkType.SELF,
                                                                    LinkType.SELF,
                                                                    LinkType.PARENT,
                                                                    LinkType.DELETE );
        assertThat( descriptor.getLinks().length, is( 4 ) );
    }

}
