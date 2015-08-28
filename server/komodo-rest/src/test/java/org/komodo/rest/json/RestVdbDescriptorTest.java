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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.komodo.rest.json.JsonConstants.JSON_BUILDER;
import static org.komodo.rest.json.RestLink.LinkType.NO_LINK_TYPES;
import java.net.URI;
import java.util.Arrays;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.spi.constants.StringConstants;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbDescriptorTest {

    private static final String DESCRIPTION = "my description";
    private static final String JSON = "{\"id\":\"MyVdb\",\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
    private static final LinkType LINK_TYPE = LinkType.SELF;
    private static final LinkType[] LINK_TYPES = new LinkType[] { LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST };
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();
    private static final String VDB_NAME = "MyVdb";

    private RestVdbDescriptor descriptor;

    @Before
    public void init() {
        this.descriptor = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        this.descriptor.setDescription( DESCRIPTION );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPES );
        descriptor2.setDescription( this.descriptor.getDescription() );
        assertThat( this.descriptor, is( descriptor2 ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyDescriptors() {
        final RestVdbDescriptor empty1 = new RestVdbDescriptor();
        final RestVdbDescriptor empty2 = new RestVdbDescriptor();
        assertThat( empty1, is( empty2 ) );
    }

    public void shouldConstructEmptyDescriptor() {
        final RestVdbDescriptor empty = new RestVdbDescriptor();
        assertThat( empty.getDescription(), is( nullValue() ) );
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().length, is( 0 ) );
    }

    @Test
    public void shouldExportJson() {
        assertThat( JSON_BUILDER.toJson( this.descriptor ), is( JSON ) );
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
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPES );
        descriptor2.setDescription( this.descriptor.getDescription() );
        assertThat( this.descriptor.hashCode(), is( descriptor2.hashCode() ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDescriptor descriptor = JSON_BUILDER.fromJson( JSON, RestVdbDescriptor.class );
        assertThat( descriptor.getName(), is( VDB_NAME ) );
        assertThat( descriptor.getDescription(), is( DESCRIPTION ) );
        assertThat( descriptor.getLinks().length, is( LINK_TYPES.length ) );
        assertThat( descriptor.getProperties().isEmpty(), is( true ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptionIsDifferent() {
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPES );
        descriptor2.setDescription( this.descriptor.getDescription() + "blah" );
        assertThat( this.descriptor.getDescription(), is( not( descriptor2.getDescription() ) ) );
        assertThat( this.descriptor, is( not( descriptor2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenLinksAreDifferent() {
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPE );
        descriptor2.setDescription( this.descriptor.getDescription() );
        assertThat( Arrays.deepEquals( this.descriptor.getLinks(), descriptor2.getLinks() ), is( false ) );
        assertThat( this.descriptor, is( not( descriptor2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenVdbNameIsDifferent() {
        final RestVdbDescriptor descriptor2 = new RestVdbDescriptor( this.descriptor.getName() + "blah", URI, LINK_TYPES );
        descriptor2.setDescription( this.descriptor.getDescription() );
        assertThat( this.descriptor.getName(), is( not( descriptor2.getName() ) ) );
        assertThat( this.descriptor, is( not( descriptor2 ) ) );
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

    @Test( expected = Exception.class )
    public void shouldNotExportJsonWhenThereIsNoName() {
        final RestVdbDescriptor descriptor = new RestVdbDescriptor();
        JSON_BUILDER.toJson( descriptor );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
        JSON_BUILDER.fromJson( malformed, RestVdbDescriptor.class );
    }

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.descriptor.setDescription( newDescription );
        assertThat( this.descriptor.getDescription(), is( newDescription ) );
    }

}
