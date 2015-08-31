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
    private static final LinkType LINK_TYPE = LinkType.SELF;
    private static final LinkType[] LINK_TYPES = new LinkType[] { LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST };
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080/v1" ).build();
    private static final String VDB_NAME = "MyVdb";

    private RestVdbDescriptor descriptor;

    @Before
    public void init() {
        this.descriptor = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        this.descriptor.setDescription( DESCRIPTION );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbDescriptor thatDescriptor = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPES );
        thatDescriptor.setDescription( this.descriptor.getDescription() );
        thatDescriptor.setLinks( this.descriptor.getLinks() );
        thatDescriptor.setProperties( this.descriptor.getProperties() );
        assertThat( this.descriptor, is( thatDescriptor ) );
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
        final RestVdbDescriptor thatDescriptor = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPES );
        thatDescriptor.setDescription( this.descriptor.getDescription() );
        assertThat( this.descriptor.hashCode(), is( thatDescriptor.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptionIsDifferent() {
        final RestVdbDescriptor thatDescriptor = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPES );
        thatDescriptor.setDescription( this.descriptor.getDescription() + "blah" );
        thatDescriptor.setLinks( this.descriptor.getLinks() );
        thatDescriptor.setProperties( this.descriptor.getProperties() );

        assertThat( this.descriptor.getDescription(), is( not( thatDescriptor.getDescription() ) ) );
        assertThat( this.descriptor, is( not( thatDescriptor ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenLinksAreDifferent() {
        final RestVdbDescriptor thatDescriptor = new RestVdbDescriptor( this.descriptor.getName(), URI, LINK_TYPE );
        thatDescriptor.setDescription( this.descriptor.getDescription() );
        thatDescriptor.setProperties( this.descriptor.getProperties() );

        assertThat( Arrays.deepEquals( this.descriptor.getLinks(), thatDescriptor.getLinks() ), is( false ) );
        assertThat( this.descriptor, is( not( thatDescriptor ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenVdbNameIsDifferent() {
        final RestVdbDescriptor thatDescriptor = new RestVdbDescriptor( this.descriptor.getName() + "blah", URI, LINK_TYPES );
        thatDescriptor.setDescription( this.descriptor.getDescription() );
        thatDescriptor.setLinks( this.descriptor.getLinks() );
        thatDescriptor.setProperties( this.descriptor.getProperties() );

        assertThat( this.descriptor.getName(), is( not( thatDescriptor.getName() ) ) );
        assertThat( this.descriptor, is( not( thatDescriptor ) ) );
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

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.descriptor.setDescription( newDescription );
        assertThat( this.descriptor.getDescription(), is( newDescription ) );
    }

}
