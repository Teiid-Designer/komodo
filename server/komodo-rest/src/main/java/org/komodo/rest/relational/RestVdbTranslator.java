/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A translator that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyTranslator",
 *     "description" : "translator description goes here",
 *     "type" : "customType",
 *     "properties" : [
 *         "green" : "lantern",
 *         "captain" : "america",
 *         "black" : "widow"
 *     ]
 * }
 * </code>
 * </pre>
 */
public final class RestVdbTranslator extends KomodoRestEntity {

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.encode(VdbLexicon.Translator.DESCRIPTION);

    /**
     * Label used to describe type
     */
    public static final String TYPE_LABEL = KomodoService.encode(VdbLexicon.Translator.TYPE);

    /**
     * An empty array of translators.
     */
    public static final RestVdbTranslator[] NO_TRANSLATORS = new RestVdbTranslator[ 0 ];

    private String description;
    private String type;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbTranslator() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb
     * @param hasChildren true if vdb has children
     * @param parentVdb the name of the parent vdb
     */
    public RestVdbTranslator(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren, String parentVdb) {
        super(baseUri, id, dataPath, kType, hasChildren);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().buildVdbTranslatorUri(LinkType.SELF, parentVdb, id)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().buildVdbTranslatorUri(LinkType.PARENT, parentVdb, id)));
    }

    /**
     * @return the description (can be empty)
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        this.description = newDescription;
    }

    /**
     * @return the translator type (can be empty)
     */
    public String getType() {
        return this.type;
    }

    /**
     * @param newType
     *        the new translator type (can be empty)
     */
    public void setType( final String newType ) {
        this.type = newType;
    }

    /**
     * @param translator source translator
     * @param parentVdb parent Vdb
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbTranslator} based on the source {@link Translator}
     * @throws KException if error occurs
     */
    public static RestVdbTranslator build(final Translator translator, final Vdb parentVdb, final URI baseUri, final UnitOfWork uow) throws KException {

        final String vdbName = parentVdb.getName(uow);
        final RestVdbTranslator entity = new RestVdbTranslator(baseUri,
                                                               translator.getName(uow), translator.getAbsolutePath(),
                                                               translator.getTypeIdentifier(uow), translator.hasChildren(uow), vdbName);

        entity.setDescription(translator.getDescription(uow));
        entity.setType(translator.getType(uow));

        entity.addExecutionProperties(uow, translator);

        return entity;
    }


    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.description == null) ? 0 : this.description.hashCode());
        result = prime * result + ((this.type == null) ? 0 : this.type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RestVdbTranslator other = (RestVdbTranslator)obj;
        if (this.description == null) {
            if (other.description != null)
                return false;
        } else
            if (!this.description.equals(other.description))
                return false;
        if (this.type == null) {
            if (other.type != null)
                return false;
        } else
            if (!this.type.equals(other.type))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdbTranslator [description=" + this.description + ", type=" + this.type + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }

}
